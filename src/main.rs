#![feature(debug_closure_helpers)]
#![allow(clippy::uninlined_format_args)]

use clap::Parser as _;
use eyre::Context as _;
use log::*;

use diag::Context;
use parser::Parser;
use source::Source;
use std::fs;
use std::path::PathBuf;
use util::CodePrinter;

mod codegen;
mod diag;
mod instruction;
mod lexer;
mod parser;
mod reader;
mod source;
mod util;

#[derive(clap::Parser)]
#[command(about = "WaveVM Assembly Compiler", long_about = None)]
struct Cli {
    /// Input file path
    #[arg()]
    input: PathBuf,
    /// Output as Wave2 binary format
    #[arg(short, long, default_value_t = false)]
    binary: bool,
    /// Output file path, only logs to stdout if not set
    #[arg(short, long)]
    output: Option<PathBuf>,
    /// Log level, valid values are: OFF, ERROR, WARN, INFO, DEBUG, TRACE
    #[arg(short, long, default_value_t = LevelFilter::Warn)]
    log_level: LevelFilter,
}

fn main() -> eyre::Result<()> {
    color_eyre::install()?;
    let cli = Cli::parse();
    simple_logger::SimpleLogger::new()
        .with_level(cli.log_level)
        .init()?;

    if !cli.input.try_exists()? {
        error!("File \"{}\" does not exist", cli.input.display());
        return Ok(());
    }

    let source = Source::new_from_file(cli.input)?;
    let mut ctx = Context::new(source);

    // TODO: i dont like having to do this, but otherwise it requires self references
    // maybe the source shouldn't be in ctx?
    let src_str = ctx.source().src().to_owned();

    let mut mem_lines = Vec::new();
    let mut code_lines = Vec::new();

    let mut in_code = true;

    for line in src_str.lines() {
        match line {
            ".memory" => {
                in_code = false;
                continue;
            }
            ".code" => {
                in_code = true;
                continue;
            }
            _ => {
                if in_code {
                    code_lines.push(line.to_string());
                } else {
                    mem_lines.push(line.to_string());
                }
            }
        }
    }

    let code_str = code_lines.join("\n");
    let mem_str = mem_lines.join("\n");

    let mut parser = match Parser::new(code_str.as_str()) {
        Ok(parser) => parser,
        Err(e) => {
            ctx.add_diag(e);
            ctx.emit_errs();
            return Err(eyre::eyre!("Failed to begin parsing."));
        }
    };

    let mut insts = Vec::new();
    loop {
        let inst = parser.parse_inst(&mut ctx);
        match inst {
            Ok(inst) => {
                if inst.len() == 0{ break; }
                for inst in inst {
                    insts.push(inst);
                }
            },
            Err(_) => {
                println!("Error parsing {:#?}", inst);
                break;
            }
        }
    }

    if ctx.had_errs() {
        ctx.emit_errs();
        error!("failed due to previous errors");
    } else {
        debug!("{:#?}", insts);
        let code = match codegen::gen(insts.as_slice()) {
            Ok(code) => code,
            Err(e) => {
                ctx.add_diag(e);
                ctx.emit_errs();
                return Err(eyre::eyre!("Failed to generate code."));
            }
        };
        let printer = CodePrinter(code.as_slice());
        info!("{:x}", printer);
        if let Some(output) = cli.output {
            if cli.binary {
                if mem_lines.len() > 0 {
                    let mem = parse_mem(mem_str);
                    let mut header = b"MWvm\x01\0\0".to_vec();
                    let memlen = mem.len().min(60) as u8;
                    header[5] = header.len() as u8;
                    header[6] = memlen + header[5];
                    let code = code
                        .iter()
                        .flat_map(|&x| x.to_be_bytes());
                    let buffer = header
                        .into_iter()
                        .chain(mem)
                        .chain(code)
                        .collect::<Vec<u8>>();
                    fs::write(&output, buffer).context("failed to write output file")?;
                } else {
                    let mut header = b"MWvm\x01\0\0".to_vec();
                    header[5] = header.len() as u8;
                    header[6] = header.len() as u8;
                    let codebytes = code
                        .iter()
                        .flat_map(|&x| x.to_be_bytes())
                        .collect::<Vec<u8>>();
                    let buffer = header
                        .into_iter()
                        .chain(codebytes)
                        .collect::<Vec<u8>>();
                    fs::write(&output, buffer).context("failed to write output file")?;
                }
                info!("Wrote compiled binary to \"{}\"", output.display())
            } else {
                fs::write(&output, format!("{:x}", printer)).context("failed to write output file")?;
                info!("Wrote compiled hex to \"{}\"", output.display())
            }
        }
    }

    Ok(())
}

fn parse_mem(mem: String) -> Vec<u8> {
    // Collect all the non-whitespace characters.
    let lines = mem.lines().map(|l| l.trim().split(";").next().unwrap());
    let chars = lines
        .flat_map(|l| l.chars())
        .filter(|c| !c.is_whitespace())
        .filter(|c| c.is_ascii_hexdigit());
    // Convert the characters to u8
    let mut out: Vec<u8> = Vec::new();
    let mut buffer: u8 = 0;
    let mut k = 0;
    for c in chars {
        buffer <<= 4;
        buffer |= c.to_digit(16).unwrap() as u8;
        k ^= 0x1;
        if k == 0 {
            out.push(buffer);
        }
    }
    out
}
