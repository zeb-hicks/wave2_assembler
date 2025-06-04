#![feature(debug_closure_helpers)]
#![allow(clippy::uninlined_format_args)]

use clap::Parser as _;
use clap_stdin::FileOrStdin;
use eyre::Context as _;
use log::*;

use diag::Context;
use parser::Parser;
use source::Source;
use std::fs;
use std::io::Read;
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
    input: FileOrStdin<String>,
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

    let filename = if cli.input.is_file() {
        cli.input.filename().to_string()
    } else {
        "stdin".to_string()
    };

    let mut reader = cli.input.into_reader()?;
    let mut src_str: String = String::new();
    reader.read_to_string(&mut src_str)?;

    let source = Source::new_from_string(&src_str, &filename);
    let mut ctx = Context::new(source);

    let mut mem_lines = Vec::new();
    let mut code_lines = Vec::new();

    let mut in_code = true;

    for line in src_str.lines() {
        if line.trim().starts_with(".memory") {
            in_code = false;
        } else if line.trim().starts_with(".code") {
            in_code = true;
        } else {
            if in_code {
                code_lines.push(line.to_string());
            } else {
                mem_lines.push(line.to_string());
            }
        }
    }

    // let code_str = code_lines.join("\n");
    let mem_str = mem_lines.join("\n");

    // println!("Memory: {:#?}", mem_lines);
    // println!("Code: {:#?}", code_lines);

    let mut parser = match Parser::new(src_str.as_str()) {
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
        let mem_bytes = parse_mem(mem_str);
        let mem = mem_bytes
            .chunks_exact(2)
            .map(|chunk| {
                let mut word: u16 = 0;
                word |= (chunk[0] as u16) << 8;
                word |= chunk[1] as u16;
                word
            })
            .collect::<Vec<u16>>();
        let buffer: Vec<u16> = if mem.len() > 0 {
            mem.iter()
                .chain(code.iter())
                .copied()
                .collect::<Vec<u16>>()
        } else {
            code.iter()
                .copied()
                .collect::<Vec<u16>>()
        };
        let code_printer = CodePrinter(buffer.as_slice());
        info!("{:x}", code_printer);
        if let Some(output) = cli.output {
            if cli.binary {
                if mem_lines.len() > 0 {
                    let mut header = b"MWvm\x01\0\0".to_vec();
                    let memlen = 2 * mem.len() as u8;
                    header[5] = header.len() as u8;
                    header[6] = memlen + header[5];

                    let code = code
                        .iter()
                        .flat_map(|&x| x.to_be_bytes())
                        .collect::<Vec<u8>>();

                    let mem = mem
                        .iter()
                        .flat_map(|&x| x.to_be_bytes())
                        .collect::<Vec<u8>>();

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

                    let code = code
                        .iter()
                        .flat_map(|&x| x.to_be_bytes())
                        .collect::<Vec<u8>>();

                    let buffer = header
                        .into_iter()
                        .chain(code)
                        .collect::<Vec<u8>>();

                    fs::write(&output, buffer).context("failed to write output file")?;
                }
                info!("Wrote compiled binary to \"{}\"", output.display())
            } else {
                fs::write(&output, format!("{:x}", code_printer)).context("failed to write output file")?;
                info!("Wrote compiled hex to \"{}\"", output.display())
            }
        } else {
            println!("{:x}", code_printer);
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
