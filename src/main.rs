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
    /// Memory descriptor file path, describes the contents of 0x00->0x3f
    #[arg(short, long)]
    memory: Option<PathBuf>,
    /// Output file path, only logs to stdout if not set
    #[arg(short, long)]
    output: Option<PathBuf>,
    /// Output in binary format
    #[arg(short, long, default_value_t = false)]
    binary: bool,
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
    let mut parser = Parser::new(src_str.as_str());

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
                break;
            }
        }
    }

    if ctx.had_errs() {
        ctx.emit_errs();
        error!("failed due to previous errors");
    } else {
        debug!("{:#?}", insts);
        let code = codegen::gen(insts.as_slice());
        let printer = CodePrinter(code.as_slice());
        info!("{:x}", printer);
        if let Some(output) = cli.output {
            if cli.binary {
                if let Some(mem) = cli.memory {
                    let mem = fs::read_to_string(mem).context("failed to read memory file")?;
                    let mem = parse_mem(mem);
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
    let chars = mem.chars().filter(|c| !c.is_whitespace());
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
