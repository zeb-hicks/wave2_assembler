#![feature(debug_closure_helpers)]

use clap::Parser as _;
use eyre::Context as _;
use log::*;

use diag::Context;
use parser::Parser;
use source::Source;
use std::fs;
use std::path::PathBuf;
use util::ArrayPrinter;

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
    /// Output file path, only logs to stdout if not set
    #[arg(short, long)]
    output: Option<PathBuf>,
    /// Log level, valid values are: OFF, ERROR, WARN, INFO, DEBUG, TRACE
    #[arg(short, long, default_value_t = LevelFilter::Info)]
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
            Ok(Some(inst)) => insts.push(inst),
            Ok(None) => {
                break;
            }
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
        let printer = ArrayPrinter(code.as_slice());
        info!("{:X}", printer);
        if let Some(output) = cli.output {
            if output.is_dir() {
                error!("Error writing to file, \"{}\" is a directory", output.display());
            } else {
                fs::write(&output, format!("{:X}", printer))?;
                info!("Wrote compiled hex to \"{}\"", output.display())
            }
        }
    }

    Ok(())
}
