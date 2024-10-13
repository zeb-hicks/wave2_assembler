use std::{fs, env};
use log::*;
use parser::Parser;
use util::ArrayPrinter;
use eyre::bail;

mod codegen;
mod instruction;
mod lexer;
mod parser;
mod reader;
mod util;

fn main() -> eyre::Result<()> {
    color_eyre::install()?;
    simple_logger::SimpleLogger::new().init()?;
    let filename = "in.txt";
    let source = fs::read_to_string(filename)?;

    let verbose = env::args().any(|v| v == "-v"); //TODO? CLI

    let mut insts = Vec::new();
    let mut errors = Vec::new();
    for (line_num, line) in source.lines().enumerate() {
        let mut parser = Parser::new(line);
        loop {
            match parser.parse_inst() {
                Ok(Some(inst)) => insts.push(inst),
                Ok(None) => break,
                Err(e) => {
                    errors.push( match verbose {
                        false => format!("[{filename}:{line_num}]: {e}"),
                        true => format!("[{filename}:{line_num}]: {e:?}"),
                    });
                    break;
                }
            }
        }
    }

    if !errors.is_empty() {
        bail!(errors.join("\n"));
    }

    debug!("{:#?}", insts);

    let code = codegen::gen(insts.as_slice());
    info!("{:X}", ArrayPrinter(code.as_slice()));

    Ok(())
}
