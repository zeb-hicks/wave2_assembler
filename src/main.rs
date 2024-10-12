use std::fs;

use log::*;

use parser::Parser;
use util::ArrayPrinter;

mod codegen;
mod instruction;
mod lexer;
mod parser;
mod reader;
mod util;

fn main() -> eyre::Result<()> {
    color_eyre::install()?;
    simple_logger::SimpleLogger::new().init()?;

    let source = fs::read_to_string("in.txt")?;

    let mut insts = Vec::new();
    for line in source.lines() {
        let mut parser = Parser::new(line);
        loop {
            let Some(inst) = parser.parse_inst()? else {
                break;
            };
            insts.push(inst);
        }
    }

    debug!("{:#?}", insts);

    let code = codegen::gen(insts.as_slice());
    info!("{:X}", ArrayPrinter(code.as_slice()));

    Ok(())
}
