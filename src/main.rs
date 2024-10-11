use std::fs;

use lexer::{Lexer, TokenKind};
use log::*;
use parser::Parser;

mod instruction;
mod lexer;
mod parser;
mod reader;

fn main() -> eyre::Result<()> {
    color_eyre::install()?;
    simple_logger::SimpleLogger::new().init()?;

    let source = fs::read_to_string("in.txt")?;
    debug!("\n{:#?}", source);

    for line in source.lines() {
        let mut parser = Parser::new(line);
        loop {
            let Some(inst) = parser.parse_inst()? else {
                break;
            };
            info!("{:?}", inst);
        }
    }

    Ok(())
}
