#![feature(debug_closure_helpers)]

use log::*;

use diag::Context;
use parser::Parser;
use source::Source;
use util::ArrayPrinter;

mod codegen;
mod diag;
mod instruction;
mod lexer;
mod parser;
mod reader;
mod source;
mod util;

fn main() -> eyre::Result<()> {
    color_eyre::install()?;
    simple_logger::SimpleLogger::new().init()?;

    let filename = "in.txt";

    let source = Source::new_from_file(filename)?;
    let mut ctx = Context::new(source);

    // TODO: i dont like having to do this, but otherwise it requires self references
    // maybe the source shouldn't be in ctx?
    let src_str = ctx.source().src().to_owned();
    let mut parser = Parser::new(src_str.as_str());

    let mut insts = Vec::new();
    loop {
        let inst = parser.parse_inst(&mut ctx);
        trace!("parsed {:?}", inst);
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
        info!("{:X}", ArrayPrinter(code.as_slice()));
    }

    Ok(())
}
