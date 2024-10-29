use core::fmt;
use std::mem;

use crate::{lexer::Span, source::Source};

pub struct Context {
    source: Source,
    diags: Vec<Diagnostic>,
    /// whether this has ever had any errors
    had_errs: bool,
}

impl Context {
    pub fn new(source: Source) -> Self {
        Self {
            source,
            diags: Vec::new(),
            had_errs: false,
        }
    }

    pub fn source(&self) -> &Source {
        &self.source
    }

    pub fn add_diag(&mut self, diag: Diagnostic) {
        self.diags.push(diag);
        self.had_errs = true;
    }

    pub fn had_errs(&mut self) -> bool {
        self.had_errs
    }

    pub fn emit_errs(&mut self) {
        // replace the current diagnostics with an empty list and then process them
        // this is done to be able to pass ctx to format
        let diags = mem::take(&mut self.diags);

        for d in diags {
            println!("ERROR: {}", d.format(self));
        }
    }

    /// gets the 1-indexed line and column of the start of the given span
    fn line_info(&self, span: Span) -> (u32, u32) {
        // get the current line (0-indexed) by finding the first line that is past
        // the span, and then going back one
        let line_idx = self
            .source
            .line_starts()
            .partition_point(|start| *start <= span.low())
            .saturating_sub(1);

        let line = line_idx as u32 + 1; // human lines are 1 indexed

        let col = span
            .low()
            .checked_sub(
                *self
                    .source
                    .line_starts()
                    .get(line_idx)
                    .expect("line should exist"),
            )
            .expect("span low should be >= line start");

        // TODO: column number
        (line, col)
    }

    fn get_line_text(&self, line_idx: u32) -> Option<&str> {
        let line_start = *self.source.line_starts().get(line_idx as usize)? as usize;
        let line_end = self
            .source
            .line_starts()
            .get((line_idx + 1) as usize)
            .map_or(self.source.src().len(), |end| *end as usize);

        Some(&self.source.src()[line_start..line_end])
    }
}

impl fmt::Debug for Context {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Context")
            .field("diags", &self.diags)
            .finish_non_exhaustive()
    }
}

#[derive(Debug)]
pub struct Diagnostic {
    msg: String,
    span: Span,
    notes: Vec<String>,
}

impl Diagnostic {
    pub fn new(msg: String, span: Span) -> Self {
        Self {
            msg,
            span,
            notes: Vec::new(),
        }
    }

    pub fn with_note(mut self, note: String) -> Self {
        self.notes.push(note);
        self
    }

    pub fn msg(&self) -> &str {
        self.msg.as_str()
    }

    pub fn span(&self) -> Span {
        self.span
    }

    fn format(&self, ctx: &Context) -> String {
        let (line, col) = ctx.line_info(self.span);
        let src_str = ctx.get_line_text(line - 1).unwrap_or("");
        format!(
            "{}\n  {}:{}:{}  {}",
            self.msg,
            ctx.source().filename(),
            line,
            col,
            src_str,
        )
    }
}
