#![allow(clippy::uninlined_format_args)]
use std::fmt::Display;

use crate::{diag::Diagnostic, reader::{Reader, ReaderToken}};

/// this is a different thing from reader to be able to easily separate
/// the splitting and the getting source mechanisms
#[derive(Debug)]
pub struct Lexer<'a> {
    src: &'a str,
    pos: usize,
    reader: Reader<'a>,
    last_token: Option<ReaderToken>,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            src,
            pos: 0,
            reader: Reader::new(src),
            last_token: None,
        }
    }

    pub fn next_token(&mut self) -> Result<Token, Diagnostic> {
        use LexerToken::*;

        // loops to skip whitespace
        loop {
            let next = self.reader.next();

            let start_pos = self.pos;
            self.pos += next.len();

            let kind: Result<LexerToken, String> = match next.kind() {
                ReaderToken::Newline => Ok(Newline),
                ReaderToken::Whitespace | ReaderToken::Comment => continue,
                ReaderToken::Ident => Ok(Ident(self.src[start_pos..self.pos].to_string())),
                ReaderToken::Number => match self.src[start_pos..self.pos].parse::<u16>() {
                    Ok(num) => Ok(Number(num)),
                    Err(_) => Err(format!("Invalid 16-bit number: {}", &self.src[start_pos..self.pos])),
                },
                ReaderToken::Hex => match u16::from_str_radix(&self.src[(start_pos+1)..self.pos], 16) {
                    Ok(num) => Ok(Number(num)),
                    Err(_) => Err(format!("Invalid 16-bit hex: {}", &self.src[(start_pos+1)..self.pos])),
                },
                ReaderToken::Literal => Ok(Literal),
                ReaderToken::Raw => match u16::from_str_radix(&self.src[start_pos..self.pos].to_string(), 16) {
                    Ok(raw) => Ok(Raw(raw)),
                    Err(_) => Err(format!("Error parsing literal: {}", &self.src[start_pos..self.pos])),
                }
                ReaderToken::EoF => Ok(EoF),
                ReaderToken::Comma => Ok(Comma),
                ReaderToken::Dot => Ok(Dot),
                ReaderToken::LeftBracket => Ok(LeftBracket),
                ReaderToken::RightBracket => Ok(RightBracket),
                ReaderToken::Plus => Ok(Plus),
                ReaderToken::Label => {
                    let label = self.src[start_pos..self.pos].to_string();
                    if label.len() < 2 || !label.starts_with(':') {
                        return Err(Diagnostic::new(
                            format!("Invalid label: {}", label),
                            Span::new(start_pos as u32, self.pos as u32),
                        ));
                    }
                    match self.last_token {
                        None | Some(ReaderToken::Newline) => Ok(LabelDef(label[1..].to_string())),
                        _ => Ok(Label(label[1..].to_string())),
                    }
                }
            };
            let kind = match kind {
                Ok(kind) => kind,
                Err(err) => return Err(Diagnostic::new(
                    format!("Invalid token: {}", err),
                    Span::new(start_pos as u32, self.pos as u32),
                )),
            };

            self.last_token = Some(next.kind());
            let span = Span::new(start_pos as u32, self.pos as u32);
            return Ok(Token::new(kind, span));
        }
        // UNREACHABLE
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Span {
    low: u32,
    high: u32,
}

impl Span {
    pub const DUMMY: Self = Self { low: 0, high: 0 };

    pub fn new(low: u32, high: u32) -> Self {
        Self { low, high }
    }

    pub fn low(&self) -> u32 {
        self.low
    }

    pub fn high(&self) -> u32 {
        self.high
    }

    pub fn between(low: Span, high: Span) -> Result<Self, ()> {
        // assert!(low.low() < high.high());
        if low.low() >= high.high() {
            return Err(());
        }
        Ok(Self {
            low: low.low(),
            high: high.high(),
        })
    }
}

#[derive(Debug)]
pub struct Token {
    kind: LexerToken,
    span: Span,
}

impl Token {
    pub fn kind(&self) -> &LexerToken {
        &self.kind
    }

    pub fn span(&self) -> Span {
        self.span
    }

    fn new(kind: LexerToken, span: Span) -> Self {
        Self { kind, span }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum LexerToken {
    EoF,
    Newline,
    Comma,
    Dot,
    LeftBracket,
    RightBracket,
    Plus,
    Ident(String),
    Number(u16),
    Literal,
    Raw(u16),
    LabelDef(String),
    Label(String),
}

impl Display for LexerToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexerToken::EoF => write!(f, "<EoF>"),
            LexerToken::Newline => write!(f, "<\\n>"),
            LexerToken::Comma => write!(f, ","),
            LexerToken::Dot => write!(f, "."),
            LexerToken::LeftBracket => write!(f, "["),
            LexerToken::RightBracket => write!(f, "]"),
            LexerToken::Plus => write!(f, "+"),
            LexerToken::Ident(s) => write!(f, "{}", s),
            LexerToken::Number(val) => write!(f, "{}", val),
            LexerToken::Literal => write!(f, "!"),
            LexerToken::Raw(val) => write!(f, "0x{:04x}", val),
            LexerToken::LabelDef(s) => write!(f, "@:{}", s),
            LexerToken::Label(s) => write!(f, ":{}", s),
        }
    }
}
