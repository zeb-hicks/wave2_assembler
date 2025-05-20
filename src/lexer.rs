#![allow(clippy::uninlined_format_args)]
use std::fmt::Display;

use crate::{diag::Diagnostic, reader::{self, Reader}};

/// this is a different thing from reader to be able to easily separate
/// the splitting and the getting source mechanisms
#[derive(Debug)]
pub struct Lexer<'a> {
    src: &'a str,
    pos: usize,
    reader: Reader<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            src,
            pos: 0,
            reader: Reader::new(src),
        }
    }

    pub fn next_token(&mut self) -> Result<Token, Diagnostic> {
        use TokenKind::*;

        // loops to skip whitespace
        loop {
            let next = self.reader.next();

            let start_pos = self.pos;
            self.pos += next.len();

            let kind: Result<TokenKind, String> = match next.kind() {
                reader::TokenKind::Newline => Ok(Newline),
                reader::TokenKind::Whitespace | reader::TokenKind::Comment => continue,
                reader::TokenKind::Ident => Ok(Ident(self.src[start_pos..self.pos].to_string())),
                reader::TokenKind::Number => match self.src[start_pos..self.pos].parse::<u16>() {
                    Ok(num) => Ok(Number(num)),
                    Err(_) => Err(format!("Invalid 16-bit number: {}", &self.src[start_pos..self.pos])),
                },
                reader::TokenKind::Hex => match u16::from_str_radix(&self.src[(start_pos+1)..self.pos], 16) {
                    Ok(num) => Ok(Number(num)),
                    Err(_) => Err(format!("Invalid 16-bit hex: {}", &self.src[(start_pos+1)..self.pos])),
                },
                reader::TokenKind::Literal => Ok(Literal),
                reader::TokenKind::Raw => match u16::from_str_radix(&self.src[start_pos..self.pos].to_string(), 16) {
                    Ok(raw) => Ok(Raw(raw)),
                    Err(_) => Err(format!("Error parsing literal: {}", &self.src[start_pos..self.pos])),
                }
                reader::TokenKind::EoF => Ok(EoF),
                reader::TokenKind::Comma => Ok(Comma),
                reader::TokenKind::Dot => Ok(Dot),
                reader::TokenKind::LeftBracket => Ok(LeftBracket),
                reader::TokenKind::RightBracket => Ok(RightBracket),
                reader::TokenKind::Plus => Ok(Plus),
                reader::TokenKind::Label => {
                    let label = self.src[start_pos..self.pos].to_string();
                    if label.len() < 2 || !label.starts_with(':') {
                        return Err(Diagnostic::new(
                            format!("Invalid label: {}", label),
                            Span::new(start_pos as u32, self.pos as u32),
                        ));
                    }
                    Ok(Label(label[1..].to_string()))
                }
            };
            let kind = match kind {
                Ok(kind) => kind,
                Err(err) => return Err(Diagnostic::new(
                    format!("Invalid token: {}", err),
                    Span::new(start_pos as u32, self.pos as u32),
                )),
            };

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
    kind: TokenKind,
    span: Span,
}

impl Token {
    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }

    pub fn span(&self) -> Span {
        self.span
    }

    fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum TokenKind {
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
    Label(String),
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::EoF => write!(f, "<EoF>"),
            TokenKind::Newline => write!(f, "<\\n>"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Dot => write!(f, "."),
            TokenKind::LeftBracket => write!(f, "["),
            TokenKind::RightBracket => write!(f, "]"),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Ident(s) => write!(f, "{}", s),
            TokenKind::Number(val) => write!(f, "{}", val),
            TokenKind::Literal => write!(f, "!"),
            TokenKind::Raw(val) => write!(f, "0x{:04x}", val),
            TokenKind::Label(s) => write!(f, ":{}", s),
        }
    }
}
