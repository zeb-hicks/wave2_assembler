use std::fmt::Display;

use crate::reader::{self, Reader};

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

    pub fn next_token(&mut self) -> Token {
        use TokenKind::*;

        // loops to skip whitespace
        loop {
            let next = self.reader.next();

            let start_pos = self.pos;
            self.pos += next.len();

            let kind = match next.kind() {
                reader::TokenKind::Newline => Newline,
                reader::TokenKind::Whitespace | reader::TokenKind::Comment => continue,
                reader::TokenKind::Ident => Ident(self.src[start_pos..self.pos].to_string()),
                reader::TokenKind::Number => Number(
                    self.src[start_pos..self.pos]
                        .parse()
                        .expect("lexed number literals should be valid"),
                ),
                reader::TokenKind::Literal => Literal,
                reader::TokenKind::Raw => {
                    let s = self.src[start_pos..self.pos].to_string();
                    let raw = u16::from_str_radix(&s, 16)
                        .expect("lexed raw literals should be valid");
                    Raw(raw)
                }
                reader::TokenKind::EoF => EoF,
                reader::TokenKind::Comma => Comma,
                reader::TokenKind::Dot => Dot,
                reader::TokenKind::LeftBracket => LeftBracket,
                reader::TokenKind::RightBracket => RightBracket,
                reader::TokenKind::Plus => Plus,
            };
            let span = Span::new(start_pos as u32, self.pos as u32);
            return Token::new(kind, span);
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

    pub fn between(low: Span, high: Span) -> Self {
        assert!(low.low() < high.high());
        Self {
            low: low.low(),
            high: high.high(),
        }
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
        }
    }
}
