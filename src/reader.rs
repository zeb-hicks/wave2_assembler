#![allow(clippy::uninlined_format_args,clippy::match_like_matches_macro)]
use std::str::Chars;

/// this is its own thing because it turns out to be easier to just collect the
/// lengths of tokens and then lex tokens from the lengths
#[derive(Debug)]
pub struct Reader<'a> {
    chars: Chars<'a>,
    // the number of characters remaining in `source` at the start of the current token
    len_at_start: usize,
    inside_literal: bool,
}

impl<'a> Reader<'a> {
    pub fn new(src: &'a str) -> Self {
        let len_at_start = src.len();
        Self {
            chars: src.chars(),
            len_at_start,
            inside_literal: false,
        }
    }

    pub fn next(&mut self) -> Token {
        let Some(start_c) = self.chars.next() else {
            return Token::new(ReaderToken::EoF, 0);
        };

        if self.inside_literal && start_c != '\n' {
            self.eat_while(move |c| c.is_ascii_hexdigit());
            let token = Token::new(ReaderToken::Raw, self.token_len());
            self.reset_len();
            return token;
        } else {
            self.inside_literal = false;
        }

        let kind = match start_c {
            '#' => self.comment(),
            ';' => self.comment(),
            '!' => self.literal(),
            '$' => self.hex(),
            '\n' => self.newline(),
            c if c.is_whitespace() => self.eat_whitespace(),
            c if is_ident_start(c) => self.ident(),

            c if c.is_ascii_digit() => self.number(),

            ',' => ReaderToken::Comma,
            '.' => ReaderToken::Dot,
            '[' => ReaderToken::LeftBracket,
            ']' => ReaderToken::RightBracket,
            '+' => ReaderToken::Plus,
            ':' => self.label(),

            _ => {
                panic!("unexpected start of token {}", start_c)
            }
        };
        let token = Token::new(kind, self.token_len());
        self.reset_len();
        token
    }

    fn comment(&mut self) -> ReaderToken {
        self.inside_literal = false;
        self.eat_while(|c| c != '\n');
        ReaderToken::Comment
    }

    fn literal(&mut self) -> ReaderToken {
        self.inside_literal = true;
        ReaderToken::Literal
    }

    fn label(&mut self) -> ReaderToken {
        self.eat_while(|c| c.is_alphanumeric() || c == '_');
        ReaderToken::Label
    }

    fn hex(&mut self) -> ReaderToken {
        self.eat_while(|c| c.is_ascii_hexdigit());
        ReaderToken::Hex
    }

    fn newline(&mut self) -> ReaderToken {
        self.eat_while(|c| c == '\n');
        self.inside_literal = false;
        ReaderToken::Newline
    }

    fn eat_whitespace(&mut self) -> ReaderToken {
        self.eat_while(|c| c != '\n' && c.is_whitespace());
        ReaderToken::Whitespace
    }

    fn ident(&mut self) -> ReaderToken {
        self.eat_while(is_ident_continue);
        ReaderToken::Ident
    }

    fn number(&mut self) -> ReaderToken {
        self.eat_while(|c: char| c.is_ascii_digit());
        ReaderToken::Number
    }

    fn token_len(&self) -> usize {
        self.len_at_start - self.chars.as_str().len()
    }

    fn reset_len(&mut self) {
        self.len_at_start = self.chars.as_str().len()
    }

    fn at_eof(&self) -> bool {
        self.chars.as_str().is_empty()
    }

    fn eat_while(&mut self, mut f: impl FnMut(char) -> bool) {
        while f(self.chars.clone().next().unwrap_or('\0')) && !self.at_eof() {
            self.chars.next();
        }
    }
}

fn is_ident_start(c: char) -> bool {
    matches!(c, 'a'..='z'|'A'..='Z'|'_')
}

fn is_ident_continue(c: char) -> bool {
    matches!(c, 'a'..='z'|'A'..='Z'|'0'..='9'|'_')
}

#[derive(Debug)]
pub struct Token {
    kind: ReaderToken,
    len: usize,
}

impl Token {
    pub fn kind(&self) -> ReaderToken {
        self.kind
    }

    pub fn len(&self) -> usize {
        self.len
    }

    fn new(kind: ReaderToken, len: usize) -> Self {
        Self { kind, len }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReaderToken {
    EoF,
    Newline,
    Whitespace,
    Comment,
    Comma,
    Dot,
    LeftBracket,
    RightBracket,
    Plus,
    Ident,
    Number,
    Hex,
    Literal,
    Raw,
    Label,
}
