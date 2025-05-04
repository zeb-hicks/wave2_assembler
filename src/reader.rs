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
            return Token::new(TokenKind::EoF, 0);
        };

        if self.inside_literal && start_c != '\n' {
            println!("Inside literal");
            println!("Start char: {:?}", start_c);

            let mut i = 1;
            self.eat_while(move |c| {
                i += 1;
                match (i, c) {
                    (1..=4, '0'..='9') => true,
                    (1..=4, 'a'..='f') => true,
                    (1..=4, 'A'..='F') => true,
                    _ => false,
                }
            });
            // self.eat_while(|c: char| !c.is_ascii_hexdigit());
            // self.eat_whitespace();
            println!("Char count: {}", self.token_len());
            let token = Token::new(TokenKind::Raw, self.token_len());
            self.reset_len();
            return token;
        } else {
            self.inside_literal = false;
        }

        let kind = match start_c {
            '#' => self.comment(),
            ';' => self.comment(),
            '!' => self.literal(),
            '\n' => self.newline(),
            c if c.is_whitespace() => self.eat_whitespace(),
            c if is_ident_start(c) => self.ident(),

            c if c.is_ascii_digit() => self.number(),

            ',' => TokenKind::Comma,
            '.' => TokenKind::Dot,
            '[' => TokenKind::LeftBracket,
            ']' => TokenKind::RightBracket,
            '+' => TokenKind::Plus,

            _ => {
                panic!("unexpected start of token {}", start_c)
            }
        };
        let token = Token::new(kind, self.token_len());
        self.reset_len();
        token
    }

    fn comment(&mut self) -> TokenKind {
        self.eat_while(|c| c != '\n');
        TokenKind::Comment
    }

    fn literal(&mut self) -> TokenKind {
        self.inside_literal = true;
        TokenKind::Literal
    }

    fn newline(&mut self) -> TokenKind {
        self.eat_while(|c| c == '\n');
        self.inside_literal = false;
        TokenKind::Newline
    }

    fn eat_whitespace(&mut self) -> TokenKind {
        self.eat_while(|c| c != '\n' && c.is_whitespace());
        TokenKind::Whitespace
    }

    fn ident(&mut self) -> TokenKind {
        self.eat_while(is_ident_continue);
        TokenKind::Ident
    }

    fn number(&mut self) -> TokenKind {
        self.eat_while(|c: char| c.is_ascii_digit());
        TokenKind::Number
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
    kind: TokenKind,
    len: usize,
}

impl Token {
    pub fn kind(&self) -> TokenKind {
        self.kind
    }

    pub fn len(&self) -> usize {
        self.len
    }

    fn new(kind: TokenKind, len: usize) -> Self {
        Self { kind, len }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
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
    Literal,
    Raw,
}
