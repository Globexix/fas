//! Lexer for fas. Produces a flat token stream with 1-based line/column spans.

use std::fmt;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    pub line: u32,
    pub col: u32,
}

impl Span {
    pub fn new(line: u32, col: u32) -> Self {
        Span { line, col }
    }
}
impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    Ident(String),
    Int(u128),
    Str(String),

    // Punctuation
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Comma,
    Dot,
    Semi,
    At,
    Question,
    Colon,

    // Operators
    Assign, // =
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    PlusEq,
    MinusEq,
    StarEq,
    SlashEq,
    PercentEq,
    AmpEq,
    PipeEq,
    CaretEq,
    Newline,
    Amp,    // &
    Pipe,   // |
    Caret,  // ^
    Tilde,  // ~
    EqEq,   // ==
    NotEq,  // !=
    Lt,     // <
    Le,     // <=
    Gt,     // >
    Ge,     // >=
    AndAnd, // &&
    OrOr,   // ||
    Not,    // !

    Eof,
}

pub struct Lexer {
    src: Vec<char>,
    pos: usize,
    line: u32,
    col: u32,
}

impl Lexer {
    fn peek(&self) -> Option<char> {
        self.src.get(self.pos).copied()
    }

    fn peek2 (&self) -> Option<char> {
        self.src.get(self.pos + 1).copied()
    }

    fn bump(&mut self) -> Option<char> {
        let c = self.src.get(self.pos).copied();
        if let Some(c) = c {
            self.pos += 1;
            if c == '\n' {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
        }
        c
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn span_display() {
        assert_eq!(Span::new(21, 37).to_string(), "21:37");
    }

    #[test]
    fn cursor_peeks_and_tracks_newlines() {
        let mut lx = Lexer { 
            src: "ab\nc".chars().collect(), 
            pos: 0, 
            line: 1, 
            col: 1
        };
        assert_eq!(lx.peek(), Some('a'));
        assert_eq!(lx.peek2(), Some('b'));
        lx.bump();
        lx.bump();
        lx.bump();
        assert_eq!((lx.line, lx.col), (2, 1));
    }
}

