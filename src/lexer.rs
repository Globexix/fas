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

    fn peek2(&self) -> Option<char> {
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

    fn skip_hspace_and_comments(&mut self) -> Result<(), String> {
        Ok(())
    }

    fn next_token(&mut self, c: char, span: Span) -> Result<Tok, String> {
        Err(format!("token logic not implemented for `{c}` at {span}"))
    }
}

pub fn lex(src: &str) -> Result<Vec<(Tok, Span)>, String> {
    let mut lx = Lexer {
        src: src.chars().collect(),
        pos: 0,
        line: 1,
        col: 1,
    };

    let mut out = Vec::new();

    loop {
        lx.skip_hspace_and_comments()?;
        let span = Span::new(lx.line, lx.col);
        match lx.peek() {
            None => {
                out.push((Tok::Eof, span));
                break;
            },
            Some('\n') => {
                lx.bump();
                out.push((Tok::Newline, span));
                
            }
            Some(c) => {
                let tok = lx.next_token(c, span)?;
                out.push((tok, span));
            }
        }
    }

    Ok(out)
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

    #[test]
    fn emtpy_input_produces_eof() {
        let tokens = lex("").expect("emtpy source should lex");
        assert_eq!(tokens, vec![(Tok::Eof, Span::new(1, 1))]);
    }

    #[test]
    fn newline_is_significant() {
        let tokens = lex("\n").expect("newline should lex");
        assert_eq!(tokens, vec![
            (Tok::Newline, Span::new(1, 1)),
            (Tok::Eof, Span::new(2, 1)),
        ]);
    }

    #[test]
    fn unfinished_token_logic_is_an_error() {
        let error = lex("x").expect_err("stub should reject x");
        assert!(error.contains("x"));
        assert!(error.contains("1:1"));
    }
}

