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
    DotDotDot,
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
        loop {
            match self.peek() {
                Some(' ') | Some('\t') | Some('\r') => { self.bump(); }
                Some('/') if self.peek2() == Some('/') => {
                    while let Some(c) = self.peek() {
                        if c == '\n' {
                            break;
                        }
                        self.bump();
                    }
                }
                Some('/') if self.peek2() == Some('*') => {
                    self.bump();
                    self.bump();
                    let start = Span::new(self.line, self.col);

                    loop {
                        match self.peek() {
                            None => {
                                return Err(format!("unterminated block comment starting at {start}"))
                            }
                            Some('*') if self.peek2() == Some('/') => {
                                self.bump();
                                self.bump();
                                break;
                            }
                            _ => {
                                self.bump();
                            }
                        }
                    }
                }
                _ => break,
            }
        }
        Ok(())
    }

    fn next_token(&mut self, c: char, span: Span) -> Result<Tok, String> {
        match c {
            '(' => {
                self.bump();
                Ok(Tok::LParen)
            }
            ')' => {
            self.bump();
            Ok(Tok::RParen)
            }
            '{' => {
                self.bump();
                Ok(Tok::LBrace)
            }
            '}' => {
                self.bump();
                Ok(Tok::RBrace)
            }
            '[' => {
                self.bump();
                Ok(Tok::LBracket)
            }
            ']' => {
                self.bump();
                Ok(Tok::RBracket)
            }
            ',' => {
                self.bump();
                Ok(Tok::Comma)
            }
            '.' => {
                self.bump();
                if self.peek() == Some('.') && self.peek2() == Some('.') {
                    self.bump();
                    self.bump();
                    Ok(Tok::DotDotDot)
                } else {
                    Ok(Tok::Dot)
                }
            }
            ';' => {
                self.bump();
                Ok(Tok::Semi)
            }
            '@' => {
                self.bump();
                Ok(Tok::At)
            }
            '?' => {
                self.bump();
                Ok(Tok::Question)
            }
            ':' => {
                self.bump();
                Ok(Tok::Colon)
            }
            '+' => {
                self.bump();
                if self.peek() == Some('=') {
                    self.bump();
                    Ok(Tok::PlusEq)
                } else {
                    Ok(Tok::Plus)
                }
            }
            '-' => {
                self.bump();
                if self.peek() == Some('=') {
                    self.bump();
                    Ok(Tok::MinusEq)
                } else {
                    Ok(Tok::Minus)
                }
            }
            '*' => {
                self.bump();
                if self.peek() == Some('=') {
                    self.bump();
                    Ok(Tok::StarEq)
                } else {
                    Ok(Tok::Star)
                }
            }
            '%' => {
                self.bump();
                if self.peek() == Some('=') {
                    self.bump();
                    Ok(Tok::PercentEq)
                } else {
                    Ok(Tok::Percent)
                }
            }
            '~' => {
                self.bump();
                Ok(Tok::Tilde)
            }
            '=' => {
                self.bump();
                if self.peek() == Some('=') {
                    self.bump();
                    Ok(Tok::EqEq)
                } else {
                    Ok(Tok::Assign)
                }
            }
            '!' => {
                self.bump();
                if self.peek() == Some('=') {
                    self.bump();
                    Ok(Tok::NotEq)
                } else {
                    Ok(Tok::Not)
                }
            }
            '<' => {
                self.bump();
                if self.peek() == Some('=') {
                    self.bump();
                    Ok(Tok::Le)
                } else {
                    Ok(Tok::Lt)
                }
            }
            '>' => {
                self.bump();
                if self.peek() == Some('=') {
                    self.bump();
                    Ok(Tok::Ge)
                } else {
                    Ok(Tok::Gt)
                }
            }
            '&' => {
                self.bump();
                if self.peek() == Some('&') {
                    self.bump();
                    Ok(Tok::AndAnd)
                } else if self.peek() == Some('=') {
                    self.bump();
                    Ok(Tok::AmpEq)
                } else {
                    Ok(Tok::Amp)
                }
            }
            '|' => {
                self.bump();
                if self.peek() == Some('|') {
                    self.bump();
                    Ok(Tok::OrOr)
                } else if self.peek() == Some('=') {
                    self.bump();
                    Ok(Tok::PipeEq)
                } else {
                    Ok(Tok::Pipe)
                }
            }
            '^' => {
                self.bump();
                if self.peek() == Some('=') {
                    self.bump();
                    Ok(Tok::CaretEq)
                } else {
                    Ok(Tok::Caret)
                }
            }
            '/' => {
                self.bump();
                if self.peek() == Some('=') {
                    self.bump();
                    Ok(Tok::SlashEq)
                } else {
                    Ok(Tok::Slash)
                }
            }
            '"' => {
                unreachable!()
            }
            c if c.is_ascii_digit() => {
                unreachable!()
            }
            c if is_ident_start(c) => {
                unreachable!()
            }
            other => Err(format!("unexpected character `{other}` at {span}"))
        }
    }
}

fn is_ident_start(_c: char) -> bool {
    false
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

    #[test]
    fn comments_are_skipped_but_line_breaks_remain() {
        let tokens = lex("// line\n /* block\ncomment */").unwrap();
        assert_eq!(tokens[0].0, Tok::Newline);
        assert_eq!(tokens.last().unwrap().0, Tok::Eof);
        assert_eq!(lex("/*").unwrap_err(), "unterminated block comment starting at 1:3");
    }
}

