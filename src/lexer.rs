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

    // Keywords
    KwFn,
    KwReturn,
    KwIf,
    KwElse,
    KwWhile,
    KwBreak,
    KwContinue,
    KwConst,
    KwStruct,
    KwOpaque,
    KwExtern,
    KwDefer,
    KwAsm,
    KwFor,
    KwSwitch,
    KwCase,
    KwDefault,

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

impl fmt::Display for Tok {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Tok::Ident(s) => write!(f, "`{s}`"),
            Tok::Int(n) => write!(f, "`{n}`"),
            Tok::Str(s) => write!(f, "\"{s}\""),
            Tok::KwFn => write!(f, "`fn`"),
            Tok::KwReturn => write!(f, "`return`"),
            Tok::KwIf => write!(f, "`if`"),
            Tok::KwElse => write!(f, "`else`"),
            Tok::KwWhile => write!(f, "`while`"),
            Tok::KwBreak => write!(f, "`break`"),
            Tok::KwContinue => write!(f, "`continue`"),
            Tok::KwConst => write!(f, "`const`"),
            Tok::KwStruct => write!(f, "`struct`"),
            Tok::KwOpaque => write!(f, "`opaque`"),
            Tok::KwExtern => write!(f, "`extern`"),
            Tok::KwDefer => write!(f, "`defer`"),
            Tok::KwAsm => write!(f, "`asm`"),
            Tok::KwFor => write!(f, "`for`"),
            Tok::KwSwitch => write!(f, "`switch`"),
            Tok::KwCase => write!(f, "`case`"),
            Tok::KwDefault => write!(f, "`default`"),
            Tok::LParen => write!(f, "`(`"),
            Tok::RParen => write!(f, "`)`"),
            Tok::LBrace => write!(f, "`{{`"),
            Tok::RBrace => write!(f, "`}}`"),
            Tok::LBracket => write!(f, "`[`"),
            Tok::RBracket => write!(f, "`]`"),
            Tok::Comma => write!(f, "`,`"),
            Tok::Dot => write!(f, "`.`"),
            Tok::DotDotDot => write!(f, "`...`"),
            Tok::Semi => write!(f, "`;`"),
            Tok::At => write!(f, "`@`"),
            Tok::Question => write!(f, "`?`"),
            Tok::Colon => write!(f, "`:`"),
            Tok::Assign => write!(f, "`=`"),
            Tok::Plus => write!(f, "`+`"),
            Tok::Minus => write!(f, "`-`"),
            Tok::Star => write!(f, "`*`"),
            Tok::Slash => write!(f, "`/`"),
            Tok::Percent => write!(f, "`%`"),
            Tok::PlusEq => write!(f, "`+=`"),
            Tok::MinusEq => write!(f, "`-=`"),
            Tok::StarEq => write!(f, "`*=`"),
            Tok::SlashEq => write!(f, "`/=`"),
            Tok::PercentEq => write!(f, "`%=`"),
            Tok::AmpEq => write!(f, "`&=`"),
            Tok::PipeEq => write!(f, "`|=`"),
            Tok::CaretEq => write!(f, "`^=`"),
            Tok::Newline => write!(f, "newline"),
            Tok::Amp => write!(f, "`&`"),
            Tok::Pipe => write!(f, "`|`"),
            Tok::Caret => write!(f, "`^`"),
            Tok::Tilde => write!(f, "`~`"),
            Tok::EqEq => write!(f, "`==`"),
            Tok::NotEq => write!(f, "`!=`"),
            Tok::Lt => write!(f, "`<`"),
            Tok::Le => write!(f, "`<=`"),
            Tok::Gt => write!(f, "`>`"),
            Tok::Ge => write!(f, "`>=`"),
            Tok::AndAnd => write!(f, "`&&`"),
            Tok::OrOr => write!(f, "`||`"),
            Tok::Not => write!(f, "`!`"),
            Tok::Eof => write!(f, "end of file"),
        }
    }
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
                Some(' ') | Some('\t') | Some('\r') => {
                    self.bump();
                }
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
                                return Err(format!(
                                    "unterminated block comment starting at {start}"
                                ));
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
                self.bump();
                let mut s = String::new();
                loop {
                    match self.peek() {
                        None => return Err(format!("unterminated string literal at {span}")),
                        Some('"') => {
                            self.bump();
                            break;
                        }
                        Some('\\') => {
                            self.bump();
                            let esc = self
                                .bump()
                                .ok_or_else(|| format!("unterminated escape at {span}"))?;
                            match esc {
                                'n' => s.push('\n'),
                                't' => s.push('\t'),
                                'r' => s.push('\r'),
                                '\\' => s.push('\\'),
                                '"' => s.push('"'),
                                '0' => s.push('\0'),
                                other => {
                                    return Err(format!("unknown escape `\\{other}` at {span}"));
                                }
                            }
                        }
                        Some(c) => {
                            s.push(c);
                            self.bump();
                        }
                    }
                }
                Ok(Tok::Str(s))
            }
            c if c.is_ascii_digit() => self.lex_number(span),
            c if is_ident_start(c) => {
                self.bump();

                let mut s = String::new();
                s.push(c);

                while let Some(c) = self.peek() {
                    if is_ident_continue(c) {
                        s.push(c);
                        self.bump();
                    } else {
                        break;
                    }
                }
                Ok(keyword_or_ident(&s))
            }
            other => Err(format!("unexpected character `{other}` at {span}")),
        }
    }

    fn lex_number(&mut self, span: Span) -> Result<Tok, String> {
        let mut s = String::new();

        while let Some(c) = self.peek() {
            if c.is_ascii_alphanumeric() || c == '_' {
                s.push(c);
                self.bump();
            } else {
                break;
            }
        }

        let cleaned: String = s.chars().filter(|&c| c != '_').collect();

        let (radix, digits) = if let Some(rest) = cleaned.strip_prefix("0x") {
            (16, rest)
        } else if let Some(rest) = cleaned.strip_prefix("0X") {
            (16, rest)
        } else if let Some(rest) = cleaned.strip_prefix("0b") {
            (2, rest)
        } else if let Some(rest) = cleaned.strip_prefix("0B") {
            (2, rest)
        } else if let Some(rest) = cleaned.strip_prefix("0o") {
            (8, rest)
        } else if let Some(rest) = cleaned.strip_prefix("0O") {
            (8, rest)
        } else {
            (10, cleaned.as_str())
        };

        if digits.is_empty() {
            return Err(format!("invalid integer literal `{s}` at {span}"));
        }

        let mut val: u128 = 0;
        for c in digits.chars() {
            let d = c
                .to_digit(radix)
                .ok_or_else(|| format!("invalid digit `{c}` in integer literal `{s}` at {span}"))?;
            val = val
                .checked_mul(radix as u128)
                .and_then(|v| v.checked_add(d as u128))
                .ok_or_else(|| format!("integer literal `{s}` overflows 128 bits at {span}"))?;
        }

        Ok(Tok::Int(val))
    }
}

fn is_ident_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_ident_continue(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

fn keyword_or_ident(s: &str) -> Tok {
    match s {
        "fn" => Tok::KwFn,
        "return" => Tok::KwReturn,
        "if" => Tok::KwIf,
        "else" => Tok::KwElse,
        "while" => Tok::KwWhile,
        "break" => Tok::KwBreak,
        "continue" => Tok::KwContinue,
        "const" => Tok::KwConst,
        "struct" => Tok::KwStruct,
        "opaque" => Tok::KwOpaque,
        "extern" => Tok::KwExtern,
        "defer" => Tok::KwDefer,
        "asm" => Tok::KwAsm,
        "for" => Tok::KwFor,
        "switch" => Tok::KwSwitch,
        "case" => Tok::KwCase,
        "default" => Tok::KwDefault,
        _ => Tok::Ident(s.to_string()),
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
            }
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
    fn token_display() {
        assert_eq!(Tok::KwFn.to_string(), "`fn`");
        assert_eq!(Tok::PlusEq.to_string(), "`+=`");
        assert_eq!(Tok::Newline.to_string(), "newline");
        assert_eq!(Tok::LBrace.to_string(), "`{`");
    }

    #[test]
    fn cursor_peeks_and_tracks_newlines() {
        let mut lx = Lexer {
            src: "ab\nc".chars().collect(),
            pos: 0,
            line: 1,
            col: 1,
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
        assert_eq!(
            tokens,
            vec![(Tok::Newline, Span::new(1, 1)), (Tok::Eof, Span::new(2, 1)),]
        );
    }

    #[test]
    fn comments_are_skipped_but_line_breaks_remain() {
        let tokens = lex("// line\n /* block\ncomment */").unwrap();
        assert_eq!(tokens[0].0, Tok::Newline);
        assert_eq!(tokens.last().unwrap().0, Tok::Eof);
        assert_eq!(
            lex("/*").unwrap_err(),
            "unterminated block comment starting at 1:3"
        );
    }

    #[test]
    fn strings_decode() {
        let tokens = lex("\"hello\"\"a\nb\"\"tab\there\"\"quote\\\"q\"\"nul\0x\"").unwrap();
        assert_eq!(tokens[0].0, Tok::Str("hello".to_string()));
        assert_eq!(tokens[1].0, Tok::Str("a\nb".to_string()));
        assert_eq!(tokens[2].0, Tok::Str("tab\there".to_string()));
        assert_eq!(tokens[3].0, Tok::Str("quote\"q".to_string()));
        assert_eq!(tokens[4].0, Tok::Str("nul\0x".to_string()));
    }

    #[test]
    fn numbers_pass() {
        let tokens = lex("0 42 1_000_000 0xff 0xFF 0b1010 0o17").unwrap();
        assert_eq!(tokens[0].0, Tok::Int(0));
        assert_eq!(tokens[1].0, Tok::Int(42));
        assert_eq!(tokens[2].0, Tok::Int(1000000));
        assert_eq!(tokens[3].0, Tok::Int(255));
        assert_eq!(tokens[4].0, Tok::Int(255));
        assert_eq!(tokens[5].0, Tok::Int(10));
        assert_eq!(tokens[6].0, Tok::Int(15));
    }

    #[test]
    fn numbers_reject() {
        let error = lex("0x").expect_err("lexer should reject");
        let error2 = lex("0xG").expect_err("lexer should reject");
        let error3 =
            lex("9999999999999999999999999999999999999999999999").expect_err("lexer should reject");
        assert!(error.contains("invalid integer literal"));
        assert!(error2.contains("invalid digit"));
        assert!(error3.contains("overflows 128 bits"));
    }

    #[test]
    fn ident_and_keywords() {
        let src = lex(
            "fn main() { return if else while break continue const struct opaque extern defer asm
                       for switch case default x2 _tmp foo123 ifx}",
        )
        .unwrap();

        assert_eq!(src[0].0, Tok::KwFn);
        assert_eq!(src[1].0, Tok::Ident("main".to_string()));
        assert_eq!(src[2].0, Tok::LParen);
    }
}
