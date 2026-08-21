//! Recursive-descent parser for fas.

#![allow(dead_code, unused_imports)]

use crate::ast::*;
use crate::lexer::{lex, Span, Tok};

pub fn parse_file(src: &str) -> Result<Program, String> {
    let (clean, asm_bodies) =  extract_asm_bodies(src)?;
    let toks = lex(&clean)?;

    let mut p = Parser {
        toks,
        pos: 0,
        struct_names: Vec::new(),
        opaque_names: Vec::new(),
        asm_bodies,
        depth: 0,
    };
    
    p.parse_program()
}

fn extract_asm_bodies(_src: &str) -> Result<(String, Vec<(String, String)>), String> {
    todo!("extract_asm_bodies")
}

fn ident_at(_chars: &[char], _i: usize, _s: &str) -> bool {
    todo!("ident_at")
}

fn skip_ws_comments(_chars: &[char], _i: &mut usize) {
    todo!("skip_ws_comments")
}

struct Parser {
    toks: Vec<(Tok, Span)>,
    pos: usize,
    struct_names: Vec<String>,
    opaque_names: Vec<String>,
    asm_bodies: Vec<(String, String)>,
    depth: usize,
}

impl Parser {
    fn peek(&self) -> &Tok {
        &self.toks[self.pos].0
    }
    fn peek_n(&self, n: usize) -> &Tok {
        let i = (self.pos + n).min(self.toks.len() - 1);
        &self.toks[i].0
    }
    fn span(&self) -> Span {
        self.toks[self.pos].1
    }
    fn bump(&mut self) -> (Tok, Span) {
        let t = self.toks[self.pos].clone();
        if self.pos + 1 < self.toks.len() {
            self.pos += 1;
        }
        t
    }
    fn at(&self, t: &Tok) -> bool {
        self.peek() == t
    }
    fn at_ident(&self, s: &str) -> bool {
        matches!(self.peek(), Tok::Ident(x) if x == s)
    }
    fn expect(&mut self, t: &Tok) -> Result<(Tok, Span), String> {
        if self.at(t) {
            Ok(self.bump())
        } else {
            Err(format!("expected {t}, found {}", self.peek()))
        }
    }
    fn expect_ident(&mut self) -> Result<(String, Span), String> {
        match self.bump() {
            (Tok::Ident(s), sp) => Ok((s, sp)),
            (other, _) => Err(format!("expected identifier, found {other}")),
        }
    }
    fn eat(&mut self, t: &Tok) -> bool {
        if self.at(t) {
            self.bump();
            true
        } else {
            false
        }
    }
    fn err<T>(&self, msg: &str) -> Result<T, String> {
        Err(format!("{msg} at {}", self.span()))
    }
    fn skip_newlines(&mut self) {
        while self.at(&Tok::Newline) {
            self.bump();
        }
    }
    fn end_stmt(&mut self) -> Result<(), String> {
        if self.eat(&Tok::Semi) {
            self.skip_newlines();
            return Ok(());
        }

        if self.at(&Tok::Newline) {
            self.skip_newlines();
            return Ok(());
        }

        if self.at(&Tok::RBrace) || self.at(&Tok::Eof) {
            return Ok(());
        }

        self.err("expected end of statement (newline or `;`)")
    }

    fn parse_program(&mut self) -> Result<Program, String> {
        todo!("parse_program")
    }
    fn parse_item(&mut self) -> Result<Vec<Item>, String> {
        todo!("parse_item")
    }
    fn parse_attr(&mut self) -> Result<Attr, String> {
        todo!("parse_attr")
    }
    fn parse_string(&mut self) -> Result<String, String> {
        todo!("parse_string")
    }
    fn parse_u32(&mut self) -> Result<u32, String> {
        todo!("parse_u32")
    }
    fn parse_u64(&mut self) -> Result<u64, String> {
        todo!("parse_u64")
    }
    fn parse_const(&mut self) -> Result<Item, String> {
        todo!("parse_const")
    }
    fn parse_struct(&mut self) -> Result<Item, String> {
        todo!("parse_struct")
    }
    fn parse_opaque(&mut self, _attrs: Vec<Attr>) -> Result<Item, String> {
        todo!("parse_opaque")
    }
    fn parse_fn(&mut self, _attrs: Vec<Attr>) -> Result<Item, String> {
        todo!("parse_fn")
    }
    fn parse_const_params(&mut self) -> Result<Vec<ConstParam>, String> {
        todo!("parse_const_params")
    }
    fn parse_extern_block(&mut self, _attrs: Vec<Attr>) -> Result<Vec<Item>, String> {
        todo!("parse_extern_block")
    }
    fn parse_extern_decl(&mut self) -> Result<Item, String> {
        todo!("parse_extern_decl")
    }
    fn parse_asm_fn(&mut self, _attrs: Vec<Attr>) -> Result<Item, String> {
        todo!("parse_asm_fn")
    }
    fn parse_signature(&mut self, _allow_variadic: bool) -> Result<(Vec<Param>, Ty, bool), String> {
        todo!("parse_signature")
    }
    fn parse_block(&mut self) -> Result<Vec<Stmt>, String> {
        todo!("parse_block")
    }

    fn starts_type(&self, _t: &Tok) -> bool {
        todo!("starts_type")
    }
    fn parse_type(&mut self) -> Result<Ty, String> {
        todo!("parse_type")
    }
    fn parse_stmt(&mut self) -> Result<Stmt, String> {
        todo!("parse_stmt")
    }
    fn looks_like_decl(&self) -> bool {
        todo!("looks_like_decl")
    }
    fn parse_decl(&mut self) -> Result<Stmt, String> {
        todo!("parse_decl")
    }
    fn parse_assign_or_expr_stmt(&mut self) -> Result<Stmt, String> {
        todo!("parse_assign_or_expr_stmt")
    }
    fn parse_if(&mut self) -> Result<Stmt, String> {
        todo!("parse_if")
    }
    fn parse_while(&mut self) -> Result<Stmt, String> {
        todo!("parse_while")
    }
    fn parse_for(&mut self) -> Result<Stmt, String> {
        todo!("parse_for")
    }
    fn parse_for_clause(&mut self) -> Result<Stmt, String> {
        todo!("parse_for_clause")
    }
    fn parse_switch(&mut self) -> Result<Stmt, String> {
        todo!("parse_switch")
    }
    fn parse_case_body(&mut self) -> Result<Vec<Stmt>, String> {
        todo!("parse_case_body")
    }

    fn parse_expr(&mut self) -> Result<Expr, String> {
        todo!("parse_expr")
    }
    fn parse_ternary(&mut self) -> Result<Expr, String> {
        todo!("parse_ternary")
    }
    fn parse_or(&mut self) -> Result<Expr, String> {
        todo!("parse_or")
    }
    fn parse_and(&mut self) -> Result<Expr, String> {
        todo!("parse_and")
    }
    fn parse_bitor(&mut self) -> Result<Expr, String> {
        todo!("parse_bitor")
    }
    fn parse_bitxor(&mut self) -> Result<Expr, String> {
        todo!("parse_bitxor")
    }
    fn parse_bitand(&mut self) -> Result<Expr, String> {
        todo!("parse_bitand")
    }
    fn parse_equality(&mut self) -> Result<Expr, String> {
        todo!("parse_equality")
    }
    fn parse_relational(&mut self) -> Result<Expr, String> {
        todo!("parse_relational")
    }
    fn parse_additive(&mut self) -> Result<Expr, String> {
        todo!("parse_additive")
    }
    fn parse_multiplicative(&mut self) -> Result<Expr, String> {
        todo!("parse_multiplicative")
    }
    fn parse_unary(&mut self) -> Result<Expr, String> {
        todo!("parse_unary")
    }
    fn parse_postfix(&mut self) -> Result<Expr, String> {
        todo!("parse_postfix")
    }
    fn parse_args(&mut self) -> Result<Vec<Expr>, String> {
        todo!("parse_args")
    }
    fn parse_primary(&mut self) -> Result<Expr, String> {
        todo!("parse_primary")
    }
}

fn ident_of(_e: &Expr) -> String {
    todo!("ident_of")
}
fn assign_target_of(_e: Expr) -> Result<AssignTarget, String> {
    todo!("assign_target_of")
}
