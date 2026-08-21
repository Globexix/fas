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
        let mut items = Vec::new();
        self.skip_newlines();

        loop {
            if self.at(&Tok::Eof) {
                break;
            }

            items.extend(self.parse_item()?);
            self.skip_newlines();
        }

        Ok(Program { items })
    }
    fn parse_item(&mut self) -> Result<Vec<Item>, String> {
        let mut attrs = Vec::new();
        while self.at(&Tok::At) {
            attrs.push(self.parse_attr()?);
            self.skip_newlines();
        }
        
        match self.peek().clone() {
            Tok::KwConst => Ok(vec![self.parse_const()?]),
            Tok::KwStruct => Ok(vec![self.parse_struct()?]),
            Tok::KwOpaque => Ok(vec![self.parse_opaque(attrs)?]),
            Tok::KwExtern => self.parse_extern_block(attrs),
            Tok::KwAsm => Ok(vec![self.parse_asm_fn(attrs)?]),
            Tok::KwFn => Ok(vec![self.parse_fn(attrs)?]),
            other => self.err(&format!("expected a top-level item, found {other}")),
        }
    }
    fn parse_attr(&mut self) -> Result<Attr, String> {
        self.expect(&Tok::At)?;
        let (name, sp) = self.expect_ident()?;
        match name.as_str() {
            "inline" => Ok(Attr::Inline),
            "noinline" => Ok(Attr::NoInline),
            "kernel" => Ok(Attr::Kernel),
            "optimize" => Ok(Attr::Optimize),
            "expect_no_call" => Ok(Attr::ExpectNoCall),
            "target" => {
                self.expect(&Tok::LParen)?;
                let s = self.parse_string()?;
                self.expect(&Tok::RParen)?;
                if !matches!(
                    s.as_str(),
                    "x86_64"
                        | "avx2"
                        | "avx512"
                        | "zen1"
                        | "zen2"
                        | "zen3"
                        | "zen4"
                        | "zen5"
                        | "skylake"
                ) {
                    return Err(format!("unsupported target `{s}` at {sp}"));
                }
                Ok(Attr::Target(s))
            }
            "align" => {
                self.expect(&Tok::LParen)?;
                let n = self.parse_u32()?;
                self.expect(&Tok::RParen)?;
                Ok(Attr::Align(n))
            }
            "expect_asm" => {
                self.expect(&Tok::LParen)?;
                let s = self.parse_string()?;
                self.expect(&Tok::RParen)?;
                Ok(Attr::ExpectAsm(s))
            }
            "expect_stack_max" => {
                self.expect(&Tok::LParen)?;
                let n = self.parse_u64()?;
                self.expect(&Tok::RParen)?;
                Ok(Attr::ExpectStackMax(n))
            }
            other => Err(format!("unknown attribute `@{other}` at {sp}")),
        }
    }
    fn parse_string(&mut self) -> Result<String, String> {
        match self.bump() {
            (Tok::Str(s), _) => Ok(s),
            (other, _) => Err(format!("expected string literal, found {other}")),
        }
    }
    fn parse_u32(&mut self) -> Result<u32, String> {
        match self.bump() {
            (Tok::Int(n), sp) => u32::try_from(n).map_err(|_| format!("integer out of range for u32 at {sp}")),
            (other, _) => Err(format!("expected integer, found {other}")),
        }
    }
    fn parse_u64(&mut self) -> Result<u64, String> {
        match self.bump() {
            (Tok::Int(n), sp) => u64::try_from(n).map_err(|_| format!("integer out of range for u64 at {sp}")),
            (other, _) => Err(format!("expected integer, found {other}")),
        }
    }
    fn parse_const(&mut self) -> Result<Item, String> {
        let sp = self.span();
        self.expect(&Tok::KwConst)?;

        let (name, _) = self.expect_ident()?;
        let ty = self.parse_type()?;

        self.expect(&Tok::Assign)?;

        let value = if self.at(&Tok::LBrace) {
            self.bump();
            self.skip_newlines();
            let mut elems = Vec::new();
            while !self.at(&Tok::RBrace) {
                elems.push(self.parse_expr()?);
                self.skip_newlines();
                if self.eat(&Tok::Comma) {
                    self.skip_newlines();
                    continue;
                }
                break;
            }
            self.expect(&Tok::RBrace)?;
            Expr::ArrayLit { elems, span: sp }
        } else {
            self.parse_expr()?
        };

        self.end_stmt()?;
        
        Ok(Item::Const {
            name,
            ty,
            value,
            span: sp,
        })
    }
    fn parse_struct(&mut self) -> Result<Item, String> {
        todo!("parse_struct")
    }
    fn parse_opaque(&mut self, _attrs: Vec<Attr>) -> Result<Item, String> {
        let sp = self.span();

        if !attrs.is_empty() {
            return Err(format!("attributes are not valid on an opaque declaration at {sp}"));
        }

        self.expect(&Tok::KwOpaque)?;
        let (name, _) = self.expect_ident()?;
        self.end_stmt()?;
        self.opaque_names.push(name.clone());

        Ok(Item::Opaque { name, span: sp })
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
