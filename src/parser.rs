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

fn extract_asm_bodies(src: &str) -> Result<(String, Vec<(String, String)>), String> {
    let chars: Vec<char> = src.chars().collect();
    let mut out = String::new();
    let mut bodies: Vec<(String, String)> = Vec::new();
    let mut i = 0;

    while i < chars.len() {
        if chars[i] == '/' && i + 1 < chars.len() && chars[i + 1] == '/' {
            while i < chars.len() && chars[i] != '\n' {
                out.push(chars[i]);
                i += 1;
            }
            continue;
        }
        if chars[i] == '/' && i + 1 < chars.len() && chars[i + 1] == '*' {
            let start = i;
            i += 2;
            while i + 1 < chars.len() && !(chars[i] == '*' && chars[i + 1] == '/') {
                i += 1;
            }
            i += 2;
            for c in &chars[start..i] {
                out.push(*c);
            }
            continue;
        }

        if chars[i] == 'a' && ident_at(&chars, i, "asm") {
            let mut j = i + 3;
            skip_ws_comments(&chars, &mut j);
            if ident_at(&chars, j, "fn") {
                let mut k = j + 2;
                skip_ws_comments(&chars, &mut k);
                let name_start = k;
                while k < chars.len() && (chars[k].is_ascii_alphanumeric() || chars[k] == '_') {
                    k += 1;
                }
                let name: String = chars[name_start..k].iter().collect();
                let mut l = k;
                while l < chars.len() && chars[l] != '{' {
                    l += 1;
                }
                if l >= chars.len() {
                    return Err(format!("asm fn `{name}` has no body"));
                }
                let body_start = l + 1;
                let mut depth = 1;
                let mut m = body_start;
                while m < chars.len() && depth > 0 {
                    match chars[m] {
                        '{' => depth += 1,
                        '}' => depth -= 1,
                        _ => {}
                    }
                    m += 1;
                }
                if depth != 0 {
                    return Err(format!("unterminated asm body for `{name}`"));
                }
                let body: String = chars[body_start..m - 1].iter().collect();
                bodies.push((name.clone(), body));
                for c in &chars[i..l] {
                    out.push(*c);
                }
                out.push_str("{}");
                i = m;
                continue;
            }
        }
        out.push(chars[i]);
        i += 1;
    }
    Ok((out, bodies))
}

fn ident_at(chars: &[char], i: usize, s: &str) -> bool {
    let sc: Vec<char> = s.chars().collect();
    if i + sc.len() > chars.len() {
        return false;
    }
    chars[i..i + sc.len()] == sc[..]
        && (i + sc.len() == chars.len()
            || !(chars[i + sc.len()].is_ascii_alphanumeric() || chars[i + sc.len()] == '_'))
}

fn skip_ws_comments(chars: &[char], i: &mut usize) {
    loop {
        while *i < chars.len() && (chars[*i] == ' ' || chars[*i] == '\t' || chars[*i] == '\n' || chars[*i] == '\r') {
            *i += 1;
        }
        if *i + 1 < chars.len() && chars[*i] == '/' && chars[*i + 1] == '/' {
            while *i < chars.len() && chars[*i] != '\n' {
                *i += 1;
            }
            continue;
        }
        if *i + 1 < chars.len() && chars[*i] == '/' && chars[*i + 1] == '*' {
            *i += 2;
            while *i + 1 < chars.len() && !(chars[*i] == '*' && chars[*i + 1] == '/') {
                *i += 1;
            }
            *i += 2;
            continue;
        }
        break;
    }
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
        let sp = self.span();
        self.expect(&Tok::KwStruct)?;
        let (name, _) = self.expect_ident()?;
        let mut align = None;
        if self.at(&Tok::At) {
            let a = self.parse_attr()?;
            if let Attr::Align(n) = a {
                align = Some(n);
            } else {
                return self.err("only @align is valid on a struct");
            }
        }
        self.expect(&Tok::LBrace)?;
        self.skip_newlines();
        let mut fields = Vec::new();
        while !self.at(&Tok::RBrace) {
            let (fname, fsp) = self.expect_ident()?;
            let fty = self.parse_type()?;
            fields.push(Field {
                name: fname,
                ty: fty,
                span: fsp,
            });
            if self.eat(&Tok::Comma) {
                self.skip_newlines();
            } else {
                self.skip_newlines();
                if self.at(&Tok::RBrace) {
                    break;
                }
                if self.at(&Tok::Eof) {
                    return self.err("unterminated struct body");
                }
            }
        }
        self.expect(&Tok::RBrace)?;
        self.end_stmt()?;
        self.struct_names.push(name.clone());
        Ok(Item::Struct {
            name,
            fields,
            align,
            span: sp,
        })
    }
    fn parse_opaque(&mut self, attrs: Vec<Attr>) -> Result<Item, String> {
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
    fn parse_fn(&mut self, attrs: Vec<Attr>) -> Result<Item, String> {
        let sp = self.span();
        self.expect(&Tok::KwFn)?;
        let (name, _) = self.expect_ident()?;
        let const_params = self.parse_const_params()?;
        let (params, ret, variadic) = self.parse_signature(false)?;
        let body = self.parse_block()?;
        Ok(Item::Func {
            name,
            params,
            ret,
            body: FuncBody::Statements(body),
            attrs,
            linkage: Linkage::Internal,
            variadic,
            const_params,
            span: sp,
        })
    }
    fn parse_const_params(&mut self) -> Result<Vec<ConstParam>, String> {
        let mut cps = Vec::new();
        if !self.at(&Tok::LBracket) {
            return Ok(cps);
        }
        self.bump();
        self.skip_newlines();
        loop {
            let (cname, csp) = self.expect_ident()?;
            self.expect(&Tok::KwConst)?;
            let cty = self.parse_type()?;
            cps.push(ConstParam {
                name: cname,
                ty: cty,
                span: csp,
            });
            if self.eat(&Tok::Comma) {
                self.skip_newlines();
                continue;
            }
            break;
        }
        self.expect(&Tok::RBracket)?;
        Ok(cps)
    }
    fn parse_extern_block(&mut self, attrs: Vec<Attr>) -> Result<Vec<Item>, String> {
        let sp = self.span();
        if !attrs.is_empty() {
            return Err(format!("attributes are not valid on an extern block at {sp}"));
        }
        self.expect(&Tok::KwExtern)?;
        let abi = self.parse_string()?;
        if abi != "C" {
            return Err(format!("unsupported ABI `\"{abi}\"` at {sp} (only \"C\")"));
        }
        self.skip_newlines();
        self.expect(&Tok::LBrace).map_err(|_| {
            format!("expected `{{` after `extern \"C\"` at {sp}; per-function extern syntax is not supported")
        })?;
        self.skip_newlines();
        let mut funcs = Vec::new();
        while !self.at(&Tok::RBrace) {
            if self.at(&Tok::Eof) {
                return Err(format!("unterminated extern \"C\" block at {sp}"));
            }
            if !self.at(&Tok::KwFn) {
                return self.err("extern \"C\" blocks may contain only function declarations");
            }
            funcs.push(self.parse_extern_decl()?);
            self.skip_newlines();
        }
        self.expect(&Tok::RBrace)?;
        self.end_stmt()?;
        Ok(funcs)
    }
    fn parse_extern_decl(&mut self) -> Result<Item, String> {
        let sp = self.span();
        self.expect(&Tok::KwFn)?;
        let (name, _) = self.expect_ident()?;
        if self.at(&Tok::LBracket) {
            return Err(format!("extern function `{name}` cannot be const-generic at {sp}"));
        }
        let (params, ret, variadic) = self.parse_signature(true)?;
        if self.at(&Tok::LBrace) {
            return Err(format!("extern function `{name}` cannot have a body at {sp}"));
        }
        self.end_stmt()?;
        Ok(Item::Func {
            name,
            params,
            ret,
            body: FuncBody::Statements(Vec::new()),
            attrs: Vec::new(),
            linkage: Linkage::ExternalC,
            variadic,
            const_params: Vec::new(),
            span: sp,
        })
    }
    fn parse_asm_fn(&mut self, attrs: Vec<Attr>) -> Result<Item, String> {
        let sp = self.span();
        self.expect(&Tok::KwAsm)?;
        self.expect(&Tok::KwFn)?;
        let (name, _) = self.expect_ident()?;
        if self.at(&Tok::LBracket) {
            return Err(format!("asm function `{name}` cannot be const-generic at {sp}"));
        }
        let (params, ret, variadic) = self.parse_signature(false)?;
        self.skip_newlines();
        self.expect(&Tok::LBrace)?;
        self.expect(&Tok::RBrace)?;
        self.end_stmt()?;
        let raw = self
            .asm_bodies
            .iter()
            .find(|(n, _)| n == &name)
            .map(|(_, b)| b.clone())
            .unwrap_or_default();
        Ok(Item::Func {
            name,
            params,
            ret,
            body: FuncBody::Asm(raw),
            attrs,
            linkage: Linkage::Internal,
            variadic,
            const_params: Vec::new(),
            span: sp,
        })
    }
    fn parse_signature(&mut self, allow_variadic: bool) -> Result<(Vec<Param>, Ty, bool), String> {
        self.expect(&Tok::LParen)?;
        self.skip_newlines();
        let mut params = Vec::new();
        let mut variadic = false;
        while !self.at(&Tok::RParen) {
            if self.eat(&Tok::DotDotDot) {
                if !allow_variadic {
                    return self.err("`...` is legal only in an `extern \"C\"` declaration");
                }
                if params.is_empty() {
                    return self.err("a C variadic declaration needs at least one fixed parameter");
                }
                variadic = true;
                if !self.at(&Tok::RParen) {
                    return self.err("`...` must be the final item in a parameter list");
                }
                break;
            }
            let (name, psp) = self.expect_ident()?;
            let mut noalias = false;
            let mut align = None;
            loop {
                if self.at_ident("noalias") {
                    self.bump();
                    noalias = true;
                } else if self.at_ident("aligned") {
                    self.bump();
                    self.expect(&Tok::LBracket)?;
                    align = Some(self.parse_u32()?);
                    self.expect(&Tok::RBracket)?;
                } else {
                    break;
                }
            }
            let ty = self.parse_type()?;
            params.push(Param {
                name,
                ty,
                noalias,
                align,
                span: psp,
            });
            if self.eat(&Tok::Comma) {
                self.skip_newlines();
            } else {
                self.skip_newlines();
                break;
            }
        }
        self.expect(&Tok::RParen)?;
        let ret = self.parse_type()?;
        Ok((params, ret, variadic))
    }
    fn parse_block(&mut self) -> Result<Vec<Stmt>, String> {
        self.expect(&Tok::LBrace)?;
        self.depth += 1;
        if self.depth > 128 {
            return Err(format!("block too deeply nested at {}", self.span()));
        }
        self.skip_newlines();
        let mut stmts = Vec::new();
        while !self.at(&Tok::RBrace) {
            if self.at(&Tok::Eof) {
                return self.err("unterminated block");
            }
            stmts.push(self.parse_stmt()?);
            self.skip_newlines();
        }
        self.expect(&Tok::RBrace)?;
        self.depth -= 1;
        Ok(stmts)
    }
    fn starts_type(&self, t: &Tok) -> bool {
        match t {
            Tok::Ident(s) => {
                matches!(
                    s.as_str(),
                    "bool" | "u8" | "u16" | "u32" | "u64" | "i8" | "i16" | "i32" | "i64"
                    | "usize" | "isize" | "ptr" | "arr" | "vec" | "void"
                ) || self.struct_names.iter().any(|n| n == s) || self.opaque_names.iter().any(|n| n == s)
            }
            _ => false,
        }
    }
    fn parse_type(&mut self) -> Result<Ty, String> {
        match self.peek().clone() {
            Tok::Ident(s) => match s.as_str() {
                "bool" => {
                    self.bump();
                    Ok(Ty::Bool)
                }
                "void" => {
                    self.bump();
                    Ok(Ty::Void)
                }
                "ptr" => {
                    self.bump();
                    self.expect(&Tok::LBracket)?;
                    let inner = self.parse_type()?;
                    self.expect(&Tok::RBracket)?;
                    Ok(Ty::Ptr(Box::new(inner)))
                }
                "arr" => {
                    self.bump();
                    self.expect(&Tok::LBracket)?;
                    let n = self.parse_u64()?;
                    self.expect(&Tok::Comma)?;
                    let inner = self.parse_type()?;
                    self.expect(&Tok::RBracket)?;
                    Ok(Ty::Array(n, Box::new(inner)))
                }
                "vec" => {
                    self.bump();
                    self.expect(&Tok::LBracket)?;
                    let n = self.parse_u64()?;
                    self.expect(&Tok::Comma)?;
                    let inner = self.parse_type()?;
                    self.expect(&Tok::RBracket)?;
                    if n == 0 {
                        return Err("vector type must have at least one lane".into());
                    }
                    if !matches!(inner, Ty::Bool | Ty::Int(_) | Ty::Ptr(_)) {
                        return Err(
                            "vector lane must be a scalar (bool, integer, or pointer)".into(),
                        );
                    }
                    if n > 16_777_216 {
                        return Err("vector type has too many lanes".into());
                    }
                    Ok(Ty::Vec(n, Box::new(inner)))
                }
                "u8" | "u16" | "u32" | "u64" | "i8" | "i16" | "i32" | "i64" | "usize"
                | "isize" => {
                    self.bump();
                    let kind = match s.as_str() {
                        "u8" => IntKind::U8,
                        "u16" => IntKind::U16,
                        "u32" => IntKind::U32,
                        "u64" => IntKind::U64,
                        "i8" => IntKind::I8,
                        "i16" => IntKind::I16,
                        "i32" => IntKind::I32,
                        "i64" => IntKind::I64,
                        "usize" => IntKind::Usize,
                        "isize" => IntKind::Isize,
                        _ => unreachable!(),
                    };
                    Ok(Ty::Int(kind))
                }
                other => {
                    self.bump();
                    if self.opaque_names.iter().any(|n| n == other) {
                        Ok(Ty::Opaque(other.to_string()))
                    } else {
                        Ok(Ty::Struct(other.to_string()))
                    }
                }
            },
            other => self.err(&format!("expected a type, found {other}")),
        }
    }
    fn parse_stmt(&mut self) -> Result<Stmt, String> {
        match self.peek().clone() {
            Tok::KwReturn => {
                let sp = self.span();
                self.bump();
                let expr = if self.at(&Tok::Newline)
                    || self.at(&Tok::Semi)
                    || self.at(&Tok::RBrace)
                {
                    None
                } else {
                    Some(self.parse_expr()?)
                };
                self.end_stmt()?;
                Ok(Stmt::Return(expr, sp))
            }
            Tok::KwIf => self.parse_if(),
            Tok::KwWhile => self.parse_while(),
            Tok::KwFor => self.parse_for(),
            Tok::KwSwitch => self.parse_switch(),
            Tok::KwBreak => {
                let sp = self.span();
                self.bump();
                self.end_stmt()?;
                Ok(Stmt::Break(sp))
            }
            Tok::KwContinue => {
                let sp = self.span();
                self.bump();
                self.end_stmt()?;
                Ok(Stmt::Continue(sp))
            }
            Tok::KwDefer => {
                let sp = self.span();
                self.bump();
                let body = self.parse_block()?;
                Ok(Stmt::Defer(body, sp))
            }
            Tok::LBrace => {
                let sp = self.span();
                let body = self.parse_block()?;
                Ok(Stmt::Block(body, sp))
            }
            Tok::Ident(_) => {
                if self.looks_like_decl() {
                    self.parse_decl()
                } else {
                    self.parse_assign_or_expr_stmt()
                }
            }
            _ => self.parse_assign_or_expr_stmt(),
        }
    }
    fn looks_like_decl(&self) -> bool {
        self.starts_type(self.peek_n(1))
    }
    fn parse_decl(&mut self) -> Result<Stmt, String> {
        let sp = self.span();
        let (name, _) = self.expect_ident()?;
        let ty = self.parse_type()?;
        
        let init = if self.eat(&Tok::Assign) {
            Some(self.parse_expr()?)
        } else {
            None
        };

        self.end_stmt()?;
        Ok(Stmt::Let {
            name,
            ty: Some(ty),
            init,
            span: sp,
        })
    }
    fn parse_assign_or_expr_stmt(&mut self) -> Result<Stmt, String> {
        let sp = self.span();
        let lhs = self.parse_expr()?;

        let compound = match self.peek() {
            Tok::PlusEq => Some(BinOp::Add),
            Tok::MinusEq => Some(BinOp::Sub),
            Tok::StarEq => Some(BinOp::Mul),
            Tok::SlashEq => Some(BinOp::Div),
            Tok::PercentEq => Some(BinOp::Rem),
            Tok::AmpEq => Some(BinOp::BitAnd),
            Tok::PipeEq => Some(BinOp::BitOr),
            Tok::CaretEq => Some(BinOp::BitXor),
            _ => None,
        };

        if let Some(op) = compound {
            self.bump();
            let rhs = self.parse_expr()?;
            self.end_stmt()?;
            let lhs_for_value = lhs.clone();
            let target = match lhs {
                Expr::Ident(_, _) => AssignTarget::Ident(ident_of(&lhs)),
                Expr::Deref { e, .. } => AssignTarget::Deref(e),
                Expr::Index { base, idx, .. } => AssignTarget::Index { base, idx },
                Expr::Field { base, name, .. } => AssignTarget::Field { base, name },
                other => {
                    return Err(format!(
                        "invalid assignment target at {}",
                        other.span()
                    ))
                }
            };

            let value = Expr::Binary {
                op,
                l: Box::new(lhs_for_value),
                r: Box::new(rhs),
                span: sp,
            };

            return Ok(Stmt::Assign {
                target,
                value,
                span: sp,
            });
        }

        if self.eat(&Tok::Assign) {
            let value = self.parse_expr()?;
            self.end_stmt()?;

            let target = match lhs {
                Expr::Ident(_, _) => AssignTarget::Ident(ident_of(&lhs)),
                Expr::Deref { e, .. } => AssignTarget::Deref(e),
                Expr::Index { base, idx, .. } => AssignTarget::Index { base, idx },
                Expr::Field { base, name, .. } => AssignTarget::Field { base, name },
                other => {
                    return Err(format!(
                        "invalid assignment target at {}",
                        other.span()
                    ))
                }
            };

            return Ok(Stmt::Assign {
                target,
                value,
                span: sp,
            });
        }

        self.end_stmt()?;
        Ok(Stmt::Expr(lhs, sp))
    }
    fn parse_if(&mut self) -> Result<Stmt, String> {
        let sp = self.span();
        self.expect(&Tok::KwIf)?;
        let cond = self.parse_expr()?;
        let then = self.parse_block()?;
        self.skip_newlines();
        let els = if self.eat(&Tok::KwElse) {
            self.skip_newlines();
            if self.at(&Tok::KwIf) {
                Some(vec![self.parse_if()?])
            } else {
                Some(self.parse_block()?)
            }
        } else {
            None
        };
        Ok(Stmt::If {
            cond,
            then,
            els,
            span: sp,
        })
    }
    fn parse_while(&mut self) -> Result<Stmt, String> {
        let sp = self.span();
        self.expect(&Tok::KwWhile)?;
        let cond = self.parse_expr()?;
        let body = self.parse_block()?;
        Ok(Stmt::While {
            cond,
            body,
            span: sp,
        })
    }
    fn parse_for(&mut self) -> Result<Stmt, String> {
        let sp = self.span();
        self.expect(&Tok::KwFor)?;
        let init = if self.at(&Tok::Semi) {
            self.bump();
            None
        } else {
            let c = self.parse_for_clause()?;
            self.expect(&Tok::Semi)?;
            Some(Box::new(c))
        };
        let cond = if self.at(&Tok::Semi) {
            None
        } else {
            Some(self.parse_expr()?)
        };
        self.expect(&Tok::Semi)?;
        let step = if self.at(&Tok::LBrace) {
            None
        } else {
            Some(Box::new(self.parse_for_clause()?))
        };
        let body = self.parse_block()?;
        Ok(Stmt::For {
            init,
            cond,
            step,
            body,
            span: sp,
        })
    }
    fn parse_for_clause(&mut self) -> Result<Stmt, String> {
        let sp = self.span();
        if matches!(self.peek(), Tok::Ident(_)) && self.looks_like_decl() {
            let (name, _) = self.expect_ident()?;
            let ty = self.parse_type()?;
            let init = if self.eat(&Tok::Assign) {
                Some(self.parse_expr()?)
            } else {
                None
            };
            return Ok(Stmt::Let {
                name,
                ty: Some(ty),
                init,
                span: sp,
            });
        }
        let lhs = self.parse_expr()?;
        let compound = match self.peek() {
            Tok::PlusEq => Some(BinOp::Add),
            Tok::MinusEq => Some(BinOp::Sub),
            Tok::StarEq => Some(BinOp::Mul),
            Tok::SlashEq => Some(BinOp::Div),
            Tok::PercentEq => Some(BinOp::Rem),
            Tok::AmpEq => Some(BinOp::BitAnd),
            Tok::PipeEq => Some(BinOp::BitOr),
            Tok::CaretEq => Some(BinOp::BitXor),
            _ => None,
        };
        if let Some(op) = compound {
            self.bump();
            let rhs = self.parse_expr()?;
            let target = assign_target_of(lhs.clone())?;
            return Ok(Stmt::Assign {
                target,
                value: Expr::Binary {
                    op,
                    l: Box::new(lhs),
                    r: Box::new(rhs),
                    span: sp,
                },
                span: sp,
            });
        }
        if self.eat(&Tok::Assign) {
            let value = self.parse_expr()?;
            let target = assign_target_of(lhs)?;
            return Ok(Stmt::Assign {
                target,
                value,
                span: sp,
            });
        }
        Ok(Stmt::Expr(lhs, sp))
    }
    fn parse_switch(&mut self) -> Result<Stmt, String> {
        let sp = self.span();
        self.expect(&Tok::KwSwitch)?;
        let scrutinee = self.parse_expr()?;
        self.skip_newlines();
        self.expect(&Tok::LBrace)?;
        self.skip_newlines();
        let mut arms: Vec<(Expr, Vec<Stmt>)> = Vec::new();
        let mut default: Option<Vec<Stmt>> = None;
        loop {
            self.skip_newlines();
            match self.peek().clone() {
                Tok::KwCase => {
                    self.bump();
                    let val = self.parse_expr()?;
                    self.expect(&Tok::Colon)?;
                    let body = self.parse_case_body()?;
                    arms.push((val, body));
                }
                Tok::KwDefault => {
                    self.bump();
                    self.expect(&Tok::Colon)?;
                    default = Some(self.parse_case_body()?);
                }
                Tok::RBrace => break,
                other => return self.err(&format!("expected `case`, `default`, or `}}`, found {other}")),
            }
        }
        self.expect(&Tok::RBrace)?;
        Ok(Stmt::Switch {
            scrutinee,
            arms,
            default,
            span: sp,
        })
    }
    fn parse_case_body(&mut self) -> Result<Vec<Stmt>, String> {
        self.skip_newlines();
        let mut stmts = Vec::new();
        while !self.at(&Tok::KwCase)
            && !self.at(&Tok::KwDefault)
            && !self.at(&Tok::RBrace)
        {
            if self.at(&Tok::Eof) {
                return self.err("unterminated switch");
            }
            stmts.push(self.parse_stmt()?);
            self.skip_newlines();
        }
        Ok(stmts)
    }
    fn parse_expr(&mut self) -> Result<Expr, String> {
        self.depth += 1;
        if self.depth > 128 {
            return Err(format!("expression too deeply nested at {}", self.span()));
        }
        let r = self.parse_ternary();
        self.depth -= 1;
        r
    }
    fn parse_ternary(&mut self) -> Result<Expr, String> {
        let cond = self.parse_or()?;
        if !self.at(&Tok::Question) {
            return Ok(cond);
        }
        let sp = self.span();
        self.bump();
        let then = self.parse_expr()?;
        self.expect(&Tok::Colon)?;
        let els = self.parse_ternary()?;
        Ok(Expr::Ternary {
            cond: Box::new(cond),
            then: Box::new(then),
            els: Box::new(els),
            span: sp,
        })
    }
    fn parse_or(&mut self) -> Result<Expr, String> {
        let mut lhs = self.parse_and()?;
        while self.at(&Tok::OrOr) {
            let sp = self.span();
            self.bump();
            let rhs = self.parse_and()?;
            lhs = Expr::Binary {
                op: BinOp::Or,
                l: Box::new(lhs),
                r: Box::new(rhs),
                span: sp,
            };
        }
        Ok(lhs)
    }
    fn parse_and(&mut self) -> Result<Expr, String> {
        let mut lhs = self.parse_bitor()?;
        while self.at(&Tok::AndAnd) {
            let sp = self.span();
            self.bump();
            let rhs = self.parse_bitor()?;
            lhs = Expr::Binary {
                op: BinOp::And,
                l: Box::new(lhs),
                r: Box::new(rhs),
                span: sp,
            };
        }
        Ok(lhs)
    }
    fn parse_bitor(&mut self) -> Result<Expr, String> {
        let mut lhs = self.parse_bitxor()?;
        while self.at(&Tok::Pipe) {
            let sp = self.span();
            self.bump();
            let rhs = self.parse_bitxor()?;
            lhs = Expr::Binary {
                op: BinOp::BitOr,
                l: Box::new(lhs),
                r: Box::new(rhs),
                span: sp,
            };
        }
        Ok(lhs)
    }
    fn parse_bitxor(&mut self) -> Result<Expr, String> {
        let mut lhs = self.parse_bitand()?;
        while self.at(&Tok::Caret) {
            let sp = self.span();
            self.bump();
            let rhs = self.parse_bitand()?;
            lhs = Expr::Binary {
                op: BinOp::BitXor,
                l: Box::new(lhs),
                r: Box::new(rhs),
                span: sp,
            };
        }
        Ok(lhs)
    }
    fn parse_bitand(&mut self) -> Result<Expr, String> {
        let mut lhs = self.parse_equality()?;
        while self.at(&Tok::Amp) {
            let sp = self.span();
            self.bump();
            let rhs = self.parse_equality()?;
            lhs = Expr::Binary {
                op: BinOp::BitAnd,
                l: Box::new(lhs),
                r: Box::new(rhs),
                span: sp,
            };
        }
        Ok(lhs)
    }
    fn parse_equality(&mut self) -> Result<Expr, String> {
        let mut lhs = self.parse_relational()?;
        loop {
            let op = match self.peek() {
                Tok::EqEq => BinOp::Eq,
                Tok::NotEq => BinOp::Ne,
                _ => break,
            };
            let sp = self.span();
            self.bump();
            let rhs = self.parse_relational()?;
            lhs = Expr::Binary {
                op,
                l: Box::new(lhs),
                r: Box::new(rhs),
                span: sp,
            };
        }
        Ok(lhs)
    }
    fn parse_relational(&mut self) -> Result<Expr, String> {
        let mut lhs = self.parse_additive()?;
        loop {
            let op = match self.peek() {
                Tok::Lt => BinOp::Lt,
                Tok::Le => BinOp::Le,
                Tok::Gt => BinOp::Gt,
                Tok::Ge => BinOp::Ge,
                _ => break,
            };
            let sp = self.span();
            self.bump();
            let rhs = self.parse_additive()?;
            lhs = Expr::Binary {
                op,
                l: Box::new(lhs),
                r: Box::new(rhs),
                span: sp,
            };
        }
        Ok(lhs)
    }
    fn parse_additive(&mut self) -> Result<Expr, String> {
        let mut lhs = self.parse_multiplicative()?;
        loop {
            let op = match self.peek() {
                Tok::Plus => BinOp::Add,
                Tok::Minus => BinOp::Sub,
                _ => break,
            };
            let sp = self.span();
            self.bump();
            let rhs = self.parse_multiplicative()?;
            lhs = Expr::Binary {
                op,
                l: Box::new(lhs),
                r: Box::new(rhs),
                span: sp,
            };
        }
        Ok(lhs)
    }
    fn parse_multiplicative(&mut self) -> Result<Expr, String> {
        let mut lhs = self.parse_unary()?;
        loop {
            let op = match self.peek() {
                Tok::Star => BinOp::Mul,
                Tok::Slash => BinOp::Div,
                Tok::Percent => BinOp::Rem,
                _ => break,
            };
            let sp = self.span();
            self.bump();
            let rhs = self.parse_unary()?;
            lhs = Expr::Binary {
                op,
                l: Box::new(lhs),
                r: Box::new(rhs),
                span: sp,
            };
        }
        Ok(lhs)
    }
    fn parse_unary(&mut self) -> Result<Expr, String> {
        match self.peek().clone() {
            Tok::Amp => {
                let sp = self.span();
                self.bump();
                let e = self.parse_unary()?;
                Ok(Expr::AddrOf {
                    e: Box::new(e),
                    span: sp,
                })
            }
            Tok::Minus => {
                let sp = self.span();
                self.bump();
                match e {
                    Expr::Int(v, _) => Ok(Expr::Int(-v, sp)),
                    other => Ok(Expr::Unary {
                        op: UnOp::Neg,
                        e: Box::new(other),
                        span: sp,
                    }),
                }
            }
            Tok::Not => {
                let sp = self.span();
                self.bump();
                let e = self.parse_unary()?;
                Ok(Expr::Unary {
                    op: UnOp::Not,
                    e: Box::new(e),
                    span: sp,
                })
            }
            Tok::Tilde => {
                let sp = self.span();
                self.bump();
                let e = self.parse_unary()?;
                Ok(Expr::Unary {
                    op: UnOp::BitNot,
                    e: Box::new(e),
                    span: sp,
                })
            }
            _ => self.parse_postfix(),
        }
    }
    fn parse_postfix(&mut self) -> Result<Expr, String> {
        let mut e = self.parse_primary()?;
        loop {
            match self.peek().clone() {
                Tok::LParen => {
                    let sp = self.span();
                    let args = self.parse_args()?;
                    e = Expr::Call {
                        f: Box::new(e),
                        args,
                        span: sp,
                    };
                }
                Tok::LBracket => {
                    let sp = self.span();
                    self.bump();
                    let first = self.parse_expr()?;
                    if self.eat(&Tok::Comma) {
                        let mut idxs = vec![first];
                        loop {
                            idxs.push(self.parse_expr()?);
                            if !self.eat(&Tok::Comma) {
                                break;
                            }
                        }
                        self.expect(&Tok::RBracket)?;
                        e = Expr::ConstArgs {
                            base: Box::new(e),
                            args: idxs,
                            span: sp,
                        };
                    } else {
                        self.expect(&Tok::RBracket)?;
                        e = Expr::Index {
                            base: Box::new(e),
                            idx: Box::new(first),
                            span: sp,
                        };
                    }
                }
                Tok::Dot => {
                    if matches!(self.peek_n(1), Tok::Star) {
                        let sp = self.span();
                        self.bump();
                        self.bump();
                        e = Expr::Deref {
                            e: Box::new(e),
                            span: sp,
                        };
                    } else {
                        let sp = self.span();
                        self.bump();
                        let (name, _) = self.expect_ident()?;
                        e = Expr::Field {
                            base: Box::new(e),
                            name,
                            span: sp,
                        };
                    }
                }
                Tok::LBrace => {
                    if let Expr::Ident(name, _) = &e {
                        if self.struct_names.iter().any(|n| n == name) {
                            let sp = self.span();
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
                            e = Expr::StructLit {
                                name: name.clone(),
                                elems,
                                span: sp,
                            };
                            continue;
                        }
                    }
                    break;
                }
                _ => break,
            }
        }
        Ok(e)
    }
    fn parse_args(&mut self) -> Result<Vec<Expr>, String> {
        todo!("parse_args")
    }
    fn parse_primary(&mut self) -> Result<Expr, String> {
        todo!("parse_primary")
    }
}

fn ident_of(e: &Expr) -> String {
    match e {
        Expr::Ident(s, _) => s.clone(),
        _ => unreachable!("not an identifier"),
    }
}
fn assign_target_of(e: Expr) -> Result<AssignTarget, String> {
    match e {
        Expr::Ident(_, _) => Ok(AssignTarget::Ident(ident_of(&e))),
        Expr::Deref { e: inner, .. } => Ok(AssignTarget::Deref(inner)),
        Expr::Index { base, idx, .. } => Ok(AssignTarget::Index { base, idx }),
        Expr::Field { base, name, .. } => Ok(AssignTarget::Field { base, name }),
        other => Err(format!("invalid assignment target at {}", other.span())),
    }
}
