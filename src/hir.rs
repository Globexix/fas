//! High level IR

#![allow(dead_code)]

use crate::ast::{Attr, BinOp, CastKind, IntKind, Linkage, Param, Span, Ty, UnOp};

#[derive(Clone, Debug)]
pub enum TExpr {
    Int {
        val: u128,
        ty: Ty,
        span: Span,
    },
    IntLit {
        val: i128,
        span: Span,
    },
    Bool {
        val: bool,
        span: Span,
    },
    NullPtr {
        ty: Ty,
        span: Span,
    },
    NullLit {
        span: Span,
    },
    StrLit {
        index: usize,
        span: Span,
    },
    Ident {
        name: String,
        ty: Ty,
        span: Span,
    },
    Unary {
        op: UnOp,
        e: Box<TExpr>,
        ty: Ty,
        span: Span,
    },
    Binary {
        op: BinOp,
        l: Box<TExpr>,
        r: Box<TExpr>,
        ty: Ty,
        span: Span,
    },
    Call {
        target: CallTarget,
        args: Vec<TExpr>,
        ret: Ty,
        span: Span,
    },
    Cast {
        kind: CastKind,
        e: Box<TExpr>,
        ty: Ty,
        span: Span,
    },
    Index {
        base: Box<TExpr>,
        idx: Box<TExpr>,
        ty: Ty,
        span: Span,
    },
    Field {
        base: Box<TExpr>,
        name: String,
        ty: Ty,
        offset: u64,
        span: Span,
    },
    Deref {
        e: Box<TExpr>,
        ty: Ty,
        span: Span,
    },
    AddrOf {
        e: Box<TExpr>,
        ty: Ty,
        span: Span,
    },
    PtrAdd {
        bytes: bool,
        ptr: Box<TExpr>,
        off: Box<TExpr>,
        ty: Ty,
        span: Span,
    },
    SizeOf {
        ty: Ty,
        size: u64,
        span: Span,
    },
    AlignOf {
        ty: Ty,
        align: u64,
        span: Span,
    },
    OffsetOf {
        ty: Ty,
        field: String,
        offset: u64,
        span: Span,
    },
    Splat {
        e: Box<TExpr>,
        ty: Ty,
        span: Span,
    },
    Ternary {
        cond: Box<TExpr>,
        then: Box<TExpr>,
        els: Box<TExpr>,
        ty: Ty,
        span: Span,
    },
    ConstArr {
        name: String,
        ty: Ty,
        span: Span,
    },
    StructLit {
        name: String,
        elems: Vec<TExpr>,
        ty: Ty,
        span: Span,
    },
}

#[derive(Clone, Debug)]
pub enum CallTarget {
    User(String),
    Builtin(Builtin),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Builtin {
    Shl,
    Lshr,
    Ashr,
    Rotl,
    Rotr,
    Popcount,
    Ctz,
    Clz,
}

#[derive(Clone, Debug)]
pub enum TStmt {
    Let {
        name: String,
        ty: Ty,
        init: Option<Box<TExpr>>,
        span: Span,
    },
    Assign {
        target: TAssignTarget,
        value: Box<TExpr>,
        span: Span,
    },
    Return(Option<Box<TExpr>>, Span),
    If {
        cond: Box<TExpr>,
        then: Vec<TStmt>,
        els: Option<Vec<TStmt>>,
        span: Span,
    },
    While {
        cond: Box<TExpr>,
        body: Vec<TStmt>,
        span: Span,
    },
    Break(Span),
    Continue(Span),
    Defer(Vec<TStmt>, Span),
    Expr(Box<TExpr>, Span),
    Block(Vec<TStmt>),
    For {
        init: Option<Box<TStmt>>,
        cond: Option<Box<TExpr>>,
        step: Option<Box<TStmt>>,
        body: Vec<TStmt>,
        span: Span,
    },
}

#[derive(Clone, Debug)]
pub enum TAssignTarget {
    Ident(String),
    Deref(Box<TExpr>),
    Index { base: Box<TExpr>, idx: Box<TExpr> },
    Field { base: Box<TExpr>, name: String, offset: u64 },
}

pub type StructDecl = (String, Vec<(String, Ty)>, Option<u32>);

pub fn layout(structs: &[StructDef], ty: &Ty) -> Result<(u64, u64), String> {
    match ty {
        Ty::Bool => Ok((1, 1)),
        Ty::Int(IntKind::U8 | IntKind::I8) => Ok((1, 1)),
        Ty::Int(IntKind::U16 | IntKind::I16) => Ok((2, 2)),
        Ty::Int(IntKind::U32 | IntKind::I32) => Ok((4, 4)),
        Ty::Int(
            IntKind::U64 | IntKind::I64 | IntKind::Usize | IntKind::Isize,
        ) => Ok((8, 8)),
        Ty::Ptr(_) => Ok((8, 8)),
        Ty::Array(n, elem) => {
            let (es, ea) = layout(structs, elem)?;
            let sz = n
                .checked_mul(es)
                .ok_or_else(|| format!("array size {n} x {es} overflows"))?;
            Ok((sz, ea))
        }
        Ty::Vec(n, elem) => {
            let (es, ea) = layout(structs, elem)?;
            let sz = n
                .checked_mul(es)
                .ok_or_else(|| format!("vector size {n} x {es} overflows"))?;
            Ok((sz, ea))
        }
        Ty::Struct(name) => {
            let s = structs
                .iter()
                .find(|s| &s.name == name)
                .ok_or_else(|| format!("unknown struct `{name}`"))?;
            Ok((s.size, s.align))
        }
        Ty::Opaque(name) => Err(format!("opaque type `{name}` has no known layout")),
        Ty::Void => Err("void has no object layout".into()),
    }
}

pub fn compute_struct_layout(
    name: &str,
    all_structs: &[StructDecl],
) -> Result<(Vec<FieldDef>, u64, u64), String> {
    fn resolve(
        name: &str,
        all: &[StructDecl],
        memo: &mut std::collections::HashMap<String, (u64, u64)>,
        visiting: &mut std::collections::HashSet<String>,
    ) -> Result<(u64, u64), String> {
        if let Some(v) = memo.get(name) {
            return Ok(*v);
        }
        if !visiting.insert(name.to_string()) {
            return Err(format!(
                "recursive struct layout involving `{name}` (by-value self-reference)"
            ));
        }
        let result = (|| {
            let (_, fields, align) = all
                .iter()
                .find(|(n, _, _)| n == name)
                .ok_or_else(|| format!("unknown struct `{name}`"))?;
            let mut offset = 0u64;
            let mut max_align = 1u64;
            for (_, fty) in fields {
                let (fsize, falign) = type_layout(fty, all, memo, visiting)?;
                max_align = max_align.max(falign);
                offset = round_up(offset, falign).checked_add(fsize)
                    .ok_or_else(|| format!("struct `{name}` size overflows"))?;
            }
            let align = align.map(|a| a as u64).unwrap_or(max_align).max(max_align);
            let size = round_up(offset, align);
            Ok((size, align))
        })();
        visiting.remove(name);
        if let Ok((size, align)) = result {
            memo.insert(name.to_string(), (size, align));
        }
        result
    }

    fn type_layout(
        ty: &Ty,
        all: &[StructDecl],
        memo: &mut std::collections::HashMap<String, (u64, u64)>,
        visiting: &mut std::collections::HashSet<String>,
    ) -> Result<(u64, u64), String> {
        match ty {
            Ty::Bool => Ok((1, 1)),
            Ty::Int(k) => Ok(match k {
                IntKind::U8 | IntKind::I8 => (1, 1),
                IntKind::U16 | IntKind::I16 => (2, 2),
                IntKind::U32 | IntKind::I32 => (4, 4),
                IntKind::U64 | IntKind::I64 | IntKind::Usize | IntKind::Isize => (8, 8),
            }),
            Ty::Ptr(_) => Ok((8, 8)),
            Ty::Array(n, e) => {
                let (es, ea) = type_layout(e, all, memo, visiting)?;
                Ok((n.checked_mul(es).ok_or_else(|| format!("array size {n} x {es} overflows"))?, ea))
            }
            Ty::Vec(n, e) => {
                let (es, ea) = type_layout(e, all, memo, visiting)?;
                Ok((n.checked_mul(es).ok_or_else(|| format!("vector size {n} x {es} overflows"))?, ea))
            }
            Ty::Struct(s) => resolve(s, all, memo, visiting),
            Ty::Opaque(name) => Err(format!("opaque type `{name}` cannot be used by value")),
            Ty::Void => Err("void cannot be used as stored object type".into()),
        }
    }

    let mut memo = std::collections::HashMap::new();
    let mut visiting = std::collections::HashSet::new();
    resolve(name, all_structs, &mut memo, &mut visiting)?;

    let (_, fields, _) = all_structs
        .iter()
        .find(|(n, _, _)| n == name)
        .expect("resolved");
    let mut fields_out = Vec::new();
    let mut offset = 0u64;
    for (fname, fty) in fields {
        let (fsize, falign) = type_layout(fty, all_structs, &mut memo, &mut visiting)?;
        offset = round_up(offset, falign);
        fields_out.push(FieldDef {
            name: fname.clone(),
            ty: fty.clone(),
            offset,
        });
        offset = offset.checked_add(fsize).ok_or_else(|| format!("struct `{name}` size overflows"))?;
    }
    let (size, align) = *memo.get(name).expect("resolved");
    Ok((fields_out, size, align))
}

fn round_up(x: u64, align: u64) -> u64 {
    if align == 0 {
        return x;
    }
    (x + align - 1) & !(align - 1)
}

pub fn is_addressable(&self) -> bool {
    match self {
        TExpr::Ident { .. } => true,
        TExpr::ConstArr { .. } => true,
        TExpr::Deref { .. } => true,
        TExpr::Index { base, .. } => match base.ty() {
            Ty::Ptr(..) => true,
            Ty::Array(..) => base.is_addressable(),
            Ty::Vec(..) => false,
            _ => false,
        },
        TExpr::Field { base, .. } => base.is_addressable(),
        _ => false,
    }
}

pub fn is_assignable(&self) -> bool {
    match self {
        TExpr::Ident { .. } => true,
        TExpr::ConstArr { .. } => true,
        TExpr::Deref { .. } => true,
        TExpr::Index { base, .. } => match base.ty() {
            Ty::Ptr(..) => true,
            Ty::Array(..) | Ty::Vec(..) => base.is_addressable(),
            _ => false,
        },
        TExpr::Field { base, .. } => base.is_addressable(),
        _ => false,
    }
}

#[derive(Clone, Debug)]
pub struct Program {
    pub structs: Vec<StructDef>,
    pub consts: Vec<ConstDef>,
    pub const_arrs: Vec<ConstArrDef>,
    pub funcs: Vec<FuncDef>,
    pub strings: Vec<String>,
}

#[derive(Clone, Debug)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<FieldDef>,
    pub size: u64,
    pub align: u64,
}

#[derive(Clone, Debug)]
pub struct FieldDef {
    pub name: String,
    pub ty: Ty,
    pub offset: u64,
}

#[derive(Clone, Debug)]
pub struct ConstDef {
    pub name: String,
    pub ty: Ty,
    pub bits: u128,
}

#[derive(Clone, Debug)]
pub struct ConstArrDef {
    pub name: String,
    pub ty: Ty,
    pub elems: Vec<u128>,
}

#[derive(Clone, Debug)]
pub struct FuncDef {
    pub name: String,
    pub params: Vec<Param>,
    pub ret: Ty,
    pub body: Vec<TStmt>,
    pub attrs: Vec<Attr>,
    pub linkage: Linkage,
    pub variadic: bool,
    pub asm_body: Option<String>,
}