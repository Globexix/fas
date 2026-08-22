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
