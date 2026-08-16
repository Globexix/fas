//! Abstract suntax tree for fas.

#![allow(dead_code)]

pub use crate::lexer::Span;

#[derive(Clone, Debug, PartialEq)]
pub enum Ty {
    Bool,
    Int(IntKind),
    Ptr(Box<Ty>),
    Array(u64, Box<Ty>),
    Vec(u64, Box<Ty>),
    Struct(String),
    Opaque(String),
    Void,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IntKind {
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    Usize,
    Isize,
}

impl IntKind {
    pub fn bits(self) -> u8 {
        match self {
            IntKind::U8 | IntKind::I8 => 8,
            IntKind::U16 | IntKind::I16 => 16,
            IntKind::U32 | IntKind::I32 => 32,
            IntKind::U64 | IntKind::I64 | IntKind::Usize | IntKind::Isize => 64,
        }
    }

    pub fn signed(self) -> bool {
        matches!(
            self,
            IntKind::I8 | IntKind::I16 | IntKind::I32 | IntKind::I64 | IntKind::Isize
        )
    }
}

#[derive(Clone, Debug)]
pub enum Expr {
    Int(i128, Span),
    Bool(bool, Span),
    Null(Span),
    Str(String, Span),
    Ident(String, Span),
    Unary {
        op: UnOp,
        e: Box<Expr>,
        span: Span,
    },
    Binary {
        op: BinOp,
        l: Box<Expr>,
        r: Box<Expr>,
        span: Span,
    },
    Call {
        f: Box<Expr>,
        args: Vec<Expr>,
        span: Span,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnOp {
    Neg,
    Not,
    BitNot,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    BitAnd,
    BitOr,
    BitXor,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn integer_kinds_report_bit_width_and_signedness() {
        assert_eq!(IntKind::U8.bits(), 8);
        assert_eq!(IntKind::Usize.bits(), 64);
        assert!(!IntKind::U64.signed());
        assert!(IntKind::Isize.signed());
    }

    #[test]
    fn builds_a_binary_expression_tree() {
        let e = Expr::Binary {
            op: BinOp::Add,
            l: Box::new(Expr::Int(1, Span::new(1, 1))),
            r: Box::new(Expr::Ident(String::from("x"), Span::new(1, 5))),
            span: Span::new(1, 1),
        };
        assert!(matches!(e, Expr::Binary { op: BinOp::Add, .. }));
    }

}