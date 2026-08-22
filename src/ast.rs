//! Abstract suntax tree for fas.

#![allow(dead_code)]

pub use crate::lexer::Span;

#[derive(Clone, Debug)]
pub struct Program {
    pub items: Vec<Item>,
}

#[derive(Clone, Debug)]
pub enum Item {
    Const {
        name: String,
        ty: Ty,
        value: Expr,
        span: Span,
    },
    Struct {
        name: String,
        fields: Vec<Field>,
        align: Option<u32>,
        span: Span,
    },
    Opaque {
        name: String,
        span: Span,
    },
    Func {
        name: String,
        params: Vec<Param>,
        ret: Ty,
        body: FuncBody,
        attrs: Vec<Attr>,
        linkage: Linkage,
        variadic: bool,
        const_params: Vec<ConstParam>,
        span: Span,
    },
}

#[derive(Clone, Debug)]
pub struct Field {
    pub name: String,
    pub ty: Ty,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Param {
    pub name: String,
    pub ty: Ty,
    pub noalias: bool,
    pub align: Option<u32>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct ConstParam {
    pub name: String,
    pub ty: Ty,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum FuncBody {
    Statements(Vec<Stmt>),
    Asm(String),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Linkage {
    Internal,
    ExternalC,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Attr {
    Inline,
    NoInline,
    Kernel,
    Optimize,
    Target(String),
    Align(u32),
    ExpectAsm(String),
    ExpectNoCall,
    ExpectStackMax(u64),
}

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
    Cast {
        kind: CastKind,
        ty: Ty,
        e: Box<Expr>,
        span: Span,
    },
    Index {
        base: Box<Expr>,
        idx: Box<Expr>,
        span: Span,
    },
    Field {
        base: Box<Expr>,
        name: String,
        span: Span,
    },
    Deref {
        e: Box<Expr>,
        span: Span,
    },
    AddrOf {
        e: Box<Expr>,
        span: Span,
    },
    PtrAdd {
        bytes: bool,
        ptr: Box<Expr>,
        off: Box<Expr>,
        span: Span,
    },
    SizeOf(Ty, Span),
    AlignOf(Ty, Span),
    OffsetOf {
        ty: Ty,
        field: String,
        span: Span,
    },
    Splat {
        e: Box<Expr>,
        span: Span,
    },
    Ternary {
        cond: Box<Expr>,
        then: Box<Expr>,
        els: Box<Expr>,
        span: Span,
    },
    ArrayLit {
        elems: Vec<Expr>,
        span: Span,
    },
    StructLit {
        name: String,
        elems: Vec<Expr>,
        span: Span,
    },
    ConstArgs {
        base: Box<Expr>,
        args: Vec<Expr>,
        span: Span,
    },
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Int(_, s)
            | Expr::Bool(_, s)
            | Expr::Null(s)
            | Expr::Str(_, s)
            | Expr::Ident(_, s)
            | Expr::Unary { span: s, .. }
            | Expr::Binary { span: s, .. }
            | Expr::Call { span: s, .. }
            | Expr::ConstArgs { span: s, .. }
            | Expr::Cast { span: s, .. }
            | Expr::Index { span: s, .. }
            | Expr::Field { span: s, .. }
            | Expr::Deref { span: s, .. }
            | Expr::AddrOf { span: s, .. }
            | Expr::PtrAdd { span: s, .. }
            | Expr::SizeOf(_, s)
            | Expr::AlignOf(_, s)
            | Expr::OffsetOf { span: s, .. }
            | Expr::Splat { span: s, .. }
            | Expr::Ternary { span: s, .. }
            | Expr::ArrayLit { span: s, .. }
            | Expr::StructLit { span: s, .. } => *s,
        }
    }
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CastKind {
    Zext,
    Sext,
    Trunc,
    Bitcast,
}

#[derive(Clone, Debug)]
pub enum Stmt {
    Let {
        name: String,
        ty: Option<Ty>,
        init: Option<Expr>,
        span: Span,
    },
    Assign {
        target: AssignTarget,
        value: Expr,
        span: Span,
    },
    Return(Option<Expr>, Span),
    If {
        cond: Expr,
        then: Vec<Stmt>,
        els: Option<Vec<Stmt>>,
        span: Span,
    },
    While {
        cond: Expr,
        body: Vec<Stmt>,
        span: Span,
    },
    Break(Span),
    Continue(Span),

    Defer(Vec<Stmt>, Span),
    Expr(Expr, Span),

    Block(Vec<Stmt>, Span),
    For {
        init: Option<Box<Stmt>>,
        cond: Option<Expr>,
        step: Option<Box<Stmt>>,
        body: Vec<Stmt>,
        span: Span,
    },
    /// switch no fallthrough
    Switch {
        scrutinee: Expr,
        arms: Vec<(Expr, Vec<Stmt>)>,
        default: Option<Vec<Stmt>>,
        span: Span,
    },
}

#[derive(Clone, Debug)]
pub enum AssignTarget {
    Ident(String),
    Deref(Box<Expr>),
    Index { base: Box<Expr>, idx: Box<Expr> },
    Field { base: Box<Expr>, name: String },
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

    #[test]
    fn constructs_and_matches_a_struct_literal() {
        let elems = vec![Expr::Int(1, Span::new(1, 1)), Expr::Int(2, Span::new(1, 1))];
        let expression = Expr::StructLit {
            name: String::from("Pair"),
            elems,
            span: Span::new(1, 1),
        };

        match expression {
            Expr::StructLit { name, elems, .. } => {
                assert_eq!(name, "Pair");
                assert_eq!(elems.len(), 2);
            }
            _ => panic!("expected a struct literal"),
        }
    }

    #[test]
    fn constructs_and_matches_for_and_switch_statements() {
        let span = Span::new(1, 1);
        let for_statement = Stmt::For {
            init: Some(Box::new(Stmt::Let {
                name: String::from("i"),
                ty: Some(Ty::Int(IntKind::I32)),
                init: Some(Expr::Int(0, span)),
                span,
            })),
            cond: Some(Expr::Bool(true, span)),
            step: Some(Box::new(Stmt::Assign {
                target: AssignTarget::Ident(String::from("i")),
                value: Expr::Int(1, span),
                span,
            })),
            body: vec![Stmt::Continue(span)],
            span,
        };
        assert!(matches!(
            for_statement,
            Stmt::For {
                init: Some(_),
                cond: Some(_),
                step: Some(_),
                body,
                ..
            } if body.len() == 1
        ));

        let switch_statement = Stmt::Switch {
            scrutinee: Expr::Ident(String::from("value"), span),
            arms: vec![(Expr::Int(0, span), vec![Stmt::Break(span)])],
            default: Some(vec![Stmt::Continue(span)]),
            span,
        };
        assert!(matches!(
            switch_statement,
            Stmt::Switch {
                arms,
                default: Some(_),
                ..
            } if arms.len() == 1
        ));
    }

    #[test]
    fn constructs_and_matches_a_one_item_program() {
        let span = Span::new(1, 1);
        let value = Expr::Int(7, span);
        let item = Item::Const {
            name: String::from("N"),
            ty: Ty::Int(IntKind::U32),
            value,
            span,
        };
        let program = Program { items: vec![item] };

        assert_eq!(program.items.len(), 1);
        assert!(matches!(program.items[0], Item::Const { .. }));
    }

    #[test]
    fn returns_spans_for_expressions() {
        let integer = Expr::Int(5, Span::new(4, 9));
        let struct_literal = Expr::StructLit {
            name: String::from("LM"),
            elems: vec![],
            span: Span::new(1, 3),
        };

        assert_eq!(integer.span(), Span::new(4, 9));
        assert_eq!(struct_literal.span(), Span::new(1, 3));
    }
}
