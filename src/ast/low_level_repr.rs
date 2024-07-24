use super::common::{self, PathUL};
use super::operators::{TBinop, TUnaop};

#[derive(Debug, Clone)]
pub struct File {
    pub funs: Vec<DeclFun>,
}

#[derive(Debug, Clone)]
pub struct DeclFun {
    pub name: common::PathUL<()>,
    pub args: Vec<(usize, usize)>, /* (id, size) */
    pub output: usize,
    pub content: Bloc,
}

#[derive(Debug, Clone)]
pub struct Bloc {
    pub content: Vec<Instr>,
    pub last_type: super::typed_rust::PostType,
}

#[derive(Debug, Clone)]
pub enum Instr {
    Expr(common::ComputedValue, Expr),
    Binding(usize, Expr),
}

#[derive(Clone)]
pub struct Expr {
    pub content: Box<ExprInner>,
    pub loc: common::Location,
    pub typed: super::typed_rust::PostType,
    pub size: usize,
}

impl Expr {
    pub fn to_ref(self, pointer_size: usize) -> Self {
        Self {
            typed: self.typed.clone().to_ref(false),
            loc: self.loc,
            content: Box::new(ExprInner::Ref(self)),
            size: pointer_size,
        }
    }

    pub fn new_usize(s: u64) -> Self {
        Self {
            content: Box::new(ExprInner::Value(Value::UInt(s, common::Sizes::SUsize))),
            loc: common::Location::default(),
            typed: super::typed_rust::PostType::usize(),
            size: 8,
        }
    }

    pub fn get_var(&self) -> Option<usize> {
        match &*self.content {
            ExprInner::VarId(id) => Some(*id),
            _ => None,
        }
    }

    pub fn get_ref_var(&self) -> Option<usize> {
        match &*self.content {
            ExprInner::Ref(expr) => expr.get_var(),
            _ => None,
        }
    }

    pub fn is_ref_var(&self) -> bool {
        self.get_ref_var().is_some()
    }
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Expr").field("c", &self.content).finish()
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Value {
    Bool(bool),
    SInt(i64, common::Sizes),
    UInt(u64, common::Sizes),
}

impl Value {
    pub fn imm(&self) -> i64 {
        match self {
            Self::Bool(_) => panic!("ICE"),
            Self::SInt(i, _) => *i,
            Self::UInt(u, _) => *u as i64,
        }
    }

    pub fn size(&self) -> common::Sizes {
        match self {
            Self::Bool(_) => common::Sizes::S8,
            Self::SInt(_, s) | Self::UInt(_, s) => *s,
        }
    }

    pub fn valid(&self) -> bool {
        match self {
            Self::Bool(_) => true,
            Self::SInt(i, s) => i.abs() < 1 << (s.max_imm_size() - 1),
            Self::UInt(i, s) => *i < 1 << s.max_imm_size(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Pos {
    Left,
    Right,
}

impl Pos {
    pub fn is_left(&self) -> bool {
        match self {
            Self::Left => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnaOp {
    Unary(TUnaop),
    Binary(TBinop, Value, Pos),
}

#[derive(Debug, Clone)]
pub enum ExprInner {
    BinOp(TBinop, Expr, Expr),
    Bloc(Bloc),
    BuildStruct(usize /* size */, Vec<(usize, Expr)>),
    Coercion(Expr, common::BuiltinType, common::BuiltinType),
    Constant(PathUL<()>),
    Deref(Expr),
    FunCall(PathUL<()>, Vec<Expr>),
    FunCallVar(usize, Vec<Expr>),
    FunVar(PathUL<()>),
    If(Expr, Bloc, Bloc),
    /// Path represent the name of the constant containing the string
    Print(PathUL<()>),
    Proj(Expr, usize),
    Ref(Expr),
    Return(Option<Expr>),
    Set(usize, Expr, Expr),
    Tuple(usize, Vec<Expr>),
    UnaOp(UnaOp, Expr),
    Value(Value),
    VarId(usize),
    While(Expr, Bloc),
}
