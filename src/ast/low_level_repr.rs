use super::common::{self, PathUL};

#[derive(Debug)]
pub struct File {
    pub funs: Vec<DeclFun>,
}

#[derive(Debug)]
pub struct DeclFun {
    pub name: common::PathUL<()>,
    pub args: Vec<(usize, usize)>, /* (id, size) */
    pub output: usize,
    pub content: Bloc,
}

#[derive(Debug)]
pub struct Bloc {
    pub content: Vec<Instr>,
    pub last_type: super::typed_rust::PostType,
}

#[derive(Debug)]
pub enum Instr {
    Expr(common::ComputedValue, Expr),
    Binding(usize, Expr),
}

#[derive()]
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
            content: Box::new(ExprInner::Int(s, common::Sizes::SUsize)),
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

#[derive(Debug)]
pub enum ExprInner {
    BinOp(common::TypedBinop, Expr, Expr),
    Bloc(Bloc),
    Bool(bool),
    BuildStruct(usize /* size */, Vec<(usize, Expr)>),
    Coercion(Expr, common::BuiltinType, common::BuiltinType),
    Constant(PathUL<()>),
    Deref(Expr),
    FunCall(PathUL<()>, Vec<Expr>),
    FunCallVar(usize, Vec<Expr>),
    FunVar(PathUL<()>),
    If(Expr, Bloc, Bloc),
    Int(u64, common::Sizes),
    /// Path represent the name of the constant containing the string
    Print(PathUL<()>),
    Proj(Expr, usize),
    Ref(Expr),
    Return(Option<Expr>),
    Set(usize, Expr, Expr),
    Tuple(usize, Vec<Expr>),
    UnaOp(common::TypedUnaop, Expr),
    VarId(usize),
    While(Expr, Bloc),
}
