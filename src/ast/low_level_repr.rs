use super::common;
use std::collections::HashMap;

#[derive(Debug)]
pub struct File {
    pub name: String,
    pub funs: Vec<DeclFun>,
    pub strings: HashMap<String, String>,
}

#[derive(Debug)]
pub struct DeclFun {
    pub name: common::Ident,
    pub args: Vec<(usize, super::typed_rust::PostType)>,
    pub output: super::typed_rust::PostType,
    pub content: Bloc,
}

#[derive(Debug)]
pub struct Bloc {
    pub content: Vec<Instr>,
    //    pub expr : Option<Expr>,
    //    pub values : HashMap<String, PostType>,
    pub last_type: super::typed_rust::PostType,
}

#[derive(Debug)]
pub enum Instr {
    Expr(common::ComputedValue, Expr),
    Binding(usize, Expr),
    While(Expr, Bloc),
    Return(Option<Expr>),
}

#[derive()]
pub struct Expr {
    pub content: Box<ExprInner>,
    pub loc: common::Location,
    pub typed: super::typed_rust::PostType,
}

impl Expr {
    pub fn to_ref(self) -> Self {
        Self {
            typed: self.typed.clone().to_ref(false),
            loc: self.loc,
            content: Box::new(ExprInner::Ref(self)),
        }
    }
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Expr").field("c", &self.content).finish()
    }
}

#[derive(Debug)]
pub enum ExprInner {
    If(Expr, Bloc, Bloc),
    Bool(bool),
    Int(u64, common::Sizes),
    VarId(usize),
    VarGlobal(String),
    FunCall(String, Vec<Expr>),
    FunCallVar(usize, Vec<Expr>),
    Bloc(Bloc),
    Ref(Expr),
    Deref(Expr),
    Tuple(usize, Vec<Expr>),
    BuildStruct(usize /* size */, Vec<(usize, Expr)>),
    Proj(Expr, usize),
    Set(usize, Expr, Expr),
    Print(String), /* name of the constant containing the string /!\ */
    Constant(String),
    BinOp(common::TypedBinop, Expr, Expr),
}
