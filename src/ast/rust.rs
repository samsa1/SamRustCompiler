use super::common;


pub struct File {
    pub name : String,
    pub content : Vec<Decl>,
}

pub enum Decl {
    Fun(DeclFun),
    Struct(DeclStruct),
}

pub struct DeclFun {
    pub name : common::Ident,
    pub args : Vec<(common::Ident, bool, PreType)>,
    pub output : PreType,
    pub content : Bloc,
}

pub struct DeclStruct {
    pub name : common::Ident,
    pub args : Vec<(common::Ident, PreType)>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PreType {
    pub content : PreTypeInner,
    pub mutable : bool,
}

impl PreType {
    pub fn unit() -> Self {
        Self {
            content : PreTypeInner::Tuple(Vec::new()),
            mutable : false,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PreTypeInner {
    Ident(common::Ident),
    IdentParametrized(common::Ident, Vec<PreType>),
    Ref(Box<PreType>),
    Tuple(Vec<PreType>),
    Fun(Vec<PreType>, Box<PreType>),
}

impl PreTypeInner {
    pub fn to_nonmut(self) -> PreType {
        PreType {
            content : self,
            mutable : false,
        }
    }

    pub fn to_mut(self) -> PreType {
        PreType {
            content : self,
            mutable : true,
        }
    }
}

pub struct Bloc {
    pub content : Vec<Instr>
}

pub enum Instr {
    Expr(Expr),
    Binding(bool, common::Ident, Expr),
    While(Expr, Bloc),
    Return(Option<Expr>),
}

pub struct Expr {
    pub content : Box<ExprInner>,
    pub loc : common::Location,
    pub typed : Option<PreType>,
}

impl Expr {
    pub fn unit() -> Self {
        Self {
            loc:common::Location::default(),
            typed:None,
            content:Box::new(ExprInner::unit()),
        }
    }
}

pub enum ExprInner {
    If(Expr, Expr, Expr),
    Bool(bool),
    Int(usize),
    Var(common::Ident),
    Method(Expr, common::Ident, Vec<Expr>),
    FunCall(common::Ident, Vec<Expr>),
    MacroCall(common::Ident, Vec<Expr>),
    Bloc(Bloc),
    Ref(bool, Expr),
    Deref(Expr),
    Tuple(Vec<Expr>),
    BuildStruct(common::Ident, Vec<Expr>),
    Proj(Expr, common::Projector),
}

impl ExprInner {
    pub fn unit() -> Self {
        Self::Tuple(Vec::new())
    }
}