use super::common;
use std::collections::HashMap;

pub struct File {
    pub name : common::Ident,
    pub content : Vec<Decl>,
}

pub enum Decl {
    Fun(DeclFun),
    Struct(DeclStruct),
}

pub struct DeclFun {
    pub name : common::Ident,
    pub args : Vec<(common::Ident, bool, Type)>,
    pub output : Type,
    pub content : Bloc,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Type {
    pub content : TypeInner,
    pub mutable : bool,
}

impl Type {
    pub fn unit() -> Self {
        Self {
            content : TypeInner::Tuple(Vec::new()),
            mutable : false,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeInner {
    BuildIn(common::BuildinType),
    Struct(common::Ident),
    Enum(common::Ident),
    IdentParametrized(common::Ident, Vec<Type>),
    Ref(Box<Type>),
    Tuple(Vec<Type>),
    Fun(Vec<Type>, Box<Type>),
}

impl TypeInner {
    pub fn to_nonmut(self) -> Type {
        Type {
            content : self,
            mutable : false,
        }
    }

    pub fn to_mut(self) -> Type {
        Type {
            content : self,
            mutable : true,
        }
    }
}

pub struct Bloc {
    pub content : Vec<Instr>,
    pub values : HashMap<common::Ident, Type>
}

pub enum Instr {
    Expr(Expr),
    Binding(bool, common::Ident, Expr),
    While(Expr, Bloc),
    Return(Option<Expr>),
}

pub struct Expr {
    pub content : Box<ExprInner>,
    pub typed : Type,
}

pub enum ExprInner {
    If(Expr, Expr, Expr),
    Bool(bool),
    Int(usize),
    Var(common::Ident),
    FunCall(Expr, Vec<Expr>),
    Constructor(usize, Vec<Expr>),
    Box(Expr),
    Free(Expr),
    Bloc(Bloc),
    Ref(bool, Expr),
    Deref(Expr),
    Tuple(Vec<Expr>),
/*    BuildStruct(common::Ident, Vec<Expr>),*/
    Proj(Expr, usize),
}