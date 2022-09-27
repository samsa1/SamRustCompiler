use super::common::{BuiltinType, Location};
use std::collections::HashMap;

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Ident {
    Name(String),
    Temp(usize),
}

pub struct File {
    pub name : Ident,
    pub content : Vec<Decl>,
}

pub enum Decl {
    Fun(DeclFun),
}

pub struct DeclFun {
    pub name : Ident,
    pub args : Vec<(Ident, bool, Type)>,
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
    BuildIn(BuildinType),
    Struct(Ident),
    Enum(Ident),
    IdentParametrized(Ident, Vec<Type>),
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
    pub values : HashMap<Ident, Type>
}

pub enum Instr {
    Expr(Expr),
    Binding(bool, Ident, Expr),
    While(Expr, Bloc),
    Return(Option<Expr>),
}

pub struct Expr {
    pub content : Box<ExprInner>,
    pub typed : Type,
    pub loc : Location,
}

pub enum VarWrapped {
    Direct(bool, Ident), /* bool : can be cloned */
    Ref(bool, Ident), /* bool : mutable */
}

pub enum ExprInner {
    If(Expr, Expr, Expr),
    Bool(bool),
    Int(usize),
    Var(VarWrapped),
    FunCall(Ident, Vec<Expr>),
    Box(Expr),
    Free(Expr),
    Bloc(Bloc),
    Ref(bool, VarWrapped),
    Deref(Expr),
    Tuple(Vec<Expr>),
    Proj(Expr, usize),
}