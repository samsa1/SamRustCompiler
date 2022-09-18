use super::common;
use std::collections::HashMap;

pub struct File {
    pub name : String,
    pub funs : Vec<DeclFun>,
    pub structs : Vec<DeclStruct>,
}

pub struct DeclFun {
    pub name : common::Ident,
    pub args : Vec<(common::Ident, bool, PostType)>,
    pub output : PostType,
    pub content : Bloc,
}

pub struct DeclStruct {
    pub name : common::Ident,
    pub args : HashMap<String, PostType>,
    pub size : usize, /* size in bytes */
}

#[derive(Clone, Debug, PartialEq)]
pub struct PostType {
    pub content : PostTypeInner,
    pub mutable : bool,
    pub size    : usize, /* size in bytes */
}

impl PostType {
    pub fn unit() -> Self {
        Self {
            content : PostTypeInner::Tuple(Vec::new()),
            mutable : false,
            size : 0,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum PostTypeInner {
    BuiltIn(common::BuiltinType),
    Struct(String),
    Enum(String),
    Box(Box<PostType>),
    IdentParametrized(common::Ident, Vec<PostType>),
    Ref(Box<PostType>),
    Tuple(Vec<PostType>),
    Fun(Vec<PostType>, Box<PostType>),
}

impl PostTypeInner {
    pub fn to_nonmut(self) -> PostType {
        PostType {
            content : self,
            mutable : false,
            size : todo!(),
        }
    }

    pub fn to_mut(self) -> PostType {
        PostType {
            content : self,
            mutable : true,
            size : todo!()
        }
    }
}

pub struct Bloc {
    pub content : Vec<Instr>,
    pub values : HashMap<String, PostType>
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
    pub typed : PostType,
}

pub enum ExprInner {
    If(Expr, Expr, Expr),
    Bool(bool),
    Int(usize),
    Var(common::Ident),
    /*Method(Expr, common::Ident, Vec<Expr>),*/
    FunCall(common::Ident, Vec<Expr>),
    Constructor(common::Ident, Vec<Expr>),
    Bloc(Bloc),
    Ref(bool, Expr),
    Deref(Expr),
    Tuple(Vec<Expr>),
    BuildStruct(common::Ident, Vec<Expr>),
    Proj(Vec<Expr>, common::Projector),
}