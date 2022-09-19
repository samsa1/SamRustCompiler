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
    pub size    : usize, /* size in bytes */
}

impl PostType {
    pub fn unit() -> Self {
        Self {
            content : PostTypeInner::Tuple(Vec::new()),
            size : 0,
        }
    }

    pub const fn diverge() -> Self {
        Self {
            content : PostTypeInner::Diverge,
            size : 0,
        }
    }

    pub const fn bool() -> Self {
        Self {
            content : PostTypeInner::BuiltIn(common::BuiltinType::Bool),
            size : 1,
        }
    }

    pub const fn i32() -> Self {
        Self {
            content : PostTypeInner::BuiltIn(common::BuiltinType::Int(true, common::Sizes::S32)),
            size : 1,
        }
    }

    pub fn to_ref(self, mutable : bool) -> Self {
        Self {
            content : PostTypeInner::Ref(mutable, Box::new(self)),
            size : 8,
        }
    }

    pub const fn string() -> Self {
        Self {
            content : PostTypeInner::String,
            size : 8,
        }
    }

    pub fn is_mut_ref(&self) -> bool {
        match &self.content {
            PostTypeInner::Ref(true, _) => true,
            _ => false
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
    Ref(bool, Box<PostType>),
    Tuple(Vec<PostType>),
    Fun(Vec<PostType>, Box<PostType>),
    Diverge,
    String,
}

#[derive(Debug)]
pub struct Bloc {
    pub content : Vec<Instr>,
    pub expr : Option<Expr>,
//    pub values : HashMap<String, PostType>,
//    pub last_type : PostType,
}

#[derive(Debug)]
pub enum Instr {
    Expr(Expr),
    Binding(bool, common::Ident, Expr),
    While(Expr, Bloc),
    Return(Option<Expr>),
}

#[derive(Debug)]
pub struct Expr {
    pub content : Box<ExprInner>,
    pub loc : common::Location,
    pub typed : PostType,
}

#[derive(Debug)]
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
    BuildStruct(common::Ident, Vec<(common::Ident, Expr)>),
    Proj(Expr, common::Projector),
    Set(Expr, Expr),
    Print(String),
    String(String),
    Vec(usize, Vec<Expr>),
}