use super::common;
use std::collections::HashMap;

#[derive(Debug)]
pub struct File {
    pub name: String,
    pub funs: Vec<DeclFun>,
    pub structs: Vec<DeclStruct>,
}

#[derive(Debug)]
pub struct DeclFun {
    pub name: common::Ident,
    pub args: Vec<(common::Ident, bool, PostType)>,
    pub output: PostType,
    pub content: Bloc,
    pub id_counter: common::IdCounter,
}

#[derive(Debug)]
pub struct DeclStruct {
    pub name: common::Ident,
    pub args: HashMap<String, PostType>,
    pub size: usize, /* size in bytes */
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PostType {
    pub content: PostTypeInner,
}

impl PostType {
    pub fn unit() -> Self {
        Self {
            content: PostTypeInner::Tuple(Vec::new()),
            //            size : 0,
        }
    }

    pub const fn diverge() -> Self {
        Self {
            content: PostTypeInner::Diverge,
            //            size : 0,
        }
    }

    pub const fn bool() -> Self {
        Self {
            content: PostTypeInner::BuiltIn(common::BuiltinType::Bool),
            //            size : 1,
        }
    }

    pub const fn i32() -> Self {
        Self {
            content: PostTypeInner::BuiltIn(common::BuiltinType::Int(true, common::Sizes::S32)),
            //            size : 1,
        }
    }

    pub fn to_ref(self, mutable: bool) -> Self {
        Self {
            content: PostTypeInner::Ref(mutable, Box::new(self)),
            //            size : 8,
        }
    }

    pub const fn string() -> Self {
        Self {
            content: PostTypeInner::String,
        }
    }

    pub fn is_mut_ref(&self) -> bool {
        matches!(&self.content, PostTypeInner::Ref(true, _))
    }

    pub fn is_unit(&self) -> bool {
        match &self.content {
            PostTypeInner::Tuple(v) => v.is_empty(),
            _ => false,
        }
    }

    pub fn fun_out_typ(&self) -> Option<&Self> {
        match &self.content {
            PostTypeInner::Fun(_, _, out) => Some(&**out),
            _ => None,
        }
    }

    pub fn get_int_size(&self) -> Option<common::Sizes> {
        match &self.content {
            PostTypeInner::BuiltIn(common::BuiltinType::Int(_, size)) => Some(*size),
            _ => None,
        }
    }

    pub fn get_struct(&self) -> Option<(&str, &Vec<PostType>)> {
        match &self.content {
            PostTypeInner::Struct(name, args) => Some((name, args)),
            PostTypeInner::Ref(_, typ) => match &typ.content {
                PostTypeInner::Struct(name, args) => Some((name, args)),
                _ => None,
            },
            _ => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PostTypeInner {
    BuiltIn(common::BuiltinType),
    Struct(String, Vec<PostType>),
    //    Enum(String),
    Box(Box<PostType>),
    /*    IdentParametrized(String, Vec<PostType>),*/
    Ref(bool, Box<PostType>),
    Tuple(Vec<PostType>),
    FreeType(String),
    // Free types, args types, out type
    Fun(Vec<String>, Vec<PostType>, Box<PostType>),
    Diverge,
    String,
}

#[derive(Debug)]
pub struct Bloc {
    pub content: Vec<Instr>,
    //    pub expr : Option<Expr>,
    //    pub values : HashMap<String, PostType>,
    pub last_type: PostType,
}

#[derive(Debug)]
pub enum Instr {
    Expr(common::ComputedValue, Expr),
    Binding(bool, common::Ident, Expr),
    While(Expr, Bloc),
    Return(Option<Expr>),
}

#[derive()]
pub struct Expr {
    pub content: Box<ExprInner>,
    pub loc: common::Location,
    pub typed: PostType,
}

impl Expr {
    pub fn to_ref(self, mutable: bool) -> Self {
        Self {
            typed: self.typed.clone().to_ref(mutable),
            loc: self.loc,
            content: Box::new(ExprInner::Ref(mutable, self)),
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
    Int(u64),
    Var(common::Ident),
    /*Method(Expr, common::Ident, Vec<Expr>),*/
    FunCall(common::Ident, Vec<Expr>),
    //    Constructor(common::Ident, Vec<Expr>),
    Bloc(Bloc),
    Ref(bool, Expr),
    Deref(Expr),
    Tuple(Vec<Expr>),
    BuildStruct(common::Ident, Vec<(common::Ident, Expr)>),
    Proj(Expr, common::Projector),
    Set(Expr, Expr),
    Print(String),
    String(String),
    //    Vec(Vec<Expr>),
    BinOp(common::TypedBinop, Expr, Expr),
}
