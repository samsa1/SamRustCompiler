use super::common;
use std::collections::HashMap;

#[derive(Debug)]
pub struct File {
    pub name: String,
    pub funs: Vec<DeclFun>,
    //    pub structs: Vec<DeclStruct>,
}

impl File {
    pub fn empty() -> Self {
        Self {
            name: String::new(),
            funs: Vec::new(),
            //            structs: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub struct DeclFun {
    pub free: Vec<String>,
    pub name: common::PathUL<()>,
    pub args: Vec<(common::Ident, bool, PostType)>,
    pub output: PostType,
    pub content: Bloc,
    pub id_counter: common::IdCounter,
}

#[derive(Debug)]
pub struct DeclStruct {
    pub name: common::Ident,
    pub args: HashMap<String, PostType>,
    /// Size in bytes
    pub size: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct PostType {
    pub content: PostTypeInner,
}

impl PostType {
    pub fn unit() -> Self {
        Self {
            content: PostTypeInner::Tuple(Vec::new()),
        }
    }

    pub const fn diverge() -> Self {
        Self {
            content: PostTypeInner::Diverge,
        }
    }

    pub const fn bool() -> Self {
        Self {
            content: PostTypeInner::BuiltIn(common::BuiltinType::Bool),
        }
    }

    pub fn free(f: &str) -> Self {
        Self {
            content: PostTypeInner::FreeType(f.to_string()),
        }
    }

    pub const fn i32() -> Self {
        Self {
            content: PostTypeInner::BuiltIn(common::BuiltinType::Int(true, common::Sizes::S32)),
        }
    }

    pub const fn usize() -> Self {
        Self {
            content: PostTypeInner::BuiltIn(common::BuiltinType::Int(false, common::Sizes::SUsize)),
        }
    }

    pub fn to_ref(self, mutable: bool) -> Self {
        Self {
            content: PostTypeInner::Ref(mutable, Box::new(self)),
        }
    }

    pub const fn string() -> Self {
        Self {
            content: PostTypeInner::String,
        }
    }

    pub fn is_ref(&self) -> bool {
        matches!(&self.content, PostTypeInner::Ref(_, _))
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

    pub fn get_struct(&self) -> Option<(&common::PathUL<()>, &Vec<PostType>)> {
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
    Struct(common::PathUL<()>, Vec<PostType>),
    Enum(common::PathUL<()>, Vec<PostType>),
    Box(Box<PostType>),
    Ref(bool, Box<PostType>),
    Tuple(Vec<PostType>),
    FreeType(String),
    /// Free types, args types, out type
    Fun(Vec<String>, Vec<PostType>, Box<PostType>),
    Diverge,
    String,
}

#[derive(Debug, Clone)]
pub struct Bloc {
    pub content: Vec<Instr>,
    pub last_type: PostType,
}

#[derive(Debug, Clone)]
pub enum Instr {
    Expr(common::ComputedValue, Expr),
    Binding(bool, common::Ident, Expr),
}

#[derive(Clone)]
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

#[derive(Debug, Clone)]
pub enum ExprInner {
    BinOp(common::TypedBinop, Expr, Expr),
    Bloc(Bloc),
    Bool(bool),
    BuildStruct(common::PathUL<()>, Vec<(common::Ident, Expr)>),
    Coercion(Expr, common::BuiltinType, common::BuiltinType),
    Constructor(common::PathUL<()>, Vec<Expr>),
    Deref(Expr),
    FunCall(common::Ident, Vec<Expr>),
    FunCallPath(Vec<PostType>, common::PathUL<()>, Vec<Expr>),
    Int(u64),
    If(Expr, Bloc, Bloc),
    PatternMatching(Expr, Vec<Pattern>, Option<(bool, common::Ident, Bloc)>),
    Proj(Expr, common::Projector),
    Print(String),
    PrintPtr(Expr),
    Ref(bool, Expr),
    Return(Option<Expr>),
    Set(Expr, Expr),
    String(String),
    TraitFun(common::PathUL<()>, PostType, String, Vec<Expr>),
    // usize is a padding at the end padding
    Tuple(Vec<Expr>, usize),
    UnaOp(common::TypedUnaop, Expr),
    Var(common::Ident),
    VarPath(common::PathUL<()>),
    While(Expr, Bloc),
}

#[derive(Debug, Clone)]
pub struct Pattern {
    pub constructor_id: u64,
    pub constructor: common::PathUL<()>,
    pub arguments: Vec<(bool, common::Ident, PostType)>,
    pub guard: Option<Expr>,
    pub bloc: Bloc,
}
