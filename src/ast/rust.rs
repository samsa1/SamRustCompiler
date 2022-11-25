use super::common::{self, PathUL};
use std::collections::{HashMap, HashSet};
use std::fmt::Display;

pub struct File {
    pub name: String,
    pub content: Vec<Decl>,
    pub dep: Vec<Open>,
    pub err_reporter: common::ErrorReporter,
}

pub enum Open {
    Mod(bool, common::Ident, Option<common::Ident>),
    Use(common::Path<()>, Option<common::Ident>),
}

pub enum Decl<DF = DeclFun> {
    Fun(DF),
    Struct(DeclStruct),
    Enum(DeclEnum),
    Impl(DeclImpl),
    Const(DeclConst),
}

pub struct DeclConst<T = Option<PreType>> {
    pub public: bool,
    pub name: common::Ident,
    pub typ: PreType,
    pub expr: Expr<T>,
}

pub struct DeclStruct {
    pub public: bool,
    pub name: common::Ident,
    pub args: Vec<(common::Ident, PreType)>,
}

pub struct DeclEnum {
    pub public: bool,
    pub name: common::Ident,
    pub args: Vec<(common::Ident, Vec<PreType>)>,
}

pub struct DeclFun {
    pub public: bool,
    pub name: common::Ident,
    pub self_arg: Option<Option<bool>>,
    pub args: Vec<(common::Ident, bool, PreType)>,
    pub generics: Vec<(common::Ident, Vec<common::Path<()>>)>,
    pub output: PreType,
    pub content: Bloc,
    pub id_counter: common::IdCounter,
}

pub struct TypedDeclFun {
    pub public: bool,
    pub name: common::Ident,
    pub args: Vec<(common::Ident, bool, usize)>,
    pub types: Types,
    pub output: PreType,
    pub content: Bloc<usize>,
}

pub struct DeclImpl {
    pub name: common::Ident,
    pub content: Vec<DeclFun>,
}

#[derive(Debug, Clone)]
pub enum Types {
    Free(String),
    Array(usize, Option<usize>),
    Bool,
    Int(Option<bool>, Option<common::Sizes>),
    Enum(PathUL<()>, Vec<usize>),
    Fun(Vec<usize>, usize),
    Struct(PathUL<()>, Vec<usize>),
    Ref(Option<bool>, usize),
    Deref(usize),
    SameAs(usize),
    Tuple(Vec<usize>),
    Unknown,
}

impl Types {
    pub fn string() -> Self {
        Self::Struct(
            common::PathUL::new(vec![common::NamePath::Name("String".to_string())]),
            Vec::new(),
        )
    }

    pub const fn int() -> Self {
        Self::Int(None, None)
    }

    pub const fn usize() -> Self {
        Self::Int(Some(false), Some(common::Sizes::SUsize))
    }

    pub const fn bool() -> Self {
        Self::Bool
    }

    pub const fn unknown() -> Self {
        Self::Unknown
    }

    pub fn struct_from_str(name: &str) -> Self {
        Self::Struct(
            common::PathUL::new(vec![common::NamePath::Name(name.to_string())]),
            Vec::new(),
        )
    }

    pub fn tuple(vec_types: Vec<usize>) -> Self {
        Self::Tuple(vec_types)
    }

    pub fn refed(mutable: bool, type_id: usize) -> Self {
        Self::Ref(Some(mutable), type_id)
    }

    pub fn unref(&self) -> Option<(bool, usize)> {
        match self {
            Self::Ref(mutable, type_id) => Some((mutable.unwrap_or(true), *type_id)),
            _ => None,
        }
    }
}

impl Display for Types {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Self::Bool => write!(f, "bool"),
            Self::Free(name) => write!(f, "free {}", name),
            Self::Fun(args, _) => write!(f, "fn [{}] -> _", args.len()),
            Self::Int(None, None) => write!(f, "{{integer}}"),
            Self::Int(Some(true), None) => write!(f, "{{signed integer}}"),
            Self::Int(Some(false), None) => write!(f, "{{usigned integer}}"),
            Self::Int(None, Some(size)) => {
                write!(f, "{{{:?}-bits integer}}", size)
            }
            Self::Int(Some(b), Some(size)) => {
                let b = common::BuiltinType::Int(*b, *size);
                write!(f, "{}", b.to_str())
            }
            Self::Struct(name, _) => {
                write!(f, "{:?}", name)
            }
            Self::Ref(_, _) => {
                write!(f, "&_")
            }
            Self::Array(_, None) => {
                write!(f, "{{array}}")
            }
            Self::Array(_, Some(length)) => {
                write!(f, "[_; {}]", length)
            }
            Self::Enum(path, _) => {
                write!(f, "{:?}", path)
            }
            Self::Tuple(args) => {
                write!(f, "{{{} tuple}}", args.len())
            }

            Self::SameAs(_) => panic!("ICE"),
            Self::Deref(_) => panic!("ICE"),
            Self::Unknown => panic!("ICE"),
        }
    }
}

#[derive(Debug)]
pub struct TypeStorage {
    free_names: HashSet<String>,
    count: usize,
    map: HashMap<usize, Types>,
}

impl TypeStorage {
    pub fn new(free: &Vec<String>) -> Self {
        let mut free_names = HashSet::new();
        for name in free {
            assert!(free_names.insert(name.clone()));
        }
        Self {
            free_names,
            count: 0,
            map: HashMap::new(),
        }
    }

    pub fn get_frees(&self) -> &HashSet<String> {
        &self.free_names
    }

    pub fn insert_unit(&mut self) -> usize {
        self.insert_type(Types::tuple(Vec::new()))
    }

    pub fn insert_bool(&mut self) -> usize {
        self.insert_type(Types::bool())
    }

    pub fn insert_string(&mut self) -> usize {
        self.insert_type(Types::string())
    }

    pub fn insert_usize(&mut self) -> usize {
        self.insert_type(Types::usize())
    }

    pub fn incr(&mut self) -> usize {
        let i = self.count;
        self.count += 1;
        i
    }

    pub fn insert_type(&mut self, typ: Types) -> usize {
        let i = self.incr();
        assert!(self.map.insert(i, typ).is_none());
        i
    }

    pub fn get(&self, id: usize) -> Option<&Types> {
        self.map.get(&id)
    }

    /// Use carefully
    pub fn set(&mut self, id: usize, typ: Types) {
        assert!(self.map.contains_key(&id));
        self.map.insert(id, typ);
    }

    pub fn new_ref_unmarked(&mut self, type_id: usize) -> usize {
        match self.map.get(&type_id).unwrap() {
            Types::Bool | Types::Unknown | Types::Int(_, _) | Types::Fun(_, _) | Types::Free(_) => {
                type_id
            }
            Types::SameAs(type_id) => self.new_ref_unmarked(*type_id),
            Types::Array(type_id, size) => {
                let size = *size;
                let type_id = self.new_ref_unmarked(*type_id);
                self.insert_type(Types::Array(type_id, size))
            }
            Types::Deref(type_id) => {
                let type_id = self.new_ref_unmarked(*type_id);
                self.insert_type(Types::Deref(type_id))
            }
            Types::Ref(Some(false), type_id) => {
                let type_id = self.new_ref_unmarked(*type_id);
                self.insert_type(Types::Ref(Some(false), type_id))
            }
            Types::Ref(_, type_id) => {
                let type_id = self.new_ref_unmarked(*type_id);
                self.insert_type(Types::Ref(None, type_id))
            }
            Types::Tuple(exprs) => {
                let mut exprs2 = Vec::new();
                for type_id in exprs.clone().into_iter() {
                    exprs2.push(self.new_ref_unmarked(type_id))
                }
                self.insert_type(Types::Tuple(exprs2))
            }
            Types::Struct(name, args) => {
                let mut args2 = Vec::new();
                let name = name.clone();
                for type_id in args.clone().into_iter() {
                    args2.push(self.new_ref_unmarked(type_id))
                }
                self.insert_type(Types::Struct(name, args2))
            }
            Types::Enum(name, args) => {
                let mut args2 = Vec::new();
                let name = name.clone();
                for type_id in args.clone().into_iter() {
                    args2.push(self.new_ref_unmarked(type_id))
                }
                self.insert_type(Types::Enum(name, args2))
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct PreType {
    pub content: PreTypeInner,
}

impl PreType {
    pub fn unit() -> Self {
        Self {
            content: PreTypeInner::Tuple(Vec::new()),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum PreTypeInner {
    Ident(common::Ident),
    IdentPath(common::Path<()>),
    IdentParametrized(common::Ident, Vec<PreType>),
    IdentParametrizedPath(common::Path<()>, Vec<PreType>),
    Ref(bool, Box<PreType>),
    Tuple(Vec<PreType>),
    Fun(Vec<PreType>, Box<PreType>),
}

impl PreTypeInner {
    pub fn to_type(self) -> PreType {
        PreType { content: self }
    }
}

#[derive(Debug, Clone)]
pub struct Bloc<T = Option<PreType>> {
    pub content: Vec<Instr<T>>,
    pub loc: common::Location,
}

impl<T> Bloc<T> {
    pub fn from_expr(expr: Expr<T>) -> Self {
        Self {
            loc: expr.loc,
            content: vec![Instr {
                loc: expr.loc,
                content: InstrInner::Expr(common::ComputedValue::Keep, expr),
            }],
        }
    }

    pub fn empty() -> Self {
        Self {
            content: Vec::new(),
            loc: common::Location::default(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Instr<T = Option<PreType>> {
    pub content: InstrInner<T>,
    pub loc: common::Location,
}

#[derive(Debug, Clone)]
pub enum InstrInner<T = Option<PreType>> {
    Expr(common::ComputedValue, Expr<T>),
    Binding(bool, common::Ident, T, Expr<T>),
}

impl<T> InstrInner<T> {
    pub fn to_instr(self, start: usize, end: usize) -> Instr<T> {
        Instr {
            loc: common::Location::new(start, end),
            content: self,
        }
    }
}

#[derive(Clone)]
pub struct Expr<T = Option<PreType>> {
    pub content: Box<ExprInner<T>>,
    pub loc: common::Location,
    pub typed: T,
}

impl<T: std::fmt::Debug> std::fmt::Debug for Expr<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Expr")
            .field("c", &*self.content)
            .field("t", &self.typed)
            .finish()
    }
}

impl<T> Expr<T> {
    pub fn to_bloc(self) -> Bloc<T> {
        Bloc {
            loc: self.loc,
            content: vec![Instr {
                loc: self.loc,
                content: InstrInner::Expr(common::ComputedValue::Keep, self),
            }],
        }
    }
}

impl<T> Expr<Option<T>> {
    pub fn unit() -> Self {
        Self {
            loc: common::Location::default(),
            typed: None,
            content: Box::new(ExprInner::unit()),
        }
    }

    pub fn var(name: &str, loc: common::Location) -> Self {
        Self {
            content: Box::new(ExprInner::Var(common::Ident::new(name, loc))),
            loc,
            typed: None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ExprInner<T = Option<PreType>> {
    Array(Vec<Expr<T>>),
    BinaryOp(common::BinOperator, Expr<T>, Expr<T>),
    Bloc(Bloc<T>),
    Bool(bool),
    BuildStruct(common::Ident, Vec<(common::Ident, Expr<T>)>),
    BuildStructPath(common::Path<()>, Vec<(common::Ident, Expr<T>)>),
    Coercion(Expr<T>, T),
    Constructor(common::Path<()>, Vec<Expr<T>>),
    Deref(Expr<T>),
    FunCall(Vec<T>, common::Ident, Vec<Expr<T>>),
    FunCallPath(Vec<T>, common::Path<()>, Vec<Expr<T>>),
    If(Expr<T>, Bloc<T>, Bloc<T>),
    Index(Expr<T>, Expr<T>),
    Int(u64, Option<(bool, common::Sizes)>),
    MacroCall(common::Ident, Vec<Expr<T>>),
    Method(Expr<T>, common::Ident, Vec<Expr<T>>),
    Parenthesis(Expr<T>),
    PatternMatching(
        Expr<T>,
        Vec<Pattern<T>>,
        Option<(bool, common::Ident, Bloc<T>)>,
    ),
    Proj(Expr<T>, common::Projector),
    Ref(bool, Expr<T>),
    Return(Option<Expr<T>>),
    String(String),
    Tuple(Vec<Expr<T>>),
    UnaryOp(common::UnaOperator, Expr<T>),
    Var(common::Ident),
    VarPath(common::Path<()>),
    While(Expr<T>, Bloc<T>),
}

impl<T> ExprInner<T> {
    pub fn unit() -> Self {
        Self::Tuple(Vec::new())
    }
}

#[derive(Debug, Clone)]
pub struct Pattern<T = Option<PreType>> {
    pub constructor: common::Path<()>,
    pub arguments: Vec<(bool, common::Ident)>,
    pub guard: Option<Expr<T>>,
    pub bloc: Bloc<T>,
}

impl<T> Pattern<T> {
    pub fn new(
        constructor: common::Path<()>,
        arguments: Vec<(bool, common::Ident)>,
        guard: Option<Expr<T>>,
        bloc: Bloc<T>,
    ) -> Self {
        Self {
            constructor,
            arguments,
            guard,
            bloc,
        }
    }

    pub fn get_constructor(&self) -> &common::Path<()> {
        &self.constructor
    }

    pub fn get_arguments(&self) -> &Vec<(bool, common::Ident)> {
        &self.arguments
    }

    pub fn get_guard(&self) -> &Option<Expr<T>> {
        &self.guard
    }
    pub fn get_bloc(&self) -> &Bloc<T> {
        &self.bloc
    }
}
