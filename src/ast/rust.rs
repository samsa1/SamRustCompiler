use super::common;
use std::collections::HashMap;
use std::env::Args;
use std::fmt::Display;

pub struct File {
    pub name: String,
    pub content: Vec<Decl>,
    pub dep: Vec<Open>,
    pub err_reporter: common::ErrorReporter,
}

pub enum Open {
    Mod(bool, common::Ident, Option<common::Ident>),
    Use(common::Path<PreType>, Option<common::Ident>),
}

pub enum Decl<DF = DeclFun> {
    Fun(DF),
    Struct(DeclStruct),
    Impl(DeclImpl),
}

pub struct DeclStruct {
    pub public: bool,
    pub name: common::Ident,
    pub args: Vec<(common::Ident, PreType)>,
}

pub struct DeclFun {
    pub public: bool,
    pub name: common::Ident,
    pub self_arg: Option<bool>,
    pub args: Vec<(common::Ident, bool, PreType)>,
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
    Array(usize, Option<usize>),
    Bool,
    Int(Option<bool>, Option<common::Sizes>),
    Enum(String),
    Fun(Vec<usize>, usize),
    Struct(String, Vec<usize>),
    Ref(Option<bool>, usize),
    Deref(usize),
    SameAs(usize),
    Tuple(Vec<usize>),
    Unknown,
}

impl Types {
    pub fn string() -> Self {
        Self::Struct("String".to_string(), Vec::new())
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
        Self::Struct(name.to_string(), Vec::new())
    }

    pub fn tuple(vec_types: Vec<usize>) -> Self {
        Self::Tuple(vec_types)
    }

    pub fn refed(mutable: bool, type_id: usize) -> Self {
        Self::Ref(Some(mutable), type_id)
    }

    pub fn boxed(type_id: usize) -> Self {
        Self::Struct("Box".to_string(), vec![type_id])
    }

    pub fn unref(&self) -> Option<(bool, usize)> {
        match self {
            Self::Ref(mutable, type_id) => Some((mutable.unwrap_or(true), *type_id)),
            Self::Struct(name, args) if name == "Box" && args.len() == 1 => Some((false, args[0])),
            _ => None,
        }
    }
}

impl Display for Types {

    fn fmt(&self, f : &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Self::Bool => write!(f, "bool"),
            Self::Fun(args, _) =>
                write!(f, "fn [{}] -> _", args.len()),
            Self::Int(None, None) =>
                write!(f, "{{integer}}"),
            Self::Int(Some(true), None) =>
                write!(f, "{{signed integer}}"),
            Self::Int(Some(false), None) =>
                write!(f, "{{usigned integer}}"),
            Self::Int(None, Some(size)) => {
                write!(f, "{{{:?}-bits integer}}", size)
            }
            Self::Int(Some(b), Some(size)) => {
                let b = common::BuiltinType::Int(*b, *size);
                write!(f, "{}", b.to_str())
            }
            Self::Struct(name, _) => {
                write!(f, "{}", name)
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
            Self::Enum(name) => {
                write!(f, "{}", name)
            }
            Self::Tuple(args) => {
                write!(f, "{{{} tuple}}", args.len())
            }

            Self::SameAs(_) => panic!("ICE"),
            Self::Deref(_) => panic!("ICE"),
            Self::Unknown => panic!("ICE"),

//            _ => todo!(),
        }
    }

}

#[derive(Debug)]
pub struct TypeStorage {
    count: usize,
    map: HashMap<usize, Types>,
}

impl Default for TypeStorage {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeStorage {
    pub fn new() -> Self {
        Self {
            count: 0,
            map: HashMap::new(),
        }
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

    // /!\ Only a few cases are implemented because
    // only a few case can be encountered at runtime /!\
    /*pub fn forces_to(&mut self, id: usize, typ: Types) -> Result<usize, ()> {
        match (self.map.get(&id), typ) {
            (None, _) => panic!("ICE"),
            (Some(Types::Unknown), typ) => {
                *self.map.get_mut(&id).unwrap() = typ;
                Ok(id)
            }
            (Some(Types::Bool), Types::Bool) => Ok(id),
            (Some(_), Types::Bool) => Err(()),
            (Some(Types::Int(b1, s1)), Types::Int(b2, s2)) => {
                if (b1.is_none() || b1 == &b2) && (s1.is_none() || s1 == &s2) {
                    *self.map.get_mut(&id).unwrap() = Types::Int(b2, s2);
                    Ok(id)
                } else {
                    Err(())
                }
            }
            (_, Types::Int(_, _)) => Err(()),

            _ => todo!(),
        }
    }*/

    pub fn get(&self, id: usize) -> Option<&Types> {
        self.map.get(&id)
    }

    // Use carefully
    pub fn set(&mut self, id: usize, typ: Types) {
        assert!(self.map.contains_key(&id));
        self.map.insert(id, typ);
    }

    pub fn new_ref_unmarked(&mut self, type_id: usize) -> usize {
        match self.map.get(&type_id).unwrap() {
            Types::Bool | Types::Enum(_) | Types::Unknown | Types::Int(_, _) | Types::Fun(_, _) => {
                type_id
            }
            Types::SameAs(type_id) => self.new_ref_unmarked(*type_id),
            Types::Array(_, _) => todo!(),
            Types::Deref(_) => todo!(),
            Types::Ref(Some(false), type_id) => {
                let type_id = self.new_ref_unmarked(*type_id);
                self.insert_type(Types::Ref(Some(false), type_id))
            }
            Types::Ref(_, type_id) => {
                let type_id = self.new_ref_unmarked(*type_id);
                self.insert_type(Types::Ref(None, type_id))
            }
            Types::Tuple(_) => todo!(),
            Types::Struct(name, args) => {
                let mut args2 = Vec::new();
                let name = name.to_string();
                for type_id in args.clone().into_iter() {
                    args2.push(self.new_ref_unmarked(type_id))
                }
                self.insert_type(Types::Struct(name, args2))
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
    IdentParametrized(common::Ident, Vec<PreType>),
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
                loc : expr.loc,
                content : InstrInner::Expr(common::ComputedValue::Keep, expr),
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
    pub content : InstrInner<T>,
    pub loc : common::Location,
}

#[derive(Debug, Clone)]
pub enum InstrInner<T = Option<PreType>> {
    Expr(common::ComputedValue, Expr<T>),
    Binding(bool, common::Ident, Expr<T>),
    While(Expr<T>, Bloc<T>),
    Return(Option<Expr<T>>),
}

impl<T> InstrInner<T> {
    pub fn to_instr(self, start : usize, end : usize) -> Instr<T> {
        Instr {
            loc : common::Location::new(start, end),
            content : self,
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
    If(Expr<T>, Bloc<T>, Bloc<T>),
    Bool(bool),
    Int(u64, Option<(bool, common::Sizes)>),
    Var(common::Ident),
    Method(Expr<T>, common::Ident, Vec<Expr<T>>),
    FunCall(Vec<T>, common::Ident, Vec<Expr<T>>),
    MacroCall(common::Ident, Vec<Expr<T>>),
    BinaryOp(common::BinOperator, Expr<T>, Expr<T>),
    UnaryOp(common::UnaOperator, Expr<T>),
    Bloc(Bloc<T>),
    Ref(bool, Expr<T>),
    Deref(Expr<T>),
    Tuple(Vec<Expr<T>>),
    BuildStruct(common::Ident, Vec<(common::Ident, Expr<T>)>),
    Proj(Expr<T>, common::Projector),
    String(String),
    Array(Vec<Expr<T>>),
    Parenthesis(Expr<T>),
    Index(Expr<T>, Expr<T>),
}

impl<T> ExprInner<T> {
    pub fn unit() -> Self {
        Self::Tuple(Vec::new())
    }
}
