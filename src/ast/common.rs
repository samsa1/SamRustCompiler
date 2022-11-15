use std::str::FromStr;
use std::hash::{Hash, Hasher};

use crate::typing::errors::TypeError;

pub enum UnOp {
    Neg,
    Not,
    Ref(bool),
    Deref,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NamePath<T, I = Ident> {
    Name(I),
    Specialisation(Vec<T>),
}

impl<T : Clone> NamePath<T, String> {
    fn add_loc(&self) -> NamePath<T, Ident> {
        match self {
            NamePath::Name(s) => NamePath::Name(Ident::new(s, Location::default())),
            NamePath::Specialisation(l) => NamePath::Specialisation(l.clone()),
        }
    }
}

impl<T : Clone> NamePath<T, Ident> {
    fn clean(&self) -> NamePath<T, String> {
        match self {
            NamePath::Name(s) => NamePath::Name(s.get_content().to_string()),
            NamePath::Specialisation(l) => NamePath::Specialisation(l.clone()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Path<T> {
    name: Vec<NamePath<T, Ident>>,
    loc: Location,
}

impl<T> Path<T> {
    pub fn new(name: Vec<NamePath<T, Ident>>, loc: Location) -> Self {
        Self { name, loc }
    }

    pub fn get_content(&self) -> &Vec<NamePath<T, Ident>> {
        &self.name
    }

    pub fn content(self) -> Vec<NamePath<T, Ident>> {
        self.name
    }

    pub fn get_loc(&self) -> Location {
        self.loc
    }

    pub fn last(&self) -> Option<Ident> {
        let i = self.name.len();
        match &self.name[i - 1] {
            NamePath::Name(id) => Some(id.clone()),
            _ => None,
        }
    }

    pub fn pop(&mut self) -> Option<NamePath<T, Ident>> {
        self.name.pop()
    }

    pub fn push(&mut self, el : NamePath<T, Ident>) {
        self.name.push(el)
    }

    pub fn from_vec(el : Vec<&str>) -> Self {
        Self {
            name : el.into_iter().map(|i| NamePath::Name(Ident::new(i, Location::default()))).collect(),
            loc : Location::default(),
        }
    }

    pub fn get_fst_id(&self) -> Option<&str> {
        match self.name.get(0)? {
            NamePath::Name(name) => Some(name.get_content()),
            _ => None,
        }
    }

    pub fn is_vec(&self) -> bool {
        if self.name.len() != 3 {
            return false
        }
        match &self.name[0] {
            NamePath::Name(id) if id.get_content() == "std" => (),
            _ => return false
        }
        match &self.name[1] {
            NamePath::Name(id) if id.get_content() == "vec" => (),
            _ => return false
        }
        match &self.name[2] {
            NamePath::Name(id) if id.get_content() == "Vec" => (),
            _ => return false
        }
        true
    }

    pub fn append(&mut self, mut other : Self) {
        self.name.append(&mut other.name)
    }
}

impl<T : Clone> Path<T> {
    pub fn cleaned(&self) -> PathUL<T> {
        PathUL {
            name : self.name.iter().map(NamePath::clean).collect(),
        }
    }
}

impl<T : PartialEq> PartialEq for Path<T> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PathUL<T, I = String> {
    name: Vec<NamePath<T, I>>,
}

impl<T, I> PathUL<T, I> {
    pub fn new(name: Vec<NamePath<T, I>>) -> Self {
        Self { name }
    }

    pub fn get_content(&self) -> &Vec<NamePath<T, I>> {
        &self.name
    }

    pub fn content(self) -> Vec<NamePath<T, I>> {
        self.name
    }

    pub fn pop(&mut self) -> Option<NamePath<T, I>> {
        self.name.pop()
    }

    pub fn push(&mut self, el : NamePath<T, I>) {
        self.name.push(el)
    }
}

impl<T> PathUL<T, String> {
    pub fn from_vec(el : Vec<&str>) -> Self {
        Self {
            name : el.into_iter().map(|i| NamePath::Name(i.to_string())).collect()
        }
    }
}

impl<T : Clone> PathUL<T, String> {
    pub fn add_loc(&self) -> Path<T> {
        Path {
            name : self.name.iter().map(|n| n.add_loc()).collect(),
            loc : Location::default(),
        }
    }

}

impl<T, I : Clone> PathUL<T, I> {
    pub fn last(&self) -> Option<I> {
        let i = self.name.len();
        match &self.name[i] {
            NamePath::Name(id) => Some(id.clone()),
            _ => None,
        }
    }
}


#[derive(Clone)]
pub struct Ident {
    name: String,
    loc: Location,
}

impl Ident {
    pub fn get_content(&self) -> &str {
        &self.name
    }

    pub fn content(self) -> String {
        self.name
    }

    pub fn new(s: &str, loc: Location) -> Self {
        Self {
            name: s.to_string(),
            loc,
        }
    }

    pub fn new_from(name: String, start: usize, end: usize) -> Self {
        Self {
            name,
            loc: Location::new(start, end),
        }
    }

    pub fn get_loc(&self) -> Location {
        self.loc
    }

    pub fn to_path(&self) -> Path<()> {
        Path {
            name: vec![NamePath::Name(self.clone())],
            loc: self.loc,
        }
    }
}

impl FromStr for Ident {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self {
            name: s.to_string(),
            loc: Location::default(),
        })
    }
}

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }

    /*    fn ne(&self, other : &Self) -> bool {
        self.name != other.name
    }*/
}

impl std::fmt::Debug for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Ident").field("n", &self.name).finish()
    }
}

impl Hash for Ident {
    fn hash<H:Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Location {
    start: usize,
    end: usize,
}

impl Location {
    pub const fn default() -> Self {
        Self {
            start: usize::MAX,
            end: usize::MAX,
        }
    }

    pub fn start(&self) -> usize {
        self.start
    }

    pub fn end(&self) -> usize {
        self.end
    }

    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Sizes {
    S8,
    S16,
    S32,
    S64,
    SUsize,
}

impl Sizes {
    pub fn to_byte_size(&self) -> usize {
        match self {
            Self::S8 => 1,
            Self::S16 => 2,
            Self::S32 => 4,
            Self::S64 => 8,
            Self::SUsize => {
                println!("Warning line 177 of common.rs");
                8
            }
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum BuiltinType {
    Int(bool /* signed */, Sizes),
    Bool,
}

impl BuiltinType {
    pub fn is_int(&self) -> bool {
        matches!(self, Self::Int(_, _))
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, Self::Bool)
    }

    pub fn to_byte_size(&self) -> usize {
        match self {
            BuiltinType::Bool => 1,
            BuiltinType::Int(_, size) => size.to_byte_size(),
        }
    }

    pub fn to_str(&self) -> &'static str {
        match self {
            BuiltinType::Int(b, size) => match (b, size) {
                (true, Sizes::S8) => "i8",
                (false, Sizes::S8) => "u8",
                (true, Sizes::S16) => "i16",
                (false, Sizes::S16) => "u16",
                (true, Sizes::S32) => "i32",
                (false, Sizes::S32) => "u32",
                (true, Sizes::S64) => "i64",
                (false, Sizes::S64) => "u64",
                (true, Sizes::SUsize) => "isize",
                (false, Sizes::SUsize) => "usize",
            },
            BuiltinType::Bool => "bool",
        }
    }
}

#[derive(Debug, Clone)]
pub enum Projector {
    Int(usize),
    Name(Ident),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOperator {
    Add,
    Sub,
    Mul,
    Mod,
    Div,
    Eq,
    Ne,
    Lower,
    LowerEq,
    Greater,
    GreaterEq,
    Set,
    And,
    Or,
    BitAnd,
    BitOr,
    Shr,
    Shl,
}

impl BinOperator {
    pub fn get_trait_name(self) -> (&'static str, &'static str) {
        match self {
            Self::Add => ("Add", "add"),
            Self::Sub => ("Sub", "sub"),
            Self::Div => ("Div", "div"),
            Self::Mod => ("Mod", "mod"),
            Self::Mul => ("Mul", "mul"),

            Self::Shl => ("Shl", "shl"),
            Self::Shr => ("Shr", "shr"),
            Self::BitAnd => ("BitAnd", "bit_and"),
            Self::BitOr => ("BitOr", "bit_or"),

            Self::And => ("And", "and"),
            Self::Or => ("Or", "or"),

            Self::Eq => ("PartialEq", "eq"),
            Self::Ne => ("PartialEq", "ne"),

            Self::Greater => ("PartialOrd", "gr"),
            Self::GreaterEq => ("PartialOrd", "ge"),
            Self::Lower => ("PartialOrd", "lo"),
            Self::LowerEq => ("PartialOrd", "le"),

            Self::Set => todo!(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UnaOperator {
    Not,
    Neg,
}

impl UnaOperator {
    pub fn get_trait_name(self) -> (&'static str, &'static str) {
        match self {
            Self::Neg => ("Neg", "neg"),
            Self::Not => ("Not", "not"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct IdCounter {
    id: usize,
}

impl IdCounter {
    pub fn new() -> Self {
        Self { id: 0 }
    }

    pub fn incr(&mut self) -> usize {
        let i = self.id;
        self.id += 1;
        i
    }

    pub fn new_name(&mut self) -> String {
        format!("@{}", self.incr())
    }
}

impl Default for IdCounter {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ComputedValue {
    Drop,
    Keep,
}

#[derive(Clone)]
pub struct ErrorReporter {
    lines: Vec<String>,
    lines_index: Vec<usize>,
    file_name: String,
}

impl ErrorReporter {
    pub fn empty() -> Self {
        Self {
            lines: Vec::new(),
            lines_index: Vec::new(),
            file_name: String::new(),
        }
    }

    pub fn new(file_name: String, file: String) -> Self {
        let mut vec_str = Vec::new();
        let mut count = 0;
        let mut vec_index = vec![count];
        let mut current_str = String::new();
        for char in file.chars() {
            count += 1;
            current_str.push(char);
            if char == '\n' {
                vec_str.push(current_str);
                vec_index.push(count);
                current_str = String::new();
            };
        }
        vec_str.push(current_str);
        Self {
            lines: vec_str,
            lines_index: vec_index,
            file_name,
        }
    }

    fn get_line_id(&self, loc: usize) -> usize {
        let mut fst = 0;
        let mut last = self.lines_index.len();
        while fst + 1 < last {
            let mid = (fst + last) / 2;
            if self.lines_index[mid] <= loc {
                fst = mid
            } else {
                last = mid
            }
        }
        fst
    }

    pub fn get_fst_line_id(&self, loc: Location) -> usize {
        self.get_line_id(loc.start())
    }

    pub fn get_last_line_id(&self, loc: Location) -> usize {
        self.get_line_id(loc.end())
    }

    pub fn get_line(&self, id: usize) -> Option<&String> {
        self.lines.get(id)
    }

    pub fn get_line_start_char(&self, id: usize) -> Option<&usize> {
        self.lines_index.get(id)
    }

    pub fn get_file_name(&self) -> &String {
        &self.file_name
    }

    pub fn report(&self, errs: Vec<TypeError>) -> ! {
        for err in errs.into_iter() {
            err.report_error(self);
        }
        std::process::exit(1);
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum TypedBinop {
    Add(Sizes),
    Mul(bool, Sizes),
    Sub(Sizes),
    Div(bool, Sizes),
    Mod(bool, Sizes),
    And(Sizes),
    Or(Sizes),
    Shl(Sizes),
    Shr(Sizes),
    Eq(Sizes),
    Neq(Sizes),
    Lower(bool, Sizes),
    LowerEq(bool, Sizes),
    Greater(bool, Sizes),
    GreaterEq(bool, Sizes),
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum TypedUnaop {
    Not(Sizes),
    Neg(Sizes),
}
