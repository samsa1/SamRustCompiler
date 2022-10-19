use std::str::FromStr;

pub enum BinOp {
    Eq,
    NotEq,
    Lower,
    LowerEq,
    Greater,
    GreaterEq,
    Add,
    Sub,
    Mult,
    Div,
    Modulo,
    And,
    Or,
}

pub enum UnOp {
    Neg,
    Not,
    Ref(bool),
    Deref,
}

#[derive(Debug, Clone)]
pub enum NamePath<T> {
    Name(Ident),
    Specialisation(Vec<T>),
}

#[derive(Debug, Clone)]
pub struct Path<T> {
    name: Vec<NamePath<T>>,
    loc: Location,
}

impl<T> Path<T> {
    pub fn new(name: Vec<NamePath<T>>, loc: Location) -> Self {
        Self { name, loc }
    }

    pub fn get_content(&self) -> &Vec<NamePath<T>> {
        &self.name
    }

    pub fn content(self) -> Vec<NamePath<T>> {
        self.name
    }

    pub fn get_loc(&self) -> Location {
        self.loc
    }

    pub fn last(&self) -> Option<Ident> {
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

    pub fn to_path(&self) -> Path<super::rust::PreType> {
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

impl std::hash::Hash for Ident {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
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
}

impl BinOperator {
    pub fn get_trait_name(self) -> (&'static str, &'static str) {
        match self {
            Self::Add => ("Add", ""),
            Self::Sub => ("Sub", ""),
            Self::Div => ("Div", ""),
            Self::Mod => ("Mod", ""),
            Self::Mul => ("Mul", ""),

            Self::And => ("And", ""),
            Self::Or => ("Or", ""),

            Self::Eq => ("PartialEq", "_eq"),
            Self::Ne => ("PartialEq", "_ne"),

            Self::Greater => ("PartialOrd", "_gr"),
            Self::GreaterEq => ("PartialOrd", "_ge"),
            Self::Lower => ("PartialOrd", "_lo"),
            Self::LowerEq => ("PartialOrd", "_le"),

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
            Self::Neg => ("Neg", ""),
            Self::Not => ("Not", ""),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ComputedValue {
    Drop,
    Keep,
}

pub struct ErrorReporter {
    lines: Vec<String>,
    lines_index: Vec<usize>,
    file_name: String,
}

impl ErrorReporter {
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
