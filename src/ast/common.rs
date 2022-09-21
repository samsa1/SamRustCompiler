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

#[derive(Clone, Debug)]
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

    pub fn get_loc(&self) -> &Location {
        &self.loc
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

#[derive(Debug, Copy, Clone)]
pub struct Location {
    start: usize,
    end: usize,
}

impl Location {
    pub fn default() -> Self {
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
    S32,
    S64,
    SUsize,
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
}

#[derive(Debug)]
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
