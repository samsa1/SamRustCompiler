
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
    name : String,
    loc : Location,
}

impl Ident {
    pub fn get_content(&self) -> &str {
        &self.name
    }

    pub fn content(self) -> String {
        self.name
    }

    pub fn from_str(s : &str) -> Self {
        Self {
            name : s.to_string(),
            loc : Location::default(),
        }
    }

    pub fn new(s:&str, loc:Location) -> Self {
        Self {
            name : s.to_string(),
            loc : loc,
        }
    }

    pub fn new_from(name : String, start : usize, end : usize) -> Self {
        Self {
            name,
            loc : Location::new(start, end),
        }
    }

    pub fn get_loc(&self) -> &Location {
        &self.loc
    }

}

impl PartialEq for Ident {
    fn eq(&self, other : &Self) -> bool {
        self.name == other.name
    }

    fn ne(&self, other : &Self) -> bool {
        self.name != other.name
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Location {
    start : usize,
    end : usize,
}

impl Location {
    pub fn default() -> Self {
        Self {
            start : usize::MAX,
            end : usize::MAX,
        }
    }

    pub fn start(&self) -> usize {
        self.start
    }

    pub fn end(&self) -> usize {
        self.end
    }

    pub fn new(start : usize, end : usize) -> Self {
        Self {
            start,
            end,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Sizes {
    S32,
    S64,
    SUsize,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BuiltinType {
    Int(bool /* signed */, Sizes),
    Bool,
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
    Times,
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

#[derive(Debug, Clone, Copy)]
pub enum UnaOperator {
    LogicalNeg,
    ArithNeg,
}
