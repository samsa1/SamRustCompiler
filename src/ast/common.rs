
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

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub struct Ident(String);

impl Ident {
    pub fn get_content(&self) -> &str {
        &self.0
    }

    pub fn from_str(s : &str) -> Self {
        Self(s.to_string())
    }
}

pub struct Location {

}

impl Location {
    pub fn default() -> Self {
        Self {

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
pub enum BuildinType {
    Int(bool /* signed */, Sizes),
    Bool,
}

pub enum Projector {
    Int(usize),
    Name(Ident),
}
