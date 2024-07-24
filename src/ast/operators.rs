use super::common::Sizes;

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum LArith {
    Add(Sizes),
    Sub(Sizes),
    And(Sizes),
    Or(Sizes),
}

impl LArith {
    pub fn commutes(&self) -> bool {
        match self {
            Self::Sub(_) => false,
            _ => true,
        }
    }

    pub fn size(&self) -> Sizes {
        match self {
            Self::Add(s) | Self::Sub(s) | Self::And(s) | Self::Or(s) => *s,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum CmpDesc {
    Eq,
    Neq,
    Lower,
    LowerEq,
    Greater,
    GreaterEq,
}

impl CmpDesc {
    fn rev(self) -> Self {
        match self {
            Self::Eq => Self::Eq,
            Self::Neq => Self::Neq,
            Self::Lower => Self::Greater,
            Self::LowerEq => Self::LowerEq,
            Self::Greater => Self::Lower,
            Self::GreaterEq => Self::LowerEq,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub struct Cmp {
    pub cmp: CmpDesc,
    pub signed: bool,
    pub size: Sizes,
}

impl Cmp {
    pub fn cond_rev(mut self, b: bool) -> Self {
        if b {
            self.cmp = self.cmp.rev()
        }
        self
    }

    pub fn new(cmp: CmpDesc, signed: bool, size: Sizes) -> Self {
        Self { cmp, signed, size }
    }

    pub fn eq(size: Sizes) -> Self {
        Self {
            cmp: CmpDesc::Eq,
            signed: false,
            size,
        }
    }

    pub fn neq(size: Sizes) -> Self {
        Self {
            cmp: CmpDesc::Neq,
            signed: false,
            size,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum HArithDesc {
    Div,
    Mod,
    Mul,
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub struct HArith {
    pub dm: HArithDesc,
    pub signed: bool,
    pub size: Sizes,
}

impl HArith {
    pub fn new(dm: HArithDesc, signed: bool, size: Sizes) -> Self {
        Self { dm, signed, size }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum Logic {
    LAnd,
    LOr,
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum Shift {
    Shl(Sizes),
    Shr(Sizes),
}

impl Shift {
    pub fn size(&self) -> Sizes {
        match self {
            Self::Shl(s) | Self::Shr(s) => *s,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum TBinop {
    LArith(LArith),
    Cmp(Cmp),
    HArith(HArith),
    Logic(Logic),
    Shift(Shift),
}
impl TBinop {
    pub fn can_unary(&self) -> bool {
        match self {
            Self::Logic(_) => false,
            _ => true,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum TUnaop {
    Not(Sizes),
    Neg(Sizes),
}

impl TUnaop {
    pub fn size(&self) -> Sizes {
        match self {
            Self::Neg(s) | Self::Not(s) => *s,
        }
    }
}
