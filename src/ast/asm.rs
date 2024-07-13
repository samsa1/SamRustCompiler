use write_x86_64::*;
use super::low_level_repr::Value;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Registers {
    RegA,
    RegC,
    RegD,
}

impl Registers {
    pub fn q(&self) -> reg::RegQ {
        match self {
            Self::RegA => RAX,
            Self::RegC => RCX,
            Self::RegD => RDX,
        }
    }

    pub fn l(&self) -> reg::RegL {
        match self {
            Self::RegA => EAX,
            Self::RegC => ECX,
            Self::RegD => EDX,
        }
    }

    pub fn w(&self) -> reg::RegW {
        match self {
            Self::RegA => AX,
            Self::RegC => CX,
            Self::RegD => DX,
        }
    }

    pub fn b(&self) -> reg::RegB {
        match self {
            Self::RegA => AL,
            Self::RegC => CL,
            Self::RegD => DL,
        }
    }
}

impl Value {
    pub fn q(&self) -> reg::Operand<reg::RegQ> {
        match self {
            Value::SInt(i, _) => immq(*i as i64),
            Value::UInt(i, _) => immq(*i as i64),
            Value::Bool(true)       => immq(1),
            Value::Bool(false)      => immq(0),
        }
    }

    pub fn l(&self) -> reg::Operand<reg::RegL> {
        match self {
            Value::SInt(i, _) => imml(*i as i32),
            Value::UInt(i, _) => imml(*i as i32),
            Value::Bool(true)       => imml(1),
            Value::Bool(false)      => imml(0),
        }
    }

    pub fn w(&self) -> reg::Operand<reg::RegW> {
        match self {
            Value::SInt(i, _) => immw(*i as i16),
            Value::UInt(i, _) => immw(*i as i16),
            Value::Bool(true)       => immw(1),
            Value::Bool(false)      => immw(0),
        }
    }

    pub fn b(&self) -> reg::Operand<reg::RegB> {
        match self {
            Value::SInt(i, _) => immb(*i as i8),
            Value::UInt(i, _) => immb(*i as i8),
            Value::Bool(true)       => immb(1),
            Value::Bool(false)      => immb(0),
        }
    }

}

pub enum ImmOrReg {
    V(Value),
    R(Registers),
}

impl ImmOrReg {
    pub fn q(&self) -> reg::Operand<reg::RegQ>  {
        match self {
            Self::R(r) => reg!(r.q()),
            Self::V(v) => v.q(),
        }
    }

    pub fn l(&self) -> reg::Operand<reg::RegL>  {
        match self {
            Self::R(r) => reg!(r.l()),
            Self::V(v) => v.l(),
        }
    }


    pub fn w(&self) -> reg::Operand<reg::RegW>  {
        match self {
            Self::R(r) => reg!(r.w()),
            Self::V(v) => v.w(),
        }
    }

    pub fn b(&self) -> reg::Operand<reg::RegB>  {
        match self {
            Self::R(r) => reg!(r.b()),
            Self::V(v) => v.b(),
        }
    }
}
