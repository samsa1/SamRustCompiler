use crate::ast::asm::{ImmOrReg, Registers};
use crate::ast::common::Sizes;
use crate::ast::operators::{Cmp, CmpDesc, LArith};
use write_x86_64::*;

impl Sizes {
    pub fn cmp(&self, op1: ImmOrReg, r: Registers) -> Text {
        match self {
            Sizes::S8 => cmpb(op1.b(), reg!(r.b())),
            Sizes::S16 => cmpw(op1.w(), reg!(r.w())),
            Sizes::S32 => cmpl(op1.l(), reg!(r.l())),
            Sizes::S64 | Sizes::SUsize => cmpq(op1.q(), reg!(r.q())),
        }
    }
}

impl Cmp {
    pub fn get_cond(&self) -> instr::Cond {
        match self.cmp {
            CmpDesc::Eq => instr::Cond::Z,
            CmpDesc::Neq => instr::Cond::NZ,
            CmpDesc::Lower => {
                if self.signed {
                    instr::Cond::L
                } else {
                    instr::Cond::B
                }
            }
            CmpDesc::LowerEq => {
                if self.signed {
                    instr::Cond::LE
                } else {
                    instr::Cond::BE
                }
            }
            CmpDesc::Greater => {
                if self.signed {
                    instr::Cond::G
                } else {
                    instr::Cond::A
                }
            }
            CmpDesc::GreaterEq => {
                if self.signed {
                    instr::Cond::GE
                } else {
                    instr::Cond::AE
                }
            }
        }
    }

    pub fn to_bin(&self, v: ImmOrReg, op2: Registers, out: Registers) -> Segment<instr::Instr> {
        self.size.cmp(v, op2) + set(self.get_cond(), reg!(out.b()))
    }
}

impl LArith {
    pub fn to_bin(&self, v: ImmOrReg, r: Registers) -> Text {
        match self {
            Self::Add(Sizes::S8) => addb(v.b(), reg!(r.b())),
            Self::Add(Sizes::S16) => addw(v.w(), reg!(r.w())),
            Self::Add(Sizes::S32) => addl(v.l(), reg!(r.l())),
            Self::Add(Sizes::S64 | Sizes::SUsize) => addq(v.q(), reg!(r.q())),
            Self::Sub(Sizes::S8) => subb(v.b(), reg!(r.b())),
            Self::Sub(Sizes::S16) => subw(v.w(), reg!(r.w())),
            Self::Sub(Sizes::S32) => subl(v.l(), reg!(r.l())),
            Self::Sub(Sizes::S64 | Sizes::SUsize) => subq(v.q(), reg!(r.q())),
            Self::And(Sizes::S8) => andb(v.b(), reg!(r.b())),
            Self::And(Sizes::S16) => andw(v.w(), reg!(r.w())),
            Self::And(Sizes::S32) => andl(v.l(), reg!(r.l())),
            Self::And(Sizes::S64 | Sizes::SUsize) => andq(v.q(), reg!(r.q())),
            Self::Or(Sizes::S8) => orb(v.b(), reg!(r.b())),
            Self::Or(Sizes::S16) => orw(v.w(), reg!(r.w())),
            Self::Or(Sizes::S32) => orl(v.l(), reg!(r.l())),
            Self::Or(Sizes::S64 | Sizes::SUsize) => orq(v.q(), reg!(r.q())),
        }
    }
}

pub fn remove_pad(pad: u64) -> Text {
    if pad == 0 {
        Text::empty()
    } else {
        addq(immq(pad as i64), reg!(RSP))
    }
}
