use crate::ast::asm::{ImmOrReg, Registers};
use crate::ast::common::{BuiltinType, Sizes};
use crate::ast::low_level_repr::Value;
use crate::ast::operators::{Cmp, CmpDesc, HArith, HArithDesc, LArith, Shift, TUnaop};
use reg::RegQ;
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
    pub fn mov(&self, r1: Registers, r2: Registers) -> Text {
        match self {
            Sizes::S8 => movb(reg!(r1.b()), reg!(r2.b())),
            Sizes::S16 => movw(reg!(r1.w()), reg!(r2.w())),
            Sizes::S32 => movl(reg!(r1.l()), reg!(r2.l())),
            Sizes::S64 | Sizes::SUsize => movq(reg!(r1.q()), reg!(r2.q())),
        }
    }

    pub fn mov_addr(&self, r1: Registers, offset: i64, reg: RegQ) -> Text {
        match self {
            Sizes::S8 => movb(reg!(r1.b()), addr!(offset, reg)),
            Sizes::S16 => movw(reg!(r1.w()), addr!(offset, reg)),
            Sizes::S32 => movl(reg!(r1.l()), addr!(offset, reg)),
            Sizes::S64 | Sizes::SUsize => movq(reg!(r1.q()), addr!(offset, reg)),
        }
    }

    pub fn addr_mov(&self, offset: i64, reg: RegQ, r2: Registers) -> Text {
        match self {
            Sizes::S8 => movb(addr!(offset, reg), reg!(r2.b())),
            Sizes::S16 => movw(addr!(offset, reg), reg!(r2.w())),
            Sizes::S32 => movl(addr!(offset, reg), reg!(r2.l())),
            Sizes::S64 | Sizes::SUsize => movq(addr!(offset, reg), reg!(r2.q())),
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

impl TUnaop {
    pub fn to_bin(&self, r: Registers) -> Text {
        match self {
            TUnaop::Neg(Sizes::S8) => negb(reg!(r.b())),
            TUnaop::Neg(Sizes::S16) => negw(reg!(r.w())),
            TUnaop::Neg(Sizes::S32) => negl(reg!(r.l())),
            TUnaop::Neg(Sizes::S64) => negq(reg!(r.q())),
            TUnaop::Neg(Sizes::SUsize) => negq(reg!(r.q())),
            TUnaop::Not(Sizes::S8) => xorb(immb(1), reg!(r.b())),
            TUnaop::Not(Sizes::S16) => xorw(immw(1), reg!(r.w())),
            TUnaop::Not(Sizes::S32) => xorl(imml(1), reg!(r.l())),
            TUnaop::Not(Sizes::S64) => xorq(immq(1), reg!(r.q())),
            TUnaop::Not(Sizes::SUsize) => xorq(immq(1), reg!(r.q())),
        }
    }
}

impl Value {
    pub fn to_reg(&self, reg: Registers) -> Text {
        match self.size() {
            Sizes::S8 => movb(self.b(), reg!(reg.b())),
            Sizes::S16 => movw(self.w(), reg!(reg.w())),
            Sizes::S32 => movl(self.l(), reg!(reg.l())),
            Sizes::S64 | Sizes::SUsize => movq(self.q(), reg!(reg.q())),
        }
    }
}

impl Shift {
    pub fn reg(&self, r: Registers) -> Text {
        match self {
            Self::Shr(Sizes::S8) => shrb_reg(reg!(r.b())),
            Self::Shr(Sizes::S16) => shrw_reg(reg!(r.w())),
            Self::Shr(Sizes::S32) => shrl_reg(reg!(r.l())),
            Self::Shr(Sizes::S64 | Sizes::SUsize) => shrq_reg(reg!(r.q())),
            Self::Shl(Sizes::S8) => shlb_reg(reg!(r.b())),
            Self::Shl(Sizes::S16) => shlw_reg(reg!(r.w())),
            Self::Shl(Sizes::S32) => shll_reg(reg!(r.l())),
            Self::Shl(Sizes::S64 | Sizes::SUsize) => shlq_reg(reg!(r.q())),
        }
    }

    pub fn imm(&self, _r: Registers, _imm: usize) -> Text {
        todo!()
        // match self {
        //     Self::Shl(Sizes::S8) => shlb(reg1, reg2)
        // }
    }
}

impl HArith {
    // Expect value 1 in RegA
    pub fn to_bin(&self, r2: Registers) -> (Text, Registers) {
        match (self.dm, self.size) {
            (_, Sizes::S8) => todo!(),
            (_, Sizes::S16) => todo!(),
            (HArithDesc::Mul, Sizes::S32) => {
                (
                    if self.signed {
                        imull(reg!(r2.l()), reg!(EAX))
                    } else {
                        // TODO use unsigned multiply
                        imull(reg!(r2.l()), reg!(EAX))
                    },
                    Registers::RegA,
                )
            }
            (HArithDesc::Mul, Sizes::S64 | Sizes::SUsize) => {
                (
                    if self.signed {
                        imulq(reg!(r2.q()), reg!(RAX))
                    } else {
                        // TODO use unsigned multiply
                        imulq(reg!(r2.q()), reg!(RAX))
                    },
                    Registers::RegA,
                )
            }
            (HArithDesc::Div | HArithDesc::Mod, Sizes::S32) => {
                let asm = leaq(
                    reg::Operand::LabRelAddr(reg::Label::from_str(
                        "division_by_zero_str".to_string(),
                    )),
                    R12,
                ) + testl(reg!(r2.l()), reg!(r2.l()))
                    + jz(reg::Label::panic())
                    + if self.signed {
                        cltd() + idivl(reg!(r2.l()))
                    } else {
                        xorl(reg!(EDX), reg!(EDX)) + divl(reg!(r2.l()))
                    };
                let pos = match self.dm {
                    HArithDesc::Div => Registers::RegA,
                    HArithDesc::Mod => Registers::RegD,
                    HArithDesc::Mul => panic!("ICE"),
                };
                (asm, pos)
            }
            (HArithDesc::Div | HArithDesc::Mod, Sizes::S64 | Sizes::SUsize) => {
                let asm = leaq(
                    reg::Operand::LabRelAddr(reg::Label::from_str(
                        "division_by_zero_str".to_string(),
                    )),
                    R12,
                ) + testq(reg!(r2.q()), reg!(r2.q()))
                    + jz(reg::Label::panic())
                    + if self.signed {
                        cqto() + idivq(reg!(r2.q()))
                    } else {
                        xorq(reg!(RDX), reg!(RDX)) + divq(reg!(r2.q()))
                    };
                let pos = match self.dm {
                    HArithDesc::Div => Registers::RegA,
                    HArithDesc::Mod => Registers::RegD,
                    HArithDesc::Mul => panic!("ICE"),
                };
                (asm, pos)
            }
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

pub fn mov_struct(
    reg_in: reg::RegQ,
    offset_in: i64,
    reg_out: reg::RegQ,
    offset_out: i64,
    mut size: u64,
    free_reg: Registers,
) -> Segment<instr::Instr> {
    let mut offset = 0;
    let mut asm = Segment::empty();
    while size >= 8 {
        asm = asm
            + movq(addr!(offset_in + offset, reg_in), reg!(free_reg.q()))
            + movq(reg!(free_reg.q()), addr!(offset_out + offset, reg_out));
        size -= 8;
        offset += 8;
    }

    while size >= 4 {
        asm = asm
            + movl(addr!(offset_in + offset, reg_in), reg!(free_reg.l()))
            + movl(reg!(free_reg.l()), addr!(offset_out + offset, reg_out));
        size -= 4;
        offset += 4;
    }

    while size >= 2 {
        asm = asm
            + movw(addr!(offset_in + offset, reg_in), reg!(free_reg.w()))
            + movw(reg!(free_reg.w()), addr!(offset_out + offset, reg_out));
        size -= 2;
        offset += 2;
    }

    while size >= 1 {
        asm = asm
            + movb(addr!(offset_in + offset, reg_in), reg!(free_reg.b()))
            + movb(reg!(free_reg.b()), addr!(offset_out + offset, reg_out));
        size -= 1;
        offset += 1;
    }
    asm
}

pub fn coercion(rin: Registers, typ1: BuiltinType, rout: Registers, typ2: BuiltinType) -> Text {
    match (typ1, typ2) {
        (t1, t2) if t1 == t2 => Text::empty(),
        (BuiltinType::Int(true, s1), BuiltinType::Int(_, s2)) => match (s1, s2) {
            (Sizes::S8, Sizes::S8)
            | (Sizes::S16, Sizes::S16)
            | (Sizes::S32, Sizes::S32)
            | (Sizes::S64, Sizes::S64)
            | (Sizes::S64, Sizes::SUsize)
            | (Sizes::SUsize, Sizes::S64)
            | (Sizes::SUsize, Sizes::SUsize) => nop(),
            (Sizes::S8, Sizes::S16) => movsbw(reg!(rin.b()), rout.w()),
            (Sizes::S8, Sizes::S32) => movsbl(reg!(rin.b()), rout.l()),
            (Sizes::S8, Sizes::S64) | (Sizes::S8, Sizes::SUsize) => movsbq(reg!(rin.b()), rout.q()),
            (Sizes::S16, Sizes::S32) => movswl(reg!(rin.w()), rout.l()),
            (Sizes::S16, Sizes::S64) | (Sizes::S16, Sizes::SUsize) => {
                movswq(reg!(rin.w()), rout.q())
            }
            (Sizes::S32, Sizes::S64) | (Sizes::S32, Sizes::SUsize) => {
                movslq(reg!(rin.l()), rout.q())
            }

            _ => {
                assert!(s1.to_byte_size() > s2.to_byte_size());
                nop()
            }
        },
        (BuiltinType::Int(false, s1), BuiltinType::Int(_, s2)) => {
            match (s1, s2) {
                (Sizes::S8, Sizes::S8)
                | (Sizes::S16, Sizes::S16)
                | (Sizes::S32, Sizes::S32)
                | (Sizes::S64, Sizes::S64)
                | (Sizes::S64, Sizes::SUsize)
                | (Sizes::SUsize, Sizes::S64)
                | (Sizes::SUsize, Sizes::SUsize) => nop(),
                (Sizes::S8, Sizes::S16) => movzbw(reg!(rin.b()), rout.w()),
                (Sizes::S8, Sizes::S32) => movzbl(reg!(rin.b()), rout.l()),
                (Sizes::S8, Sizes::S64) | (Sizes::S8, Sizes::SUsize) => {
                    movzbq(reg!(rin.b()), rout.q())
                }
                (Sizes::S16, Sizes::S32) => movzwl(reg!(rin.w()), rout.l()),
                (Sizes::S16, Sizes::S64) | (Sizes::S16, Sizes::SUsize) => {
                    movzwq(reg!(rin.w()), rout.q())
                }
                (Sizes::S32, Sizes::S64) | (Sizes::S32, Sizes::SUsize) => {
                    // this instruction should fill top bytes of RAX with zeros
                    movl(reg!(rin.l()), reg!(rout.l()))
                }
                _ => {
                    println!("{s1:?} {s2:?}");
                    assert!(s1.to_byte_size() > s2.to_byte_size());
                    nop()
                }
            }
        }
        (t1, t2) => {
            println!("Coercion {:?} {:?} to do", t1, t2);
            todo!()
        }
    }
}
