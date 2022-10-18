use super::reg::{Label, Operand, Reg, RegQ};
use std::io::prelude::*;

pub trait Instr {
    fn write_in(&self, file : &mut std::fs::File) -> std::io::Result<()>;
}

pub enum OpOpInstrName {
    Move,
    Add,
    Sub,
    And,
    Or,
    Xor,
    Shl,
    Shr,
    Sar,
    Cmp,
    Test,
    Lea,
    IMul,
}

impl OpOpInstrName {
    pub fn to_str(&self) -> &'static str {
        match self {
            Self::Move => "mov",
            Self::Add => "add",
            Self::Sub => "sub",
            Self::And => "and",
            Self::Or =>  "or",
            Self::Xor => "xor",
            Self::Shl => "shl",
            Self::Shr => "shr",
            Self::Sar => "sar",
            Self::Cmp => "cmp",
            Self::Test => "test",
            Self::Lea => "lea",
            Self::IMul => "imul",
        }
    }
}

pub struct InstrOpOp<T: Reg> {
    instr: OpOpInstrName,
    reg1: Operand<T>,
    reg2: Operand<T>,
}

impl<T: Reg> InstrOpOp<T> {
    pub fn new(instr: OpOpInstrName, reg1: Operand<T>, reg2: Operand<T>) -> Self {
        Self { instr, reg1, reg2 }
    }
}

impl<T: Reg> Instr for InstrOpOp<T> {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        file.write_all(b"\t")?;
        file.write_all(self.instr.to_str().as_bytes())?;
        file.write_all(&[T::SIZE.to_char() as u8])?;
        file.write_all(b" ")?;
        self.reg1.write_in(file)?;
        file.write_all(b", ")?;
        self.reg2.write_in(file)?;
        file.write_all(b"\n")
    }
}

/*pub enum OpRegInstrName {
    Lea,
}

impl OpRegInstrName {
    pub fn to_str(&self) -> &'static str {
        match self {
            Lea => "lea",
        }
    }
}

pub struct InstrOpReg<T: Reg> {
    instr: OpRegInstrName,
    reg1: Operand<T>,
    reg2: T,
}

impl<T: Reg> Instr for InstrOpReg<T> {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        file.write_all(b"\t")?;
        file.write_all(self.instr.to_str().as_bytes())?;
        file.write_all(&[T::SIZE.to_char() as u8])?;
        file.write_all(b" ")?;
        self.reg1.write_in(file)?;
        file.write_all(b", ")?;
        self.reg2.write_in(file)?;
        file.write_all(b"\n")
    }
}*/

pub enum OpInstrName {
    Inc,
    Dec,
    Neg,
    Not,
    Push,
    Pop,
    UnsignedDiv,
    SignedDiv,
}

impl OpInstrName {
    pub fn to_str(&self) -> &'static str {
        match self {
            Self::Inc => "inc",
            Self::Dec => "dec",
            Self::Neg => "neg",
            Self::Not => "not",
            Self::Push => "push",
            Self::Pop => "pop",
            Self::UnsignedDiv => "div",
            Self::SignedDiv => "idiv",
        }
    }
}

pub struct InstrOp<T: Reg> {
    instr: OpInstrName,
    reg: Operand<T>,
}

impl<T: Reg> InstrOp<T> {
    pub fn new(instr: OpInstrName, reg: Operand<T>) -> Self {
        Self { instr, reg }
    }
}

impl<T: Reg> Instr for InstrOp<T> {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        file.write_all(b"\t")?;
        file.write_all(self.instr.to_str().as_bytes())?;
        file.write_all(&[T::SIZE.to_char() as u8])?;
        file.write_all(b" ")?;
        self.reg.write_in(file)?;
        file.write_all(b"\n")
    }
}

pub enum InstrNoArg {
    Ret,
    Leave,
    Syscall,
    Hlt,
    Cltd,
    Cqto,
}

impl Instr for InstrNoArg {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        file.write_all(b"\t")?;
        match self {
            Self::Ret => file.write_all(b"ret\n"),
            Self::Leave => file.write_all(b"leave\n"),
            Self::Syscall => file.write_all(b"syscall\n"),
            Self::Hlt => file.write_all(b"hlt\n"),
            Self::Cltd => file.write_all(b"cltd\n"),
            Self::Cqto => file.write_all(b"cqto\n"),
        }
    }
}

pub enum Cond {
    JE,
    JZ,
    JNE,
    JNZ,
    JS,
    JNS,
    JG,
    JGE,
    JL,
    JLE,
    JA,
    JAE,
    JB,
    JBE,
}

impl Cond {
    fn to_str(&self) -> &'static str {
        match self {
            Self::JE => "e",
            Self::JZ => "z",
            Self::JNE => "ne",
            Self::JNZ => "nz",
            Self::JS => "s",
            Self::JNS => "ns",
            Self::JG => "g",
            Self::JGE => "ge",
            Self::JL => "l",
            Self::JLE => "le",
            Self::JA => "a",
            Self::JAE => "ae",
            Self::JB => "b",
            Self::JBE => "be",
        }

    }
}

pub struct CondMove<T : Reg> {
    cond : Cond,
    reg1 : Operand<T>,
    reg2 : Operand<T>
}

impl<T : Reg> CondMove<T> {
    pub fn new(cond : Cond, reg1 : Operand<T>, reg2 : Operand<T>) -> Self {
        Self {
            cond,
            reg1,
            reg2,
        }
    }
}

impl<T : Reg> Instr for CondMove<T> {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        file.write_all(b"\tcmov")?;
        file.write_all(self.cond.to_str().as_bytes())?;
        file.write_all(b" ")?;
        self.reg1.write_in(file)?;
        file.write_all(b", ")?;
        self.reg2.write_in(file)?;
        file.write_all(b"\n")
    }
}

pub enum Goto {
    Call(Label),
    CallStar(Operand<RegQ>),
    CondJump(Cond, Label),
    Jump(Label),
}


impl Instr for Goto {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        file.write_all(b"\t")?;
        match self {
            Self::Call(label) => {
                file.write_all(b"call ")?;
                label.write_in(file)?
            },
            Self::CallStar(operand) => {
                file.write_all(b"call *")?;
                operand.write_in(file)?
            },
            Self::CondJump(cond, label) => {
                file.write_all(b"j")?;
                file.write_all(cond.to_str().as_bytes())?;
                file.write_all(b" ")?;
                label.write_in(file)?
            }
            Self::Jump(label) => {
                file.write_all(b"jmp ")?;
                label.write_in(file)?
            }
        }
        file.write_all(b"\n")
    }
}
