use super::reg::{Label, Operand, Reg, RegQ};

pub trait Instr {
    fn to_string(&self) -> String;
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
}

impl OpOpInstrName {
    pub fn to_str(&self) -> &'static str {
        match self {
            Self::Move => "mov",
            Self::Add => "add",
            Self::Sub => "sub",
            _ => todo!(),
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
    fn to_string(&self) -> String {
        let mut name = String::new();
        name.push('\t');
        name.push_str(self.instr.to_str());
        name.push(T::SIZE.to_char());
        name.push(' ');
        name.push_str(&self.reg1.to_string());
        name.push_str(", ");
        name.push_str(&self.reg2.to_string());
        name.push('\n');
        name
    }
}

pub enum OpRegInstrName {
    Lea,
}

impl OpRegInstrName {
    pub fn to_str(&self) -> &'static str {
        match self {
            _ => todo!(),
        }
    }
}

pub struct InstrOpReg<T: Reg> {
    instr: OpRegInstrName,
    reg1: Operand<T>,
    reg2: T,
}

impl<T: Reg> Instr for InstrOpReg<T> {
    fn to_string(&self) -> String {
        let mut name = String::new();
        name.push('\t');
        name.push_str(self.instr.to_str());
        name.push(T::SIZE.to_char());
        name.push(' ');
        name.push_str(&self.reg1.to_string());
        name.push_str(", ");
        name.push_str(&self.reg2.to_string());
        name.push('\n');
        name
    }
}

pub enum OpInstrName {
    Incr,
    Decr,
    Neg,
    Not,
    Push,
    Pop,
}

impl OpInstrName {
    pub fn to_str(&self) -> &'static str {
        match self {
            _ => todo!(),
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
    fn to_string(&self) -> String {
        let mut name = String::new();
        name.push('\t');
        name.push_str(self.instr.to_str());
        name.push(T::SIZE.to_char());
        name.push(' ');
        name.push_str(&self.reg.to_string());
        name.push('\n');
        name
    }
}

pub enum InstrNoArg {
    Ret,
    Leave,
    Syscall,
    Hlt,
}

impl Instr for InstrNoArg {
    fn to_string(&self) -> String {
        let mut name = String::new();
        todo!();
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

pub enum Goto {
    Call(Label),
    Call_star(Operand<RegQ>),
    CondJump(Cond, Label),
    Jump(Label),
}

impl Instr for Goto {
    fn to_string(&self) -> String {
        todo!()
    }
}
