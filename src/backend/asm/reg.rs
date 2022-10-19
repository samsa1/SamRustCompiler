use std::io::prelude::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Sizes {
    Byte,
    Word,
    Long,
    Quad,
}

impl Sizes {
    pub fn to_char(&self) -> char {
        match self {
            Self::Byte => 'b',
            Self::Word => 'w',
            Self::Long => 'l',
            Self::Quad => 'q',
        }
    }
}

pub trait Reg {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()>;

    const SIZE: Sizes;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RegQ {
    Rax,
    Rbx,
    Rcx,
    Rdx,
    Rsi,
    Rdi,
    Rbp,
    Rsp,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

impl RegQ {
    pub fn to_str(&self) -> &'static str {
        match self {
            Self::Rax => "%rax",
            Self::Rbx => "%rbx",
            Self::Rcx => "%rcx",
            Self::Rdx => "%rdx",
            Self::Rsi => "%rsi",
            Self::Rdi => "%rdi",
            Self::Rbp => "%rbp",
            Self::Rsp => "%rsp",
            Self::R8 => "%r8",
            Self::R9 => "%r9",
            Self::R10 => "%r10",
            Self::R11 => "%r11",
            Self::R12 => "%r12",
            Self::R13 => "%r13",
            Self::R14 => "%r14",
            Self::R15 => "%r15",
        }
    }
}

impl Reg for RegQ {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        file.write_all(self.to_str().as_bytes())
    }

    const SIZE: Sizes = Sizes::Quad;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RegL {
    Eax,
    Ebx,
    Ecx,
    Edx,
    Esi,
    Edi,
    Ebp,
    Esp,
    R8d,
    R9d,
    R10d,
    R11d,
    R12d,
    R13d,
    R14d,
    R15d,
}

impl RegL {
    pub fn to_str(&self) -> &'static str {
        match self {
            Self::Eax => "%eax",
            Self::Ebx => "%ebx",
            Self::Ecx => "%ecx",
            Self::Edx => "%edx",
            Self::Esi => "%esi",
            Self::Edi => "%edi",
            Self::Ebp => "%ebp",
            Self::Esp => "%esp",
            Self::R8d => "%r8d",
            Self::R9d => "%r9d",
            Self::R10d => "%r10d",
            Self::R11d => "%r11d",
            Self::R12d => "%r12d",
            Self::R13d => "%r13d",
            Self::R14d => "%r14d",
            Self::R15d => "%r15d",
        }
    }
}

impl Reg for RegL {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        file.write_all(self.to_str().as_bytes())
    }

    const SIZE: Sizes = Sizes::Long;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RegW {
    Ax,
    Bx,
    Cx,
    Dx,
    Si,
    Di,
    Bp,
    Sp,
    R8w,
    R9w,
    R10w,
    R11w,
    R12w,
    R13w,
    R14w,
    R15w,
}

impl RegW {
    pub fn to_str(&self) -> &'static str {
        match self {
            Self::Ax => "%ax",
            Self::Bx => "%bx",
            Self::Cx => "%cx",
            Self::Dx => "%dx",
            Self::Si => "%si",
            Self::Di => "%di",
            Self::Bp => "%bp",
            Self::Sp => "%sp",
            Self::R8w => "%r8w",
            Self::R9w => "%r9w",
            Self::R10w => "%r10w",
            Self::R11w => "%r11w",
            Self::R12w => "%r12w",
            Self::R13w => "%r13w",
            Self::R14w => "%r14w",
            Self::R15w => "%r15w",
        }
    }
}

impl Reg for RegW {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        file.write_all(self.to_str().as_bytes())
    }

    const SIZE: Sizes = Sizes::Word;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RegB {
    Al,
    Ah,
    Bl,
    Bh,
    Cl,
    Ch,
    Dl,
    Dh,
    Sil,
    Dil,
    Bpl,
    Spl,
    R8b,
    R9b,
    R10b,
    R11b,
    R12b,
    R13b,
    R14b,
    R15b,
}

impl RegB {
    pub fn to_str(&self) -> &'static str {
        match self {
            Self::Al => "%al",
            Self::Ah => "%ah",
            Self::Bl => "%bl",
            Self::Bh => "%bh",
            Self::Cl => "%cl",
            Self::Ch => "%ch",
            Self::Dl => "%dl",
            Self::Dh => "%dh",
            Self::Sil => "%sil",
            Self::Dil => "%dil",
            Self::Bpl => "%bpl",
            Self::Spl => "%spl",
            Self::R8b => "%r8b",
            Self::R9b => "%r9b",
            Self::R10b => "%r10b",
            Self::R11b => "%r11b",
            Self::R12b => "%r12b",
            Self::R13b => "%r13b",
            Self::R14b => "%r14b",
            Self::R15b => "%r15b",
        }
    }
}

impl Reg for RegB {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        file.write_all(self.to_str().as_bytes())
    }

    const SIZE: Sizes = Sizes::Byte;
}

#[derive(Debug, Clone)]
pub enum Operand<T: Reg> {
    Addr(i64, RegQ, Option<RegQ>, u8), /* value at 1 + 2 + 3 * 4 */
    Reg(T),
    LabRelAddr(Label),
    LabAbsAddr(Label),
    LabVal(Label),
    Imm(i64),
}

impl<T: Reg> Operand<T> {
    pub fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        match self {
            Self::Reg(reg) => reg.write_in(file),
            Self::Addr(offset, reg, index, scale) => {
                file.write_all(format!("{offset}(").as_bytes())?;
                reg.write_in(file)?;
                match index {
                    None => file.write_all(b")"),
                    Some(index) => {
                        file.write_all(b", ")?;
                        index.write_in(file)?;
                        file.write_all(format!(", {scale})").as_bytes())
                    }
                }
            }
            Self::LabRelAddr(label) => {
                label.write_in(file)?;
                file.write_all(b"(%rip)")
            }
            Self::LabAbsAddr(label) => {
                file.write_all(b"$")?;
                label.write_in(file)
            }
            Self::LabVal(label) => {
                file.write_all(b"(")?;
                label.write_in(file)?;
                file.write_all(b")")
            }
            Self::Imm(id) => file.write_all(format!("${id}").as_bytes()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Label {
    name: String,
}

impl Label {
    pub fn to_string(&self) -> String {
        todo!()
    }

    pub fn from_str(name: String) -> Self {
        Self { name }
    }

    pub fn printf() -> Self {
        Self {
            name: "printf".to_string(),
        }
    }

    pub fn malloc() -> Self {
        Self {
            name: "malloc".to_string(),
        }
    }

    pub fn free() -> Self {
        Self {
            name: "free".to_string(),
        }
    }

    pub fn realloc() -> Self {
        Self {
            name: "realloc".to_string(),
        }
    }

    pub fn panic() -> Self {
        Self {
            name: "panic".to_string(),
        }
    }

    #[cfg(target_os = "macos")]
    pub fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        file.write_all(b"_")?;
        file.write_all(self.name.as_bytes())
    }
    
    #[cfg(target_os = "linux")]
    pub fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        file.write_all(self.name.as_bytes())
    }
}
