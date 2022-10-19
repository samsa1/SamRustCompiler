use std::collections::HashMap;
use std::io::prelude::*;
use std::ops::Add;

pub enum DataEL {
    Byte(Vec<i8>),
    Word(Vec<i16>),
    Long(Vec<i32>),
    Quad(Vec<i64>),
    Space(usize),
    Address(Vec<super::reg::Label>),
    String(String),
}

impl DataEL {
    fn write_in(&self, file : &mut std::fs::File) -> std::io::Result<()> {
        match self {
            Self::Byte(vec) => {
                file.write_all(b"\t.byte")?;
                let mut vec = vec.iter();
                match vec.next() {
                    None => (),
                    Some(el) => file.write_all(format!(" {el}").as_bytes())?,
                }
                for el in vec {
                    file.write_all(format!(", {el}").as_bytes())?;
                }
                file.write_all(b"\n")
            },
            Self::Word(vec) => {
                file.write_all(b"\t.word")?;
                let mut vec = vec.iter();
                match vec.next() {
                    None => (),
                    Some(el) => file.write_all(format!(" {el}").as_bytes())?,
                }
                for el in vec {
                    file.write_all(format!(", {el}").as_bytes())?;
                }
                file.write_all(b"\n")
            },
            Self::Long(vec) => {
                file.write_all(b"\t.long")?;
                let mut vec = vec.iter();
                match vec.next() {
                    None => (),
                    Some(el) => file.write_all(format!(" {el}").as_bytes())?,
                }
                for el in vec {
                    file.write_all(format!(", {el}").as_bytes())?;
                }
                file.write_all(b"\n")
            },
            Self::Quad(vec) => {
                file.write_all(b"\t.quad")?;
                let mut vec = vec.iter();
                match vec.next() {
                    None => (),
                    Some(el) => file.write_all(format!(" {el}").as_bytes())?,
                }
                for el in vec {
                    file.write_all(format!(", {el}").as_bytes())?;
                }
                file.write_all(b"\n")
            },
            Self::Address(vec) => {
                file.write_all(b"\t.quad ")?;
                let mut vec = vec.iter();
                match vec.next() {
                    None => (),
                    Some(el) => {
                        el.write_in(file)?
                    },
                }
                for el in vec {
                    file.write_all(b", ")?;
                    el.write_in(file)?;
                }
                file.write_all(b"\n")
            },
            Self::String(str) => {
                file.write_all(b"\t.string \"")?;
                file.write_all(str.as_bytes())?;
                file.write_all(b"\"\n")
            },
            Self::Space(i) => {
                file.write_all(format!("\t.space {i}\n").as_bytes())
            }
        }
    }
}

pub struct Data {
    infos : Vec<(Option<String>, DataEL)>
}

impl Data {
    pub fn new(name : Option<String>, data : DataEL) -> Self {
        Self {
            infos: vec![(name, data)]
        }
    }

    pub fn from_strings(strings : HashMap<String, String>) -> Self {
        Self {
            infos : strings.into_iter().map(|(s1, s2)| (Some(s2), DataEL::String(s1))).collect(),
        }
    }

    pub fn write_in(&self, file : &mut std::fs::File) -> std::io::Result<()> {
        for (name, data) in &self.infos {
            match name {
                None => (),
                Some(name) => {
                    file.write_all(b"_")?;
                    file.write_all(name.as_bytes())?;
                    file.write_all(b":\n")?;        
                },
            }
            data.write_in(file)?;
        }
        Ok(())
    }
}

impl Add for Data {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        let mut infos = self.infos;
        let mut infos2 = other.infos;
        infos.append(&mut infos2);
        Self {
            infos,
        }
    }
}