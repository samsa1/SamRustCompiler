use super::Asm;
use std::collections::HashMap;
use std::fs;
use std::io::prelude::*;

pub struct File {
    code_ss: Asm,
    data_ss: super::data::Data,
}

impl File {
    pub fn new(code_ss: Asm, strings: HashMap<String, String>) -> Self {
        let data_ss = super::data::Data::from_strings(strings);
        Self { code_ss, data_ss }
    }

    pub fn print_in(self, file_name: &str) -> std::io::Result<()> {
        let mut file = fs::File::create(file_name)?;
        file.write_all(b"\t.text\n")?;
        file.write_all(b"\t.globl\t_main\n")?;
        self.code_ss.write_in(&mut file)?;
        file.write_all(b"\t.data\n")?;

        self.data_ss.write_in(&mut file)?;

        file.write_all(b"_my_string:\n\t.string \"%zd\\n\"\n")
    }
}
