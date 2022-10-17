use super::Asm;
use std::collections::HashMap;
use std::fs;
use std::io::prelude::*;

pub struct File {
    code_ss: Asm,
    data_ss: HashMap<String, String>,
}

impl File {
    pub fn new(code_ss : Asm, data_ss : HashMap<String, String>) -> Self {
        Self {
            code_ss,
            data_ss,
        }
    }

    pub fn print_in(self, file_name: &str) -> std::io::Result<()> {
        let mut file = fs::File::create(file_name)?;
        file.write_all(b"\t.text\n")?;
        file.write_all(b"\t.globl\t_main\n")?;
        self.code_ss.write_in(&mut file)?;
        file.write_all(b"\t.data\n")?;

        for (str1, str2) in self.data_ss {
            file.write_all(b"_")?;
            file.write_all(str2.as_bytes())?;
            file.write_all(b":\n\t.string \"")?;
            file.write_all(str1.as_bytes())?;
            file.write_all(b"\"\n")?;
        };

        Ok(())

    }
}
