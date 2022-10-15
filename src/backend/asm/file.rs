use super::Asm;
use std::collections::HashMap;

pub struct File {
    code_ss: Asm,
    data_ss: HashMap<String, String>,
}

impl File {
    pub fn print_in(self, file_name: &str) {
        todo!()
    }
}
