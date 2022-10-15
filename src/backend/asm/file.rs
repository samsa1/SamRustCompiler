use super::Asm;
use std::collections::HashMap;

pub struct File {
    code_ss: Asm,
    data_ss: HashMap<String, String>,
}
