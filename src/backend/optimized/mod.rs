use crate::ast::low_level_repr as llr;
use std::collections::HashMap;

mod llr_to_pasm;
mod pasm_to_asm;
mod pre_asm;

pub fn compile(
    file: llr::File,
    strings: HashMap<String, String>,
    vec_info: crate::to_llr::VecInfo,
) -> write_x86_64::file::File {
    let file = llr_to_pasm::compile(file.clone());
    pasm_to_asm::compile(file, strings, vec_info)
}
