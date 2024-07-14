use crate::ast::low_level_repr as llr;
use std::collections::HashMap;
use write_x86_64::file;

mod base;
mod context;
mod default;
mod utils;

pub fn compile(
    file: llr::File,
    strings: HashMap<String, String>,
    vec_info: crate::to_llr::VecInfo,
) -> file::File {
    default::compile(file, strings, vec_info)
}
