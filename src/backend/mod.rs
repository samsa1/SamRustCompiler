use crate::ast::low_level_repr as llr;
use std::collections::HashMap;
use write_x86_64::file;

mod base;
mod context;
mod default;
mod optimized;
mod utils;

pub fn compile(
    optimized: bool,
    file: llr::File,
    strings: HashMap<String, String>,
    vec_info: crate::to_llr::VecInfo,
) -> file::File {
    if optimized {
        optimized::compile(file, strings, vec_info)
    } else {
        default::compile(file, strings, vec_info)
    }
}
