use crate::ast::common::*;
use crate::ast::rust::*;
use std::collections::HashMap;

fn rewrite_decl(decl: Decl, map: &HashMap<String, Path<PreType>>) -> Decl {
    match decl {
        Decl::Fun(fun) => todo!(),
        Decl::Struct(str) => todo!(),
    }
}

pub fn handle_file(file: File) -> File {
    let mut map = HashMap::new();
    for open_decl in file.dep.iter() {
        match open_decl {
            Open::Mod(_, module, None) => {
                map.insert(module.get_content().to_string(), module.to_path());
            }
            Open::Mod(_, module, Some(given_name)) => {
                map.insert(given_name.get_content().to_string(), module.to_path());
            }
            Open::Use(path, None) => {
                map.insert(path.last().unwrap().get_content().to_string(), path.clone());
            }
            Open::Use(path, Some(given_name)) => {
                map.insert(given_name.get_content().to_string(), path.clone());
            }
        }
    }

    let content = file
        .content
        .into_iter()
        .map(|d| rewrite_decl(d, &map))
        .collect();

    File {
        name: file.name,
        content,
        dep: file.dep,
        err_reporter: file.err_reporter,
    }
}
