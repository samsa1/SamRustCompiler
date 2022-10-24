mod parser;
use crate::ast::rust::{File, Open};
use std::collections::HashMap;
use std::path::PathBuf;

pub struct Module {
    pub content: File,
    submodules: HashMap<String, (bool, Module)>,
}

impl Module {
    pub fn new(base_file_name: String) -> Self {
        let base_file_name = PathBuf::from(base_file_name);
        match base_file_name.extension() {
            Some(os_str) if os_str.to_str() == Some("rs") => (),
            _ => panic!("Expected a .rs file"),
        }
        Self::new_inner(base_file_name, true)
    }

    fn new_inner(file_name: PathBuf, is_base: bool) -> Self {
        let content = parser::parse_file(file_name.to_str().unwrap().to_string());
        println!("parsed one");
        let mut submodules = HashMap::new();
        for dep in content.dep.iter() {
            println!("handling deps");
            match dep {
                Open::Use(_, _) => (),
                Open::Mod(b, name1, name2) => {
                    if !is_base && file_name.file_name().unwrap().to_str() != Some("mod.rs") {
                        panic!("Not allowed")
                    };
                    let mut path1 = file_name.clone();
                    path1.pop();
                    let mut path2 = path1.clone();
                    path1.push(name1.get_content());
                    path1.set_extension("rs");
                    path2.push(name1.get_content());
                    path2.push("mod.rs");
                    println!("file 1 {:?} {}", path1, path1.exists());
                    println!("file 2 {:?} {}", path2, path2.exists());
                    let file_name = match (path1.exists(), path2.exists()) {
                        (true, false) => path1,
                        (false, true) => path2,
                        (true, true) => panic!("Ambiguity"),
                        (false, false) => panic!("No file"),
                    };
                    let submod = Self::new_inner(file_name, false);
                    let name = match name2 {
                        None => name1.get_content().to_string(),
                        Some(name2) => name2.get_content().to_string(),
                    };
                    submodules.insert(name, (*b, submod));
                }
            }
        }
        println!("handled deps");
        Self {
            content,
            submodules,
        }
    }
}
