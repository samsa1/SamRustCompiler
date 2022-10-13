#![allow(dead_code)]

pub mod ast;
mod backend;
mod frontend;
mod passes;
mod typing;

fn main() {
    let mut filenames = Vec::new();
    let mut parse_only = false;
    let mut type_only = false;

    let mut args = std::env::args().skip(1);
    //    println!("{:?}", args);
    #[allow(clippy::while_let_on_iterator)]
    while let Some(t) = args.next() {
        if t.is_empty() {
            panic!("should never happen")
        } else if t.bytes().next() == Some(b'-') {
            match t.as_str() {
                "--parse-only" => parse_only = true,
                "--type-only" => type_only = true,
                _ => panic!("unkown option"),
            }
        } else {
            filenames.push(t)
        }
    }
    //    println!("{:?}", filenames);

    if filenames.len() != 1 {
        panic!("must give exactly one file to compile")
    }

    let parsed_file = frontend::Module::new(filenames.pop().unwrap());
    let parsed_file = parsed_file.content;
    if parse_only {
        std::process::exit(0)
    }

    let unfolded_macros = passes::macros::rewrite_file(parsed_file);
    let moved_refs = passes::move_refs::rewrite_file(unfolded_macros);

    let typed_file = typing::type_inferencer(moved_refs, true);

    println!("{:?}", typed_file);

    if type_only {
        std::process::exit(0)
    }

    todo!()
}
