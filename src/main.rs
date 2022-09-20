#![allow(dead_code)]

pub mod ast;
mod typing;
mod frontend;

fn main() {

    let mut filenames = Vec::new();
    let mut parse_only = false;
    let mut type_only = false;

    let mut args = std::env::args().skip(1);
//    println!("{:?}", args);
    while let Some(t) = args.next() {
        if t.len() == 0 {
            panic!("should never happen")
        } else {
            if t.bytes().next() == Some(b'-') {
                match t.as_str() {
                    "--parse-only" => parse_only = true,
                    "--type-only" => type_only = true,
                    _ => panic!("unkown option"),
                }
            } else {
                filenames.push(t)
            }
        }
    }
//    println!("{:?}", filenames);

    if filenames.len() != 1 {
        panic!("must give exactly one file to compile")
    }

    let parsed_file = frontend::parser::parse_file(filenames.pop().unwrap());
    if parse_only { std::process::exit(0)}

    let typed_file = typing::type_inferencer(parsed_file);

    if type_only { std::process::exit(0) }

    todo!()
}
