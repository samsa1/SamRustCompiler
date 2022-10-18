#![allow(dead_code)]

pub mod ast;
mod backend;
mod frontend;
mod passes;
mod to_llr;
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

    let in_name = filenames.pop().unwrap();

    let parsed_file = frontend::Module::new(in_name.clone());
    let parsed_file = parsed_file.content;
    if parse_only {
        std::process::exit(0)
    }

    let unfolded_macros = passes::macros::rewrite_file(parsed_file);
    let distinct_names = passes::give_uniq_id::rewrite_file(unfolded_macros);
    let moved_refs = passes::move_refs::rewrite_file(distinct_names);

    println!("<- typing ");

    let typed_file = typing::type_inferencer(moved_refs, true);

    println!("<- check lifetime (TODO) ");

    let checked_lifetime = typed_file;

    //    println!("{:?}", checked_lifetime);

    if type_only {
        std::process::exit(0)
    }

    println!("<- linear programs pass");

    let made_linear = passes::linear_programs::rewrite_file(checked_lifetime);

    println!("<- to llr");

    let llr_form = to_llr::rewrite_file(made_linear);

    println!("<- to asm");

    let asm = backend::to_asm(llr_form);

    let mut out_name = std::path::PathBuf::from(in_name);
    out_name.set_extension("s");

    match asm.print_in(out_name.to_str().unwrap()) {
        Ok(()) => (),
        Err(err) => {
            println!("Failed during printing asm with internal error {:?}", err);
            std::process::exit(1)
        }
    };
}
