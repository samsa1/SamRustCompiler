use std::collections::HashMap;

pub mod ast;
mod backend;
pub mod config;
mod frontend;
mod passes;
mod std_file;
mod to_llr;
mod typing;

fn main() {
    let mut filenames = Vec::new();
    let mut parse_only = false;
    let mut type_only = false;
    let mut generate_std = false;

    let mut args = std::env::args().skip(1);

    #[allow(clippy::while_let_on_iterator)]
    while let Some(t) = args.next() {
        if t.is_empty() {
            panic!("should never happen")
        } else if t.bytes().next() == Some(b'-') {
            match t.as_str() {
                "--parse-only" => parse_only = true,
                "--type-only" => type_only = true,
                "--generate-std" => generate_std = true,
                _ => panic!("unkown option"),
            }
        } else {
            filenames.push(t)
        }
    }

    if generate_std {
        if filenames.len() != 0 {
            panic!("no argument can be given with the --generate-std option");
        }

        let std = frontend::Module::new("std/mod.rs".to_string());
        std.write_in_out("src/std_file.rs").unwrap();
        std::process::exit(0)
    }

    if filenames.len() != 1 {
        panic!("must give exactly one file to compile")
    }

    // Parsing files
    let in_name = filenames.pop().unwrap();
    let code = frontend::Module::new(in_name.clone());
    if parse_only {
        std::process::exit(0)
    }

    // Various preprocessing
    let code = passes::macros::rewrite(code);
    let code = passes::unfold_uses::rewrite(code, &mut ast::common::Path::from_vec(vec!["crate"]));
    let code = passes::give_uniq_id::rewrite(code);
    let code = passes::move_refs::rewrite(code);

    // Typing of code
    let (code_modint, typed_file) =
        typing::type_inferencer(code, true, ast::common::PathUL::from_vec(vec!["crate"]));

    println!("<- check lifetime (TODO) ");

    let code = typed_file;

    if type_only {
        std::process::exit(0)
    }

    // Compiling std
    let std = std_file::stdlib().unwrap();
    let std = passes::macros::rewrite(std);
    let std = passes::unfold_uses::rewrite(std, &mut ast::common::Path::from_vec(vec!["crate"]));
    let std = passes::give_uniq_id::rewrite(std);
    let std = passes::move_refs::rewrite(std);
    let (std_modint, std) =
        typing::type_inferencer(std, false, ast::common::PathUL::from_vec(vec!["crate"]));
    println!("<- check lifetime (TODO) for std");

    let std = passes::change_crate_name::rewrite(std, "crate", "std");

    // Fusionning the code and the std in a single huge module
    let mut submodules = HashMap::new();
    submodules.insert("crate".to_string(), (true, code));
    submodules.insert("std".to_string(), (true, std));
    let code = frontend::Module::build(ast::typed_rust::File::empty(), submodules);

    let std_modint = std_modint.extract("std");
    let code_modint = code_modint.extract("crate");
    let mut modint = typing::context::ModuleInterface::empty();
    modint.insert("std".to_string(), true, std_modint);
    modint.insert("crate".to_string(), true, code_modint);

    let (code, modint) = passes::handle_generics::rewrite(code, modint);
    let code = passes::to_builtin_ops::rewrite(code);

    // Make code linear
    let code = passes::linear_programs::rewrite(code);
    let code = passes::handle_enums::rewrite(code, &modint);

    // Transform the typed modules to a single llr File
    let (llr_form, strings, vec_info) = to_llr::rewrite(code, modint, "file".to_string());
    let llr_form = passes::concat_all::rewrite(llr_form);

    // Compile to asm
    let asm = backend::compile(llr_form, strings, vec_info);

    // Printing asm
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
