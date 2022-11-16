use crate::ast::common::Path;

pub mod ast;
//mod backend;
mod frontend;
mod passes;
mod std_file;
//mod to_llr;
mod typing;

fn main() {
    let mut filenames = Vec::new();
    let mut parse_only = false;
    let mut type_only = false;
    let mut generate_std = false;

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
                "--generate-std" => generate_std = true,
                _ => panic!("unkown option"),
            }
        } else {
            filenames.push(t)
        }
    }
    //    println!("{:?}", filenames);

    if generate_std {
        if filenames.len() != 0 {
            panic!("no argument can be given with the --generate-std option");
        }

        let mut std = frontend::Module::new("std/mod.rs".to_string());
        std.write_in_out("src/std_file.rs");
        std::process::exit(0)
    }

    if filenames.len() != 1 {
        panic!("must give exactly one file to compile")
    }

    let in_name = filenames.pop().unwrap();

    let code = frontend::Module::new(in_name.clone());
    if parse_only {
        std::process::exit(0)
    }

    println!("<- parsing");

    let code = passes::macros::rewrite(code);
    let code = passes::unfold_uses::rewrite(code, &mut ast::common::Path::from_vec(vec!["crate"]));
    let code = passes::give_uniq_id::rewrite(code);
    let code = passes::move_refs::rewrite(code);

    println!("<- typing ");

    let typed_file =
        typing::type_inferencer(code, true, ast::common::PathUL::from_vec(vec!["crate"]));

    println!("<- check lifetime (TODO) ");

    let mut checked_lifetime = typed_file;

    //    println!("{:?}", checked_lifetime);

    if type_only {
        std::process::exit(0)
    }

    let std = std_file::stdlib().unwrap();
    let std = passes::macros::rewrite(std);
    let std = passes::unfold_uses::rewrite(std, &mut ast::common::Path::from_vec(vec!["crate"]));
    let std = passes::give_uniq_id::rewrite(std);
    let std = passes::move_refs::rewrite(std);
    let std = typing::type_inferencer(std, false, ast::common::PathUL::from_vec(vec!["crate"]));
    let mut std = passes::linear_programs::rewrite(std);
    let allocator = std.remove("allocator").unwrap().content;
    //    let allocator = to_llr::rewrite_file(allocator, "alloc".to_string());

    println!("<- linear programs pass");

    let std = passes::linear_programs::rewrite(checked_lifetime);

    println!("<- to llr");

    todo!();

    /*    let llr_form = to_llr::rewrite_file(checked_lifetime, "file".to_string());

    println!("<- to asm");

    let mut ctxt = backend::get_ctxt();
    let entry_point = backend::base(&mut ctxt);
    let allocator = backend::to_asm(allocator, &mut ctxt);
    let asm = backend::to_asm(llr_form, &mut ctxt);
    let asm = backend::bind(vec![entry_point, allocator, asm]);

    let mut out_name = std::path::PathBuf::from(in_name);
    out_name.set_extension("s");

    match asm.print_in(out_name.to_str().unwrap()) {
        Ok(()) => (),
        Err(err) => {
            println!("Failed during printing asm with internal error {:?}", err);
            std::process::exit(1)
        }
    };*/
}
