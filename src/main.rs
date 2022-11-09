pub mod ast;
mod backend;
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

    let parsed_file = frontend::Module::new(in_name.clone());
    let parsed_file = parsed_file.content;
    if parse_only {
        std::process::exit(0)
    }

    println!("<- parsing");

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

    let mut std = std_file::stdlib().unwrap();
    let allocator = std.remove("allocator").unwrap().content;
    let allocator = passes::macros::rewrite_file(allocator);
    let allocator = passes::give_uniq_id::rewrite_file(allocator);
    let allocator = passes::move_refs::rewrite_file(allocator);
    let allocator = typing::type_inferencer(allocator, false);
    let allocator = passes::linear_programs::rewrite_file(allocator);
    let allocator = to_llr::rewrite_file(allocator, "alloc".to_string());

    println!("<- linear programs pass");

    let made_linear = passes::linear_programs::rewrite_file(checked_lifetime);

    println!("<- to llr");

    let llr_form = to_llr::rewrite_file(made_linear, "file".to_string());

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
    };
}
