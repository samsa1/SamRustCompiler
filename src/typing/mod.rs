use crate::ast::{common::*, rust, typed_rust};
use crate::frontend::Module;

mod consts;
pub mod context;
pub mod errors;
mod expr;
mod functions;
mod implementation;
mod inferencer;
mod lifetime_analysis;
mod structs;
pub mod types;

pub fn type_inferencer(
    mut file: Module<rust::File>,
    needs_main: bool,
    path: PathUL<()>,
) -> (context::ModuleInterface, Module<typed_rust::File>) {
    let (mut module_interface, structs) = structs::type_structs(&mut file);
    module_interface = consts::handle(&mut file, module_interface);
    module_interface = implementation::handle(&mut file, module_interface, &mut path.clone());
    let (module_interface, file) = functions::handle(
        file,
        module_interface,
        PathUL::new(vec![NamePath::Name("crate".to_string())]),
    );

    // let mut has_main = false;
    // for fun in file.content.funs.iter() {
    //     has_main |= fun.name.get_content() == "main"
    // }

    // if needs_main && !has_main {
    //     println!("No main");
    //     std::process::exit(1)
    // }

    (module_interface, file)
}

/*     let mut funs = Vec::new();
    let mut structs = Vec::new();
    let mut impls = Vec::new();
    let mut consts = Vec::new();
    for decl in file.content.into_iter() {
        match decl {
            rust::Decl::Fun(f) => funs.push(f),
            rust::Decl::Struct(s) => structs.push(s),
            rust::Decl::Impl(i) => impls.push(i),
            rust::Decl::Const(c) => consts.push(c),
        }
    }
    let (mut global_ctxt, structs) = structs::type_structs(structs);
    handle_constants(consts, &mut global_ctxt, &file.err_reporter);
    handle_implemantations(impls, &mut funs, &mut global_ctxt, &file.err_reporter);
    let funs = type_funs(funs, &mut global_ctxt, &file.err_reporter);
    let mut has_main = false;
    for fun in funs.iter() {
        has_main |= fun.name.get_content() == "main"
    }

    if needs_main && !has_main {
        println!("No main");
        std::process::exit(1)
    }

    typed_rust::File {
        name: file.name,
        structs,
        funs,
    }
*/
