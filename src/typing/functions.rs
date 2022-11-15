use std::collections::HashMap;

use crate::frontend::Module;
use crate::ast::rust as rr;
use crate::ast::typed_rust as tr;
use crate::ast::common::*;
use super::context;
use super::context::GlobalContext;
use super::context::ModuleInterface;
use super::types;
use super::inferencer;

/*
fn type_funs(
    funs: Vec<rust::DeclFun>,
    known_types: &mut context::GlobalContext,
    err_reporter: &ErrorReporter,
) -> Vec<typed_rust::DeclFun> {
    let mut fun_types = Vec::new();
    let mut fun_names = HashSet::new();
    for fun_decl in funs.iter() {
        let args: Option<Vec<typed_rust::PostType>> = fun_decl
            .args
            .iter()
            .map(|(_, _, typ)| types::translate_typ(typ.clone(), known_types))
            .collect();
        let args = match args {
            Some(a) => a,
            None => todo!(),
        };
        let output = match types::translate_typ(fun_decl.output.clone(), known_types) {
            Some(out) => out,
            None => todo!(),
        };
        fun_types.push((args.clone(), output.clone()));
        let fun_typ = typed_rust::PostType {
            content: typed_rust::PostTypeInner::Fun(Vec::new(), args, Box::new(output)),
        };

        known_types.insert(fun_decl.name.get_content().to_string(), fun_typ);
    }

    let free_type = typed_rust::PostType {
        content: typed_rust::PostTypeInner::FreeType("T".to_string()),
    };
    let vec_type = typed_rust::PostType {
        content: typed_rust::PostTypeInner::Struct("Vec".to_string(), vec![free_type.clone()]),
    };
    let ref_vec_type = typed_rust::PostType {
        content: typed_rust::PostTypeInner::Ref(false, Box::new(vec_type.clone())),
    };
    let mut_ref_vec_type = typed_rust::PostType {
        content: typed_rust::PostTypeInner::Ref(true, Box::new(vec_type.clone())),
    };
    let fun_typ = typed_rust::PostType {
        content: typed_rust::PostTypeInner::Fun(vec!["T".to_string()], vec![], Box::new(vec_type)),
    };

    known_types.insert("std::vec::Vec::new".to_string(), fun_typ);
    let fun_typ = typed_rust::PostType {
        content: typed_rust::PostTypeInner::Fun(
            vec!["T".to_string()],
            vec![ref_vec_type],
            Box::new(known_types.get_typ("usize").unwrap().clone()),
        ),
    };
    known_types.insert("std::vec::Vec::len".to_string(), fun_typ);

    let fun_typ = typed_rust::PostType {
        content: typed_rust::PostTypeInner::Fun(
            vec!["T".to_string()],
            vec![mut_ref_vec_type, free_type],
            Box::new(typed_rust::PostType::unit()),
        ),
    };
    known_types.insert("std::vec::Vec::push".to_string(), fun_typ);
    known_types.impl_method("Vec", "len".to_string(), "std::vec::Vec::len".to_string());
    known_types.impl_method("Vec", "push".to_string(), "std::vec::Vec::push".to_string());

    assert_eq!(fun_types.len(), funs.len());
    let mut fun_vec = Vec::new();

    for (fun_decl, (args_typ, output)) in funs.into_iter().zip(fun_types.into_iter()) {
        let in_types: Vec<(Ident, bool, typed_rust::PostType)> = fun_decl
            .args
            .into_iter()
            .zip(args_typ.into_iter())
            .map(|((name, b, _pre_type), post_type)| (name, b, post_type))
            .collect();
        let in_types2: Vec<(String, bool, &typed_rust::PostType)> = in_types
            .iter()
            .map(|(id, b, typ)| (id.get_content().to_string(), *b, typ))
            .collect();
        let (content, types) = match inferencer::type_funs(
            &fun_decl.name,
            known_types,
            &in_types2,
            &output,
            fun_decl.content.clone(),
        ) {
            Ok(out) => out,
            Err(errs) => err_reporter.report(errs),
        };
        //        {println!("typing not finished"); std::process::exit(0)},
        let mut local_ctxt = context::LocalContext::new(&in_types);

        let content = match expr::type_bloc(
            content,
            known_types,
            &mut local_ctxt,
            &output,
            Some(&output),
            &types,
        ) {
            Ok(out) => out,
            Err(errs) => err_reporter.report(errs),
        };
        if !fun_names.insert(fun_decl.name.get_content().to_string()) {
            println!(
                "Function {} is declared multiple times",
                fun_decl.name.get_content()
            );
            std::process::exit(1)
        }

        fun_vec.push(typed_rust::DeclFun {
            name: fun_decl.name,
            args: in_types,
            output,
            content,
            id_counter: fun_decl.id_counter,
        });
    }

    fun_vec
}
*/

fn handle_fun(ctxt : &GlobalContext, fun_decl : rr::DeclFun, err_reporter : &ErrorReporter) -> tr::DeclFun {
    let fun_type = ctxt.get_top_fun(fun_decl.name.get_content()).unwrap();
    let (args_typ, out_type) = match &fun_type.content {
        tr::PostTypeInner::Fun(_, args, out) => (args.clone(), &**out),
        _ => panic!("ICE"),
    };

    let in_types: Vec<(Ident, bool, tr::PostType)> = fun_decl
        .args
        .into_iter()
        .zip(args_typ.into_iter())
        .map(|((name, b, _pre_type), post_type)| (name, b, post_type))
        .collect();
    let in_types2: Vec<(String, bool, &tr::PostType)> = in_types
        .iter()
        .map(|(id, b, typ)| (id.get_content().to_string(), *b, typ))
        .collect();
    let (content, types) = match inferencer::type_funs(
        &fun_decl.name,
        ctxt,
        &in_types2,
        &out_type,
        fun_decl.content.clone(),
    ) {
        Ok(out) => out,
        Err(errs) => err_reporter.report(errs),
    };

    let mut local_ctxt = context::LocalContext::new(&in_types);
    let content = match super::expr::type_bloc(
        content,
        ctxt,
        &mut local_ctxt,
        out_type,
        Some(out_type),
        &types,
    ) {
        Ok(out) => out,
        Err(errs) => err_reporter.report(errs),
    };

    tr::DeclFun {
        name: fun_decl.name,
        args: in_types,
        output : out_type.clone(),
        content,
        id_counter: fun_decl.id_counter,
    }
}

pub fn add_fun_types(module : &Module<rr::File>, modint : ModuleInterface, path : PathUL<()>) -> ModuleInterface {
    let mut ctxt = GlobalContext::new(path.clone(), modint);
    for decl in module.content.content.iter() {
        match decl {
            rr::Decl::Fun(fun_decl) => {
                let args: Option<Vec<tr::PostType>> = fun_decl
                    .args
                    .iter()
                    .map(|(_, _, typ)| types::translate_typ(typ.clone(), &ctxt))
                    .collect();
                let args = match args {
                    Some(a) => a,
                    None => todo!(),
                };
                let output = match types::translate_typ(fun_decl.output.clone(), &ctxt) {
                    Some(out) => out,
                    None => todo!(),
                };
                let fun_typ = tr::PostType {
                    content: tr::PostTypeInner::Fun(Vec::new(), args, Box::new(output)),
                };
                if ctxt.impl_fun(
                    fun_decl.name.get_content().to_string(), fun_decl.public, fun_typ
                ).is_some() {
                    panic!("")
                };
            },
            _ => (),
        }
    }
    let mut modint = ctxt.extract_module();
    for (name, (_, module)) in module.submodules.iter() {
        let mut path = path.clone();
        path.push(NamePath::Name(name.to_string()));
        modint = add_fun_types(module, modint, path)
    }
    modint
}

fn translate_funs(module : Module<rr::File>, modint : ModuleInterface, path : PathUL<()>) -> (ModuleInterface, Module<tr::File>) {
    let ctxt = GlobalContext::new(path.clone(), modint);
    let mut funs = Vec::new();
    for decl in module.content.content.into_iter() {
        match decl {
            rr::Decl::Fun(ds) => {
                funs.push(handle_fun(&ctxt, ds, &module.content.err_reporter))
            },
            _ => panic!("ICE"),
        }
    }
    let mut modint = ctxt.extract_module();
    let mut submodules = HashMap::new();
    for (name, (b, module)) in module.submodules.into_iter() {
        let mut path = path.clone();
        path.push(NamePath::Name(name.clone()));
        let out = translate_funs(module, modint, path);
        modint = out.0;
        submodules.insert(name, (b, out.1));
    }

    (
        modint, 
        Module::build(tr::File {
            name : module.content.name,
            funs,
            structs : Vec::new(),
        }, submodules)
    )
}

pub fn handle(module : Module<rr::File>, modint : ModuleInterface, path : PathUL<()>) -> (ModuleInterface, Module<tr::File>) {
    let mut modint = add_fun_types(&module, modint, path.clone());
    translate_funs(module, modint, path)
}
