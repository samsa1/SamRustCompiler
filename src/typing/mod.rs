use crate::ast::{common::*, rust, typed_rust};
use std::collections::HashSet;
use std::str::FromStr;

use self::context::GlobalContext;
use self::structs::topological_sort;

mod consts;
pub mod context;
pub mod errors;
mod expr;
mod inferencer;
mod lifetime_analysis;
mod structs;
pub mod types;

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
            Err(errs) => {
                for err in errs.into_iter() {
                    err.report_error(err_reporter);
                }
                std::process::exit(1);
            }
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
            Err(errs) => {
                for err in errs.into_iter() {
                    err.report_error(err_reporter);
                }
                std::process::exit(1);
            }
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

pub fn handle_implemantations(
    impls: Vec<rust::DeclImpl>,
    funs: &mut Vec<rust::DeclFun>,
    global_ctxt: &mut context::GlobalContext,
    err_reporter: &ErrorReporter,
) {
    for impl_decl in impls {
        if global_ctxt
            .get_known_types()
            .get(impl_decl.name.get_content())
            .is_none()
        {
            errors::TypeError::unknown_struct(impl_decl.name).report_error(err_reporter);
            std::process::exit(1)
        };
        for mut decl_fun in impl_decl.content {
            let mut new_name = impl_decl.name.get_content().to_string();
            new_name.push_str("::");
            new_name.push_str(decl_fun.name.get_content());
            let name = Ident::new_from(
                new_name,
                decl_fun.name.get_loc().start(),
                decl_fun.name.get_loc().end(),
            );

            let args = match decl_fun.self_arg {
                None => decl_fun.args,
                Some(b) => {
                    if global_ctxt
                        .impl_method(
                            impl_decl.name.get_content(),
                            decl_fun.name.content(),
                            name.get_content().to_string(),
                        )
                        .is_some()
                    {
                        todo!()
                    }
                    let typ = rust::PreType {
                        content: rust::PreTypeInner::Ident(impl_decl.name.clone()),
                    };
                    let typ = if let Some(b) = b {
                        rust::PreType {
                            content: rust::PreTypeInner::Ref(b, Box::new(typ)),
                        }
                    } else {
                        typ
                    };
                    let mut args = vec![(Ident::from_str("self").unwrap(), false, typ)];
                    args.append(&mut decl_fun.args);
                    args
                }
            };

            let decl_fun = rust::DeclFun {
                name,
                args,
                ..decl_fun
            };
            funs.push(decl_fun)
        }
    }
}

fn handle_constants(
    consts: Vec<rust::DeclConst>,
    ctxt: &mut GlobalContext,
    err_reporter: &ErrorReporter,
) {
    let mut graph = structs::Graph::new();
    for const_decl in consts.iter() {
        let typ = match types::translate_typ(const_decl.typ.clone(), ctxt) {
            Some(t) => t,
            None => todo!(),
        };
        // is this relevant ?
        ctxt.add_const(const_decl.name.get_content().to_string(), typ);
        graph.add_node(const_decl.name.get_content().to_string());
    }
    for const_decl in consts.iter() {
        consts::add_deps(&const_decl.expr, const_decl.name.get_content(), &mut graph);
    }
    let consts = match topological_sort(consts, &graph) {
        Ok(c) => c,
        _ => todo!(),
    };
    for const_decl in consts.into_iter() {
        let expr = match inferencer::type_const(const_decl.expr, ctxt) {
            Ok(e) => e,
            _ => todo!(),
        };
        let expr = match expr::type_const(expr, ctxt) {
            Ok(e) => e,
            _ => todo!(),
        };
        let val = consts::compute_const(expr, ctxt);

        ctxt.add_const_val(const_decl.name.get_content(), val);
    }
}

pub fn type_inferencer(file: rust::File, needs_main: bool) -> typed_rust::File {
    let mut funs = Vec::new();
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
}
