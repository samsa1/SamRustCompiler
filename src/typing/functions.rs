use std::collections::HashMap;
use std::collections::HashSet;

use super::context;
use super::context::GlobalContext;
use super::context::ModuleInterface;
use super::inferencer;
use super::types;
use crate::ast::common::*;
use crate::ast::rust as rr;
use crate::ast::typed_rust as tr;
use crate::frontend::Module;
use std::str::FromStr;

fn handle_fun(
    ctxt: &mut GlobalContext,
    fun_decl: rr::DeclFun,
    fun_path: &Path<()>,
    err_reporter: &ErrorReporter,
) -> tr::DeclFun {
    let fun_info = ctxt.get_fun(fun_path).unwrap();
    let free = fun_info.get_free().clone();
    let args_typ = fun_info.get_args().clone();
    let out_type = fun_info.get_out().clone();

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
        &free,
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
        &out_type,
        Some(&out_type),
        &types,
    ) {
        Ok(out) => out,
        Err(errs) => err_reporter.report(errs),
    };
    ctxt.update_dependancies(fun_path, local_ctxt.extract_fun());
    tr::DeclFun {
        name: ctxt.get_path(fun_decl.name.get_content()),
        args: in_types,
        output: out_type,
        free,
        content,
        id_counter: fun_decl.id_counter,
    }
}

pub fn add_fun_types(
    module: &Module<rr::File>,
    modint: ModuleInterface,
    path: PathUL<()>,
) -> ModuleInterface {
    let mut ctxt = GlobalContext::new(path.clone(), modint);
    for decl in module.content.content.iter() {
        match decl {
            rr::Decl::Fun(fun_decl) => {
                let mut frees = HashSet::new();
                for id in &fun_decl.generics {
                    if !frees.insert(id.get_content().to_string()) {
                        todo!()
                    }
                }
                let args: Option<Vec<tr::PostType>> = fun_decl
                    .args
                    .iter()
                    .map(|(_, _, typ)| types::translate_typ(typ.clone(), &ctxt, &frees))
                    .collect();
                let args = match args {
                    Some(a) => a,
                    None => todo!(),
                };
                let output = match types::translate_typ(fun_decl.output.clone(), &ctxt, &frees) {
                    Some(out) => out,
                    None => todo!(),
                };
                // let fun_typ = tr::PostType {
                //     content: tr::PostTypeInner::Fun(Vec::new(), args, Box::new(output)),
                // };
                if ctxt
                    .impl_fun(
                        fun_decl.name.get_content().to_string(),
                        fun_decl.public,
                        fun_decl
                            .generics
                            .iter()
                            .map(|id| id.get_content().to_string())
                            .collect(),
                        args,
                        output,
                    )
                    .is_some()
                {
                    println!("Need to write error message 5");
                    std::process::exit(1)
                };
            }
            rr::Decl::Impl(impl_decl) => {
                let frees = HashSet::new();
                for fun_decl in &impl_decl.content {
                    let args: Option<Vec<tr::PostType>> =
                        match fun_decl.self_arg {
                            None => fun_decl
                                .args
                                .iter()
                                .map(|(_, _, typ)| types::translate_typ(typ.clone(), &ctxt, &frees))
                                .collect(),
                            Some(b) => {
                                let typ = rr::PreType {
                                    content: rr::PreTypeInner::Ident(impl_decl.name.clone()),
                                };
                                let typ = if let Some(b) = b {
                                    rr::PreType {
                                        content: rr::PreTypeInner::Ref(b, Box::new(typ)),
                                    }
                                } else {
                                    typ
                                };
                                let mut args = vec![types::translate_typ(typ, &ctxt, &frees)];
                                for expr in fun_decl.args.iter().map(|(_, _, typ)| {
                                    types::translate_typ(typ.clone(), &ctxt, &frees)
                                }) {
                                    args.push(expr)
                                }
                                args.into_iter().collect()
                            }
                        };
                    let args = match args {
                        Some(a) => a,
                        None => todo!(),
                    };
                    let output = match types::translate_typ(fun_decl.output.clone(), &ctxt, &frees)
                    {
                        Some(out) => out,
                        None => todo!(),
                    };
                    // let fun_typ = tr::PostType {
                    //     content: tr::PostTypeInner::Fun(Vec::new(), args, Box::new(output)),
                    // };
                    if ctxt
                        .impl_fun_path(
                            PathUL::from_vec(vec![impl_decl.name.get_content()]),
                            fun_decl.name.get_content().to_string(),
                            fun_decl.public,
                            Vec::new(),
                            args,
                            output,
                        )
                        .is_some()
                    {
                        panic!("")
                    };
                }
            }
            _ => panic!("ICE"),
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

fn translate_funs(
    module: Module<rr::File>,
    modint: ModuleInterface,
    path: PathUL<()>,
) -> (ModuleInterface, Module<tr::File>) {
    let mut ctxt = GlobalContext::new(path.clone(), modint);
    let mut funs = Vec::new();
    for decl in module.content.content.into_iter() {
        match decl {
            rr::Decl::Fun(ds) => {
                let mut fun_path = path.add_loc();
                fun_path.push(NamePath::Name(ds.name.clone()));
                //                let fun_info = ctxt.get_top_fun(ds.name.get_content()).unwrap();
                funs.push(handle_fun(
                    &mut ctxt,
                    ds,
                    &fun_path,
                    &module.content.err_reporter,
                ))
            }
            rr::Decl::Impl(impl_decl) => {
                for mut fun_decl in impl_decl.content {
                    match fun_decl.self_arg {
                        None => (),
                        Some(b) => {
                            let typ = rr::PreType {
                                content: rr::PreTypeInner::Ident(impl_decl.name.clone()),
                            };
                            let typ = if let Some(b) = b {
                                rr::PreType {
                                    content: rr::PreTypeInner::Ref(b, Box::new(typ)),
                                }
                            } else {
                                typ
                            };
                            let mut args = vec![(Ident::from_str("self").unwrap(), false, typ)];
                            args.append(&mut fun_decl.args);
                            fun_decl.args = args;
                            fun_decl.self_arg = None;
                        }
                    };
                    let mut fun_path = path.add_loc();
                    fun_path.push(NamePath::Name(impl_decl.name.clone()));
                    fun_path.push(NamePath::Name(fun_decl.name.clone()));
                    println!("{:?}", fun_path);
                    let mut fun_decl =
                        handle_fun(&mut ctxt, fun_decl, &fun_path, &module.content.err_reporter);
                    let out = fun_decl.name.pop();
                    fun_decl
                        .name
                        .push(NamePath::Name(impl_decl.name.get_content().to_string()));
                    fun_decl.name.push(out.unwrap());
                    funs.push(fun_decl)
                }
            }
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
        Module::build(
            tr::File {
                name: module.content.name,
                funs,
                //                structs: Vec::new(),
            },
            submodules,
        ),
    )
}

pub fn handle(
    module: Module<rr::File>,
    modint: ModuleInterface,
    path: PathUL<()>,
) -> (ModuleInterface, Module<tr::File>) {
    let modint = add_fun_types(&module, modint, path.clone());
    translate_funs(module, modint, path)
}
