use crate::frontend::Module;
use crate::ast::rust::{self, Decl, File};
use crate::ast::common::{NamePath, PathUL, Ident};
use super::context::ModuleInterface;
use std::str::FromStr;
use std::collections::HashMap;

/*
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
            let errs = vec![errors::TypeError::unknown_struct(impl_decl.name)];
            err_reporter.report(errs)
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
*/

pub fn handle(module : &mut Module<File>, modint : &mut ModuleInterface) {
    let mut local_structs = Vec::new();
    local_structs.append(&mut module.content.content);
    for decl in local_structs.into_iter() {
        match decl {
            Decl::Impl(impl_decl) => {
                panic!("Not implemented");
                if modint.get_struct(&PathUL::new(vec![NamePath::Name(impl_decl.name.get_content().to_string())])).is_none() {
                    let errs = vec![];
                    println!("Todo in handle interface");
                    module.content.err_reporter.report(errs)
                }
                let mut funs = Vec::new();
                for mut decl_fun in impl_decl.content {
                    let args = match decl_fun.self_arg {
                        None => decl_fun.args,
                        Some(b) => {
                            if modint
                                .impl_method(
                                    impl_decl.name.get_content(),
                                    decl_fun.name.content(),
                                    decl_fun.name.get_content().to_string(),
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

                    funs.push(Decl::Fun(rust::DeclFun {
                        args,
                        ..decl_fun
                    }))
                }
                match module.submodules.get_mut(impl_decl.name.get_content()) {
                    None => {
                        assert!(module.submodules.insert(impl_decl.name.get_content().to_string(), 
                        (true, Module::build(rust::File {
                            name : impl_decl.name.content(),
                            content : funs,
                            dep : Vec::new(),
                            err_reporter : module.content.err_reporter.clone(),
                        }, HashMap::new()))).is_none())
                    }
                    Some((_, sb)) => sb.content.content.append(&mut funs)
                }
                todo!()
            },
            decl => module.content.content.push(decl),
        }
    }
}