use super::context::{GlobalContext, ModuleInterface};
use crate::ast::common::{NamePath, PathUL};
use crate::ast::rust::{Decl, File};
use crate::frontend::Module;

pub fn handle(
    module: &mut Module<File>,
    modint: ModuleInterface,
    path: &mut PathUL<()>,
) -> ModuleInterface {
    let mut ctxt = GlobalContext::new(path.clone(), modint);
    for decl in module.content.content.iter() {
        match decl {
            Decl::Impl(impl_decl) => match (
                ctxt.get_struct(impl_decl.name.get_content()),
                ctxt.get_enum_name(impl_decl.name.get_content()),
            ) {
                (None, None) => {
                    let errs = vec![];
                    println!("Todo in handle interface");
                    module.content.err_reporter.report(errs)
                }
                (Some(_), None) | (None, Some(_)) => {
                    for decl_fun in &impl_decl.content {
                        match decl_fun.self_arg {
                            None => (),
                            Some(_) => {
                                if ctxt
                                    .impl_method(
                                        impl_decl.name.get_content(),
                                        decl_fun.name.get_content().to_string(),
                                        PathUL::from_vec(vec![
                                            impl_decl.name.get_content(),
                                            decl_fun.name.get_content(),
                                        ]),
                                    )
                                    .is_some()
                                {
                                    todo!()
                                }
                            }
                        }
                    }
                }
                (Some(_), Some(_)) => todo!(),
            },

            _ => (),
        }
    }
    let mut modint = ctxt.extract_module();
    for (name, (_, module)) in module.submodules.iter_mut() {
        path.push(NamePath::Name(name.to_string()));
        modint = handle(module, modint, path);
        path.pop();
    }
    modint
}
