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
    let mut module_interface = structs::type_structs(&mut file);
    module_interface = consts::handle(&mut file, module_interface);
    module_interface = implementation::handle(&mut file, module_interface, &mut path.clone());
    let mut path2 = path.add_loc();
    let (module_interface, file) = functions::handle(file, module_interface, path);

    path2.push(NamePath::Name(Ident::new("main", Location::default())));
    if needs_main && module_interface.get_fun(&path2).is_none() {
        println!("No main");
        std::process::exit(1)
    }
    (module_interface, file)
}
