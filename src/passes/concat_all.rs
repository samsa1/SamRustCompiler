use crate::ast::low_level_repr::{DeclFun, File};
use crate::frontend::Module;

fn rewrite_rec(module: &mut Module<File>, funs: &mut Vec<DeclFun>) {
    funs.append(&mut module.content.funs);
    for (_, (_, module)) in module.submodules.iter_mut() {
        rewrite_rec(module, funs)
    }
}

pub fn rewrite(mut module: Module<File>) -> File {
    let mut funs = Vec::new();
    rewrite_rec(&mut module, &mut funs);
    File { funs }
}
