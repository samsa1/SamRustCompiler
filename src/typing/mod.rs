use crate::ast::{common::*, rust, typed_rust};
use std::collections::HashSet;

pub mod context;
mod expr;
mod scopes;
mod structs;
pub mod types;

fn type_funs(
    funs: Vec<rust::DeclFun>,
    known_types: &mut context::GlobalContext,
) -> Vec<typed_rust::DeclFun> {
    let mut fun_types = Vec::new();
    let mut fun_names = HashSet::new();
    for fun_decl in funs.iter() {
        let args: Vec<typed_rust::PostType> = fun_decl
            .args
            .iter()
            .map(|(_, _, typ)| types::translate_typ(typ.clone(), known_types))
            .collect();
        let output = types::translate_typ(fun_decl.output.clone(), known_types);
        fun_types.push((args.clone(), output.clone()));
        let fun_typ = typed_rust::PostType {
            content: typed_rust::PostTypeInner::Fun(args, Box::new(output)),
        };

        known_types.insert(fun_decl.name.get_content().to_string(), fun_typ);
    }
    assert_eq!(fun_types.len(), funs.len());
    let mut fun_vec = Vec::new();

    for (fun_decl, (args_typ, output)) in funs.into_iter().zip(fun_types.into_iter()) {
        let in_types: Vec<(Ident, bool, typed_rust::PostType)> = fun_decl
            .args
            .into_iter()
            .zip(args_typ.into_iter())
            .map(|((name, b, _pre_type), post_type)| (name, b, post_type))
            .collect();
        let mut local_ctxt = context::LocalContext::new(&in_types);

        let content = expr::type_block(
            fun_decl.content,
            known_types,
            &mut local_ctxt,
            &output,
            Some(&output),
        );
        if !fun_names.insert(fun_decl.name.get_content().to_string()) {
            todo!();
        }
        fun_vec.push(typed_rust::DeclFun {
            name: fun_decl.name,
            args: in_types,
            output,
            content,
        });
    }

    fun_vec
}

pub fn type_inferencer(file: rust::File) -> typed_rust::File {
    let mut funs = Vec::new();
    let mut structs = Vec::new();
    for decl in file.content.into_iter() {
        match decl {
            rust::Decl::Fun(f) => funs.push(f),
            rust::Decl::Struct(s) => structs.push(s),
        }
    }

    let (mut known_types, structs) = structs::type_structs(structs);
    let funs = type_funs(funs, &mut known_types);

    typed_rust::File {
        name: file.name,
        structs,
        funs,
    }
}
