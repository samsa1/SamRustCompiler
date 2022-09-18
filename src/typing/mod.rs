use crate::ast::{rust, typed_rust, common::*};
use crate::ast::common::{BuiltinType, Sizes};
use std::collections::HashMap;

pub mod context;
mod structs;
mod expr;
pub mod types;

fn type_funs(funs : Vec<rust::DeclFun>, known_types : &mut context::GlobalContext) -> Vec<typed_rust::DeclFun> {
    let mut fun_types = Vec::new();
    for fun_decl in funs.iter() {
        let args : Vec<typed_rust::PostType> = fun_decl.args.iter().map(|(n, b, typ)| types::translate_typ(typ.clone(), known_types)).collect();
        let output = types::translate_typ(fun_decl.output.clone(), known_types);
        fun_types.push((args.clone(), output.clone()));
        let fun_typ = typed_rust::PostType {
            content : typed_rust::PostTypeInner::Fun(args, Box::new(output)),
            mutable : false,
            size : 8, // todo!()
        };

        known_types.insert(fun_decl.name.get_content().to_string(), fun_typ);
    }
    assert_eq!(fun_types.len(), funs.len());
    let mut fun_vec = Vec::new();

    for (fun_decl, (args_typ, output)) in funs.into_iter().zip(fun_types.into_iter()) {
        let in_types : Vec<(Ident, bool, typed_rust::PostType)> = fun_decl.args.into_iter().zip(args_typ.into_iter()).map(|((name, b, pre_type), post_type)| (name, b, post_type)).collect();
        let mut local_ctxt = context::LocalContext::new();

        let content = expr::type_block(fun_decl.content, known_types, &mut local_ctxt, &output);

        fun_vec.push(
            typed_rust::DeclFun {
                name : fun_decl.name,
                args : in_types,
                output,
                content,
            }
        );
    }

    fun_vec
}

pub fn type_inferencer(file : rust::File) -> typed_rust::File {
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
        name : file.name,
        structs,
        funs,
    }
}