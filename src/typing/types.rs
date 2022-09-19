use crate::ast::rust::{PreType, PreTypeInner};
use crate::ast::typed_rust::{PostType, PostTypeInner};
use crate::ast::common::Ident;

use std::collections::HashMap;
use super::context::GlobalContext;


pub fn translate_typ(typ : PreType, sizes : &GlobalContext) -> PostType {
    match typ.content {
        PreTypeInner::Fun(args, out) => {
            let mut args2 = Vec::new();
            for args in args.into_iter() {
                args2.push(translate_typ(args, sizes))
            }
            let out = translate_typ(*out, sizes);
            PostType {
                content : PostTypeInner::Fun(args2, Box::new(out)),
                size : todo!(),
            }
        },
        PreTypeInner::Ident(id) => {
            match sizes.get_typ(id.get_content()) {
                None => todo!(),
                Some(post_type) => post_type.clone(),
            }
        },
        PreTypeInner::IdentParametrized(id, mut args) => {
            match id.get_content() {
                "Vec" => {
                    if args.len() == 1 {
                        let typ = translate_typ(args.pop().unwrap(), sizes);
                        PostType {
                            content : PostTypeInner::IdentParametrized(Ident::from_str("Vec"), vec![typ]),
                            size : 8,
                        }
                    } else {
                        todo!()
                    }
                },
                "Box" => {todo!()},
                _ => todo!(),
            }

        },

        PreTypeInner::Ref(mutable, typ) => { translate_typ(*typ, sizes).to_ref(mutable) },

        PreTypeInner::Tuple(elements) => {
            let mut elements2 = Vec::new();
            let mut total_size = 0;
            for el in elements.into_iter() {
                let el = translate_typ(el, sizes);
                total_size += el.size;
                elements2.push(el);
            }
            PostType {
                content : PostTypeInner::Tuple(elements2),
                size : total_size,
            }
        }
    }
}
