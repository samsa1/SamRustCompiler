use crate::ast::rust::{PreType, PreTypeInner};
use crate::ast::typed_rust::{PostType, PostTypeInner};
use crate::ast::common::{Sizes, BuiltinType};

use super::context::GlobalContext;

pub fn compute_size(typ : &PreType, sizes : &GlobalContext) -> usize {
    match &typ.content {
        PreTypeInner::Fun(_args, _out) => todo!(),
        PreTypeInner::Ident(id) => {
            match sizes.get_size(id.get_content()) {
                None => todo!(),
                Some(size) => size,
            }
        },
        PreTypeInner::IdentParametrized(id, args) => {
            match id.get_content() {
                "Vec" => {
                    if args.len() == 1 {
                        8
                    } else {
                        todo!()
                    }
                },
                "Box" => {todo!()},
                _ => todo!(),
            }

        },

        PreTypeInner::Ref(_mutable, _typ) => todo!(),

        PreTypeInner::Tuple(elements) => {
            let mut total_size = 0;
            for el in elements.iter() {
                total_size += compute_size(el, sizes);
            }
            total_size
        }
    }
}

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
                            content : PostTypeInner::IdentParametrized(String::from("Vec"), vec![typ]),
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
            for el in elements.into_iter() {
                let el = translate_typ(el, sizes);
                elements2.push(el);
            }
            PostType {
                content : PostTypeInner::Tuple(elements2),
            }
        }
    }
}

pub fn are_compatible(expected : &PostType, got : &PostType) -> bool {
    match (&expected.content, &got.content) {
        (_, PostTypeInner::Diverge) => true,
        (PostTypeInner::BuiltIn(b1), PostTypeInner::BuiltIn(b2)) => b1 == b2,
        (PostTypeInner::Struct(name1), PostTypeInner::Struct(name2)) => name1 == name2,
        (PostTypeInner::Box(box1), PostTypeInner::Box(box2)) => are_compatible(&**box1, &**box2),
        (PostTypeInner::Ref(mut1, box1), PostTypeInner::Ref(mut2, box2)) => (*mut2 || !*mut1) && are_compatible(&**box1, &**box2),
        (PostTypeInner::Tuple(vec1), PostTypeInner::Tuple(vec2)) if vec1.len() == vec2.len() => {
            for (t1, t2) in vec1.iter().zip(vec2.iter()) {
                if !are_compatible(t1, t2) {
                    return false
                }
            }; true
        },
        (PostTypeInner::IdentParametrized(name1, args1),
         PostTypeInner::IdentParametrized(name2, args2)) => {
            if name1 == name2 {
                for (t1, t2) in args1.iter().zip(args2.iter()) {
                    if !are_compatible(t1, t2) {
                        return false
                    }
                }
                true
            } else {
                false
            }
         }
        _ => {println!("not compatible :\n {:?}\n {:?}", expected, got); todo!()},
    }
}

pub fn biggest_compatible(typ1 : &PostType, typ2 : &PostType) -> Option<PostType> {
    match (&typ1.content, &typ2.content) {
        (_, PostTypeInner::Diverge) => Some(typ1.clone()),
        (PostTypeInner::Diverge, _) => Some(typ2.clone()),
        (PostTypeInner::BuiltIn(btyp1),
         PostTypeInner::BuiltIn(btyp2)) if btyp1 == btyp2 => Some(typ1.clone()),
        (PostTypeInner::Ref(b1, typ1),
         PostTypeInner::Ref(b2, typ2)) =>
            biggest_compatible(&**typ1, &**typ2).map(|t| t.to_ref(*b1 && *b2)),
        (PostTypeInner::Tuple(vec1),
         PostTypeInner::Tuple(vec2)) if vec1.len() == vec2.len() => {
            let mut vec_out = Vec::new();
            for (typ1, typ2) in vec1.iter().zip(vec2.iter()) {
                if let Some(typ) = biggest_compatible(typ1, typ2) {
                    vec_out.push(typ)
                } else {
                    return None
                }
            };
            Some(PostType {
                content : PostTypeInner::Tuple(vec_out),
//                size : typ1.size,
            })
         },
        (PostTypeInner::Struct(s1), PostTypeInner::Struct(s2)) if s1 == s2 =>
            Some(typ1.clone()),

        _ => {println!("unkown biggest compatible for\n {:?}\n {:?}", typ1.content, typ2.content); todo!()}
    }
}


pub fn is_type_int(typ : &Option<PostType>) -> bool {
    if let Some(typ) = typ {
        matches!(&typ.content, PostTypeInner::BuiltIn(_))
    } else {
        false
    }
}

pub fn can_be_deref(typ : &PostType) -> bool {
    matches!(&typ.content, PostTypeInner::BuiltIn(_))
}

pub fn type_int_name(typ : &PostType) -> Option<&'static str> {
    match &typ.content {
        PostTypeInner::BuiltIn(BuiltinType::Int(b, size)) => {
            Some(match (b, size) {
                (true,  Sizes::S32) => "i32",
                (false, Sizes::S32) => "u32",
                (true,  Sizes::S64) => "i64",
                (false, Sizes::S64) => "u64",
                (true,  Sizes::SUsize) => "isize",
                (false, Sizes::SUsize) => "usize",
            })
        },
        _ => None
    }
}

pub fn builtin_name(typ : &BuiltinType) -> &'static str {
    match typ {
        BuiltinType::Int(b, size) => {
            match (b, size) {
                (true,  Sizes::S32) => "i32",
                (false, Sizes::S32) => "u32",
                (true,  Sizes::S64) => "i64",
                (false, Sizes::S64) => "u64",
                (true,  Sizes::SUsize) => "isize",
                (false, Sizes::SUsize) => "usize",
            }
        },
        BuiltinType::Bool => "bool",
    }
}