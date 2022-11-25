use super::context::{GlobalContext, ModuleInterface};
use crate::ast::common::{BuiltinType, NamePath, PathUL, Sizes};
use crate::ast::rust::{PreType, PreTypeInner};
use crate::ast::typed_rust::{PostType, PostTypeInner};
use std::collections::{HashMap, HashSet};

pub fn compute_size(typ: &PreType, ctxt: &ModuleInterface, path: &mut PathUL<(), String>) -> usize {
    match &typ.content {
        PreTypeInner::Fun(_args, _out) => todo!(),
        PreTypeInner::Ident(id) => {
            path.push(NamePath::Name(id.get_content().to_string()));
            match ctxt.get_size(path) {
                None => {
                    path.pop();
                    match ctxt.get_size(&PathUL::from_vec(vec![id.get_content()])) {
                        None => {
                            println!("{:?}", id);
                            todo!()
                        }
                        Some(s) => *s,
                    }
                }
                Some(size) => {
                    path.pop();
                    *size
                }
            }
        }
        PreTypeInner::IdentPath(path) => match ctxt.get_size(&path.cleaned()) {
            None => todo!(),
            Some(size) => *size,
        },
        PreTypeInner::IdentParametrized(_, _) => todo!(),

        PreTypeInner::IdentParametrizedPath(path, args) if args.len() == 1 && path.is_vec() => 8,
        PreTypeInner::IdentParametrizedPath(_, _) => todo!(),

        PreTypeInner::Ref(_mutable, _typ) => todo!(),

        PreTypeInner::Tuple(elements) => {
            let mut total_size = 0;
            for el in elements.iter() {
                total_size += compute_size(el, ctxt, path);
            }
            total_size
        }
    }
}

pub fn translate_typ(
    typ: PreType,
    ctxt: &GlobalContext,
    frees: &HashSet<String>,
) -> Option<PostType> {
    match typ.content {
        PreTypeInner::Fun(args, out) => {
            let mut args2 = Vec::new();
            for args in args.into_iter() {
                args2.push(translate_typ(args, ctxt, frees)?)
            }
            let out = translate_typ(*out, ctxt, frees)?;
            Some(PostType {
                content: PostTypeInner::Fun(vec![], args2, Box::new(out)),
            })
        }
        PreTypeInner::Ident(id) if frees.contains(id.get_content()) => Some(PostType {
            content: PostTypeInner::FreeType(id.content()),
        }),
        PreTypeInner::Ident(id) => match id.get_content() {
            "bool" => Some(PostType {
                content: PostTypeInner::BuiltIn(BuiltinType::Bool),
            }),
            "i8" => Some(PostType {
                content: PostTypeInner::BuiltIn(BuiltinType::Int(true, Sizes::S8)),
            }),
            "u8" => Some(PostType {
                content: PostTypeInner::BuiltIn(BuiltinType::Int(false, Sizes::S8)),
            }),
            "i16" => Some(PostType {
                content: PostTypeInner::BuiltIn(BuiltinType::Int(true, Sizes::S16)),
            }),
            "u16" => Some(PostType {
                content: PostTypeInner::BuiltIn(BuiltinType::Int(false, Sizes::S16)),
            }),
            "i32" => Some(PostType {
                content: PostTypeInner::BuiltIn(BuiltinType::Int(true, Sizes::S32)),
            }),
            "u32" => Some(PostType {
                content: PostTypeInner::BuiltIn(BuiltinType::Int(false, Sizes::S32)),
            }),
            "i64" => Some(PostType {
                content: PostTypeInner::BuiltIn(BuiltinType::Int(true, Sizes::S64)),
            }),
            "u64" => Some(PostType {
                content: PostTypeInner::BuiltIn(BuiltinType::Int(false, Sizes::S64)),
            }),
            "isize" => Some(PostType {
                content: PostTypeInner::BuiltIn(BuiltinType::Int(true, Sizes::SUsize)),
            }),
            "usize" => Some(PostType {
                content: PostTypeInner::BuiltIn(BuiltinType::Int(false, Sizes::SUsize)),
            }),
            _ => match (
                ctxt.get_struct(id.get_content()),
                ctxt.get_enum_name(id.get_content()),
            ) {
                (None, None) => {
                    println!("{:?}", id);
                    todo!()
                }
                (Some(_), None) => Some(PostType {
                    content: PostTypeInner::Struct(ctxt.get_path(id.get_content()), Vec::new()),
                }),
                (None, Some(_)) => Some(PostType {
                    content: PostTypeInner::Enum(ctxt.get_path(id.get_content()), Vec::new()),
                }),
                (Some(_), Some(_)) => todo!(),
            },
        },
        PreTypeInner::IdentPath(path) => {
            let path = path.cleaned();
            match (ctxt.get_struct_path(&path), ctxt.get_enum(&path)) {
                (None, None) => todo!(),
                (Some(_), None) => Some(PostType {
                    content: PostTypeInner::Struct(path, Vec::new()),
                }),
                (None, Some(_)) => Some(PostType {
                    content: PostTypeInner::Enum(path, Vec::new()),
                }),
                (Some(_), Some(_)) => todo!(),
            }
        }

        PreTypeInner::IdentParametrizedPath(path, args) => {
            let args: Option<_> = args
                .into_iter()
                .map(|typ| translate_typ(typ, ctxt, frees))
                .collect();
            let path = path.cleaned();
            match (ctxt.get_struct_path(&path), ctxt.get_enum(&path)) {
                (None, None) => todo!(),
                (Some(_), None) => Some(PostType {
                    content: PostTypeInner::Struct(path, args?),
                }),
                (None, Some(_)) => Some(PostType {
                    content: PostTypeInner::Enum(path, args?),
                }),
                (Some(_), Some(_)) => todo!(),
            }
        }
        PreTypeInner::IdentParametrized(id, _) => {
            println!("{:?}", id);
            todo!()
        }

        PreTypeInner::Ref(mutable, typ) => Some(translate_typ(*typ, ctxt, frees)?.to_ref(mutable)),

        PreTypeInner::Tuple(elements) => {
            let mut elements2 = Vec::new();
            for el in elements.into_iter() {
                let el = translate_typ(el, ctxt, frees)?;
                elements2.push(el);
            }
            Some(PostType {
                content: PostTypeInner::Tuple(elements2),
            })
        }
    }
}

pub fn are_compatible(expected: &PostType, got: &PostType) -> bool {
    match (&expected.content, &got.content) {
        (_, PostTypeInner::Diverge) => true,
        (PostTypeInner::BuiltIn(b1), PostTypeInner::BuiltIn(b2)) => b1 == b2,
        (_, PostTypeInner::BuiltIn(_)) | (PostTypeInner::BuiltIn(_), _) => false,
        (PostTypeInner::Box(box1), PostTypeInner::Box(box2)) => are_compatible(&**box1, &**box2),
        (PostTypeInner::Ref(mut1, box1), PostTypeInner::Ref(mut2, box2)) => {
            (*mut2 || !*mut1) && are_compatible(&**box1, &**box2)
        }
        (PostTypeInner::Tuple(vec1), PostTypeInner::Tuple(vec2)) if vec1.len() == vec2.len() => {
            for (t1, t2) in vec1.iter().zip(vec2.iter()) {
                if !are_compatible(t1, t2) {
                    return false;
                }
            }
            true
        }
        (PostTypeInner::Struct(name1, args1), PostTypeInner::Struct(name2, args2)) => {
            if name1 == name2 {
                for (t1, t2) in args1.iter().zip(args2.iter()) {
                    if !are_compatible(t1, t2) {
                        return false;
                    }
                }
                true
            } else {
                false
            }
        }
        (PostTypeInner::FreeType(id1), PostTypeInner::FreeType(id2)) if id1 == id2 => true,
        (PostTypeInner::Fun(free1, args1, out1), PostTypeInner::Fun(free2, args2, out2))
            if free1.is_empty() && free2.is_empty() =>
        {
            let mut b = are_compatible(&*out1, &*out2) && args1.len() == args2.len();
            for (a1, a2) in args1.iter().zip(args2.iter()) {
                b &= are_compatible(a1, a2)
            }
            b
        }
        (PostTypeInner::Enum(path1, args1), PostTypeInner::Enum(path2, args2)) => {
            if path1 == path2 {
                for (t1, t2) in args1.iter().zip(args2.iter()) {
                    if !are_compatible(t1, t2) {
                        return false;
                    }
                }
                true
            } else {
                false
            }
        }
        _ => {
            println!("not compatible :\n {:?}\n {:?}", expected, got);
            todo!()
        }
    }
}

pub fn biggest_compatible(typ1: &PostType, typ2: &PostType) -> Option<PostType> {
    match (&typ1.content, &typ2.content) {
        (_, PostTypeInner::Diverge) => Some(typ1.clone()),
        (PostTypeInner::Diverge, _) => Some(typ2.clone()),
        (PostTypeInner::BuiltIn(btyp1), PostTypeInner::BuiltIn(btyp2)) if btyp1 == btyp2 => {
            Some(typ1.clone())
        }
        (PostTypeInner::Ref(b1, typ1), PostTypeInner::Ref(b2, typ2)) => {
            biggest_compatible(&**typ1, &**typ2).map(|t| t.to_ref(*b1 && *b2))
        }
        (PostTypeInner::Tuple(vec1), PostTypeInner::Tuple(vec2)) if vec1.len() == vec2.len() => {
            let mut vec_out = Vec::new();
            for (typ1, typ2) in vec1.iter().zip(vec2.iter()) {
                if let Some(typ) = biggest_compatible(typ1, typ2) {
                    vec_out.push(typ)
                } else {
                    return None;
                }
            }
            Some(PostType {
                content: PostTypeInner::Tuple(vec_out),
            })
        }
        (PostTypeInner::Struct(s1, args1), PostTypeInner::Struct(s2, args2))
            if s1 == s2 && args1.len() == args2.len() =>
        {
            if args1.is_empty() {
                Some(typ1.clone())
            } else {
                todo!()
            }
        }

        (sub_typ1, sub_typ2) if sub_typ1 == sub_typ2 => Some(typ1.clone()),

        _ => {
            println!(
                "unkown biggest compatible for\n {:?}\n {:?}",
                typ1.content, typ2.content
            );
            todo!()
        }
    }
}

pub fn is_type_int(typ: &Option<PostType>) -> bool {
    if let Some(typ) = typ {
        matches!(&typ.content, PostTypeInner::BuiltIn(_))
    } else {
        false
    }
}

pub fn substitute(typ: PostType, hash_map: &HashMap<String, PostType>) -> PostType {
    let content = match typ.content {
        PostTypeInner::Box(typ) => PostTypeInner::Box(Box::new(substitute(*typ, hash_map))),
        PostTypeInner::BuiltIn(built_in) => PostTypeInner::BuiltIn(built_in),
        PostTypeInner::Diverge => PostTypeInner::Diverge,
        PostTypeInner::FreeType(t) => return hash_map.get(&t).unwrap().clone(),
        PostTypeInner::Fun(_, _, _) => todo!(),
        PostTypeInner::Ref(mutable, typ) => {
            PostTypeInner::Ref(mutable, Box::new(substitute(*typ, hash_map)))
        }
        PostTypeInner::String => PostTypeInner::String,
        PostTypeInner::Struct(name, args) => PostTypeInner::Struct(
            name,
            args.into_iter().map(|t| substitute(t, hash_map)).collect(),
        ),
        PostTypeInner::Enum(name, args) => PostTypeInner::Enum(
            name,
            args.into_iter().map(|t| substitute(t, hash_map)).collect(),
        ),
        PostTypeInner::Tuple(types) => {
            PostTypeInner::Tuple(types.into_iter().map(|t| substitute(t, hash_map)).collect())
        }
    };
    PostType { content }
}
