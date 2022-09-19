use super::context;
use crate::ast::{rust, typed_rust};
use crate::ast::common::*;
use super::types::translate_typ;

fn compatible_types(type1 : &Option<typed_rust::PostType>, type2 : typed_rust::PostType) -> typed_rust::PostType {
    match type1 {
        None => type2,
        Some(_type1) => todo!{}
    }
}

fn biggest_compatible(typ1 : &typed_rust::PostType, typ2 : &typed_rust::PostType) -> Option<typed_rust::PostType> {
    if typ1 == typ2 {
        Some(typ1.clone())
    } else {
        None
    }
}

fn is_type_int(typ : &Option<typed_rust::PostType>) -> bool {
    if let Some(typ) = typ {
        match &typ.content {
            typed_rust::PostTypeInner::BuiltIn(BuiltinType::Int(_, _)) => true,
            _ => false
        }
    } else {
        false
    }
}

fn type_int_name(typ : &typed_rust::PostType) -> Option<&'static str> {
    match &typ.content {
        typed_rust::PostTypeInner::BuiltIn(BuiltinType::Int(b, size)) => {
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

fn get_index_function(ctxt : &context::GlobalContext, typ : &typed_rust::PostType) -> Option<(&'static str, typed_rust::PostType)> {
    match &typ.content {
        typed_rust::PostTypeInner::IdentParametrized(id, param) if id.get_content() == "vec" => {
            Some(("get_element_vec", param[0].clone()))
        },
        _ => None,
    }
}

pub fn type_checker(ctxt : &context::GlobalContext, expr : rust::Expr, loc_ctxt : &mut context::LocalContext, out : &typed_rust::PostType) -> typed_rust::Expr {
    let mut translated_typ = expr.typed.map(|t| translate_typ(t, ctxt));
    let (found_type, content) = 
        match *expr.content {
            rust::ExprInner::Bool(b) => {
                (typed_rust::PostType::bool(), typed_rust::ExprInner::Bool(b))
            },

            rust::ExprInner::Int(i) => {
                if is_type_int(&translated_typ) {
                    let typ = translated_typ.unwrap();
                    translated_typ = None;
                    (typ, typed_rust::ExprInner::Int(i))
                } else {
                    (typed_rust::PostType::i32(), typed_rust::ExprInner::Int(i))
                }
            },

            rust::ExprInner::Var(var_name) => {
                let typ =
                    match loc_ctxt.get_typ(&var_name) {
                        Some(typ) => typ.clone(),
                        None => panic!("undefined variable")
                    };
                (typ, typed_rust::ExprInner::Var(var_name))
            },

            rust::ExprInner::Ref(b, expr) => {
                let mut expr = type_checker(ctxt, expr, loc_ctxt, out);
                (Box::new(expr.typed.clone()).to_ref(b),
                typed_rust::ExprInner::Ref(b, expr))
            },

            rust::ExprInner::Index(e1, e2) => {
                let e1 = type_checker(ctxt, e1, loc_ctxt, out);
                let e2 = type_checker(ctxt, e2, loc_ctxt, out);
                if let Some((index_function, return_type)) = get_index_function(ctxt, &e1.typed) {
                    if typed_rust::PostTypeInner::BuiltIn(BuiltinType::Int(true, Sizes::S32)) == e2.typed.content {
                        (return_type,
                        typed_rust::ExprInner::FunCall(Ident::from_str(index_function), vec![e1, e2]))
                    } else {
                        todo!()
                    }
                } else {
                    todo!()
                }
            },

            rust::ExprInner::BinaryOp(BinOperator::Set, e1, e2) => {
                let e1 = type_checker(ctxt, e1, loc_ctxt, out);
                let e2 = type_checker(ctxt, e2, loc_ctxt, out);
                if are_compatible(&e1.typed, &e2.typed) {
                    (typed_rust::PostType::unit(), 
                    typed_rust::ExprInner::Set(e1, e2))
                } else {
                    println!("not compatible for set {:?} {:?}", e1.typed, e2.typed);
                    todo!()
                }
            
            },

            rust::ExprInner::BinaryOp(BinOperator::Add, e1, e2) => {
                let e1 = type_checker(ctxt, e1, loc_ctxt, out);
                let e2 = type_checker(ctxt, e2, loc_ctxt, out);
                if let Some(typ_name) = type_int_name(&e1.typed) {
                    if &e1.typed.content == &e2.typed.content {
                        let mut name = String::from(typ_name);
                        name.push_str("_add");
                        (e1.typed.clone(),
                        typed_rust::ExprInner::FunCall(Ident::from_str(&name), vec![e1, e2]))
                    } else {
                        todo!()
                    }
                } else {
                    todo!()
                }
            
            },

            rust::ExprInner::FunCall(var_name, args) => {
                let typ =
                    match loc_ctxt.get_typ(&var_name) {
                        Some(typ) => typ.clone(),
                        None => todo!()
                };
                if let typed_rust::PostTypeInner::Fun(args_typ, output) = typ.content {
                    if args.len() != args_typ.len() {
                        panic!("not allowed")
                    }

                    let mut args2 = Vec::new();
                    for (expr, arg) in args.into_iter().zip(args_typ.iter()) {
                        let expr = type_checker(ctxt, expr, loc_ctxt, out);
                        if are_compatible(arg, &expr.typed) {
                            args2.push(expr)
                        };
                    }
                    (*output,
                    typed_rust::ExprInner::FunCall(var_name, args2))

                } else {
                    panic!("not allowed")
                }
            },

            rust::ExprInner::MacroCall(name, mut args) if name.get_content() == "print" => {
                if args.len() == 1 {
                    match *args.pop().unwrap().content {
                        rust::ExprInner::String(s) => {
                            (typed_rust::PostType::unit(),
                            typed_rust::ExprInner::Print(s))
                        },
                        _ => todo!(),
                    }
                } else {
                    todo!()
                }
            },

            rust::ExprInner::MacroCall(name, vec) if name.get_content() == "vec" => {
                let mut typ = None;
                let mut vec2 = Vec::new();
                for expr in vec.into_iter() {
                    let expr = type_checker(ctxt, expr, loc_ctxt, out);
                    match &typ {
                        None => typ = Some(expr.typed.clone()),
                        Some(typ) => if are_compatible(typ, &expr.typed) {
                            vec2.push(expr)
                        } else {
                            todo!()
                        }
                    }
                };
                let size = match &typ {
                    None => todo!(),
                    Some(typ) => typ.size
                };
                (typed_rust::PostType {
                    content : typed_rust::PostTypeInner::IdentParametrized(Ident::from_str("vec"), vec![typ.unwrap()]),
                    size : 8,
                },
                typed_rust::ExprInner::Vec(size, vec2))
            },

            rust::ExprInner::MacroCall(name, vec) => {
                panic!("should not happen")
            },

            rust::ExprInner::Method(_, _, _) => todo!(),

            rust::ExprInner::Bloc(bloc) => {
                let bloc = type_block(bloc, ctxt, loc_ctxt, out);
                (bloc.last_type.clone(),
                typed_rust::ExprInner::Bloc(bloc))
            },

            rust::ExprInner::Deref(_expr) => todo!(),

            rust::ExprInner::If(e1, e2, e3) => {
                let expr1 = type_checker(ctxt, e1, loc_ctxt, out);
                if expr1.typed.content != typed_rust::PostTypeInner::BuiltIn(BuiltinType::Bool) {
                    panic!("Type error")
                }
                let expr2 = type_checker(ctxt, e2, loc_ctxt, out);
                let expr3 = type_checker(ctxt, e3, loc_ctxt, out);

                if let Some(typ) = biggest_compatible(&expr2.typed, &expr3.typed) {
                    (typ, typed_rust::ExprInner::If(expr1, expr2, expr3))
                } else {
                    panic!("incompatible types")
                }
            },

            rust::ExprInner::BuildStruct(name, args) => {
                if let Some(mut struct_info) = ctxt.struct_infos(name.get_content()) {
                    let mut args2 = Vec::new();
                    for (name, expr) in args.into_iter() {
                        let expr = type_checker(ctxt, expr, loc_ctxt, out);
                        if let Some(typ) = struct_info.get_typ(name.get_content()) {
                            if are_compatible(typ, &expr.typed) {
                                args2.push((name, expr));
                            } else {
                                todo!()
                            }
                        } else {
                            todo!()
                        }
                    }
                    match struct_info.check_finished() {
                        Some(name) => todo!(),
                        None =>
                            (ctxt.get_typ(name.get_content()).unwrap().clone(),
                                typed_rust::ExprInner::BuildStruct(name, args2))
                    }
                } else {
                    todo!()
                }
            },

            rust::ExprInner::Tuple(vec1) => {
                let mut vec_expr = Vec::new();
                let mut vec_type = Vec::new();
                let mut size = 0;
                for expr in vec1.into_iter() {
                    let expr = type_checker(ctxt, expr, loc_ctxt, out);
                    size += expr.typed.size;
                    vec_type.push(expr.typed.clone());
                    vec_expr.push(expr);
                }
                (typed_rust::PostType {
                    content : typed_rust::PostTypeInner::Tuple(vec_type),
                    size
                },
                typed_rust::ExprInner::Tuple(vec_expr))
            },

            rust::ExprInner::Proj(expr, Projector::Int(id)) => {
                todo!()
            },

            rust::ExprInner::Proj(expr, Projector::Name(name)) => {
                let expr = type_checker(ctxt, expr, loc_ctxt, out);
                match &expr.typed.content {
                    typed_rust::PostTypeInner::Struct(s) => {
                        match ctxt.get_struct(&s) {
                            None => panic!("should not happend"),
                            Some(struct_info) => 
                                if let Some(typ) = struct_info.get_field_typ(name.get_content()) {
                                    (typ.clone(), typed_rust::ExprInner::Proj(expr, Projector::Name(name)))
                                } else {
                                    todo!()
                                },
                        }
                    },
                    _ => todo!(),
                }
            },

            _ => todo!(),
        };
    typed_rust::Expr {
        content : Box::new(content),
        typed : compatible_types(&translated_typ, found_type),
        loc : expr.loc,
    }
}

pub fn are_compatible(expected : &typed_rust::PostType, got : &typed_rust::PostType) -> bool {
    match (&expected.content, &got.content) {
        (_, typed_rust::PostTypeInner::Diverge) => true,
        (typed_rust::PostTypeInner::BuiltIn(b1), typed_rust::PostTypeInner::BuiltIn(b2)) => b1 == b2,
        (typed_rust::PostTypeInner::Struct(name1), typed_rust::PostTypeInner::Struct(name2)) => name1 == name2,
        (typed_rust::PostTypeInner::Box(box1), typed_rust::PostTypeInner::Box(box2)) => are_compatible(&*box1, &*box2),
        (typed_rust::PostTypeInner::Ref(mut1, box1), typed_rust::PostTypeInner::Ref(mut2, box2)) => (*mut2 || !*mut1) && are_compatible(&*box1, &*box2),
        (typed_rust::PostTypeInner::Tuple(vec1), typed_rust::PostTypeInner::Tuple(vec2)) if vec1.len() == vec2.len() => {
            for (t1, t2) in vec1.iter().zip(vec2.iter()) {
                if !are_compatible(t1, t2) {
                    return false
                }
            }; true
        },
        _ => todo!(),
    }
}


pub fn type_block(bloc : rust::Bloc,
    ctxt : &context::GlobalContext,
    loc_ctxt : &mut context::LocalContext,
    output : &typed_rust::PostType)
-> typed_rust::Bloc {
    loc_ctxt.add_layer();
    let mut content = Vec::new();
    let mut reachable = true;
    for instr in bloc.content {
        match instr {
            rust::Instr::Expr(expr) => {
                let expr = type_checker(ctxt, expr, loc_ctxt, output);
                content.push(typed_rust::Instr::Expr(expr));
//                todo!();
            },
            rust::Instr::Binding(b, ident, expr) => {
                let mut expr = type_checker(ctxt, expr, loc_ctxt, output);
                loc_ctxt.add_var(&ident, &expr.typed);
                content.push(typed_rust::Instr::Binding(b, ident, expr));
//                todo!();
            },
            rust::Instr::While(_, _) => todo!(),
            rust::Instr::Return(None) => {
                match &output.content {
                    typed_rust::PostTypeInner::Tuple(l) if l.len() == 0 => {
                        content.push(typed_rust::Instr::Return(None));
                        reachable = false;
                    },
                    _ => todo!()
                }
            },
            rust::Instr::Return(Some(expr)) => {
                let expr = type_checker(ctxt, expr, loc_ctxt, output);
                if are_compatible(output, &expr.typed) {
                    content.push(typed_rust::Instr::Return(Some(expr)));
                    reachable = false;
                } else {
                    todo!()
                }

            }
        }
    };

    let last_type;

    if reachable {
        last_type = match content.pop() {
            Some(typed_rust::Instr::Expr(e)) => {let typ = e.typed.clone(); content.push(typed_rust::Instr::Expr(e)); typ},
            _ => typed_rust::PostType::unit(),
        };
    } else {
        last_type = typed_rust::PostType::diverge();
    }

    let out_values = match loc_ctxt.pop_layer() {
        | Some(map) => map,
        | None => panic!("should not happen"),
    };

    typed_rust::Bloc{ content, values : out_values, last_type }
}
