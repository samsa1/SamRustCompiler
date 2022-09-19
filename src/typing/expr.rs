use super::context;
use crate::ast::{rust, typed_rust};
use crate::ast::common::*;
use super::types::*;

fn compatible_types(type1 : &Option<typed_rust::PostType>, type2 : typed_rust::PostType) -> typed_rust::PostType {
    match type1 {
        None => type2,
        Some(_type1) => todo!{}
    }
}

fn get_index_function(ctxt : &context::GlobalContext, typ : &typed_rust::PostType) -> Option<(&'static str, typed_rust::PostType)> {
    match &typ.content {
        typed_rust::PostTypeInner::IdentParametrized(id, param) if id.get_content() == "Vec" => {
            Some(("get_element_vec", param[0].clone()))
        },
        typed_rust::PostTypeInner::Ref(_, typ) => {
            match &typ.content {
                typed_rust::PostTypeInner::IdentParametrized(id, param) if id.get_content() == "Vec" => {
                    Some(("get_element_vec", param[0].clone()))
                },
                _ => None,
            }
        },
        _ => None,
    }
}

fn arith_fun_name(binop : BinOperator) -> Option<&'static str> {
    match binop {
        BinOperator::Add => Some("_add"),
        BinOperator::Div => Some("_div"),
        BinOperator::Mod => Some("_mod"),
        BinOperator::Sub => Some("_sub"),
        _ => None
    }
}

fn arith_cmp_fun(binop : BinOperator) -> Option<&'static str> {
    match binop {
        BinOperator::Lower      => Some("_lower"),
        BinOperator::LowerEq    => Some("_lower_eq"),
        BinOperator::Greater    => Some("_greater"),
        BinOperator::GreaterEq  => Some("_greater_eq"),
        _ => None
    }
}

pub fn type_checker(ctxt : &context::GlobalContext,
        expr : rust::Expr,
        loc_ctxt : &mut context::LocalContext, out : &typed_rust::PostType,
        expected_typ : Option<&typed_rust::PostType>) -> (bool, typed_rust::Expr)
{
    let mut translated_typ = expr.typed.map(|t| translate_typ(t, ctxt));
    let (affectable, found_type, content) =
        match *expr.content {
            rust::ExprInner::Bool(b) => {
                (false, typed_rust::PostType::bool(),
                    typed_rust::ExprInner::Bool(b))
            },

            rust::ExprInner::Int(i) => {
                if is_type_int(&translated_typ) {
                    let typ = translated_typ.unwrap();
                    translated_typ = None;
                    (false, typ, typed_rust::ExprInner::Int(i))
                } else {
                    (false, typed_rust::PostType::i32(),
                        typed_rust::ExprInner::Int(i))
                }
            },

            rust::ExprInner::Var(var_name) => {
                let (mutable, typ) =
                    match loc_ctxt.get_typ(&var_name) {
                        Some((mutable, typ)) => (*mutable, typ.clone()),
                        None => panic!("undefined variable {} at {:?}", var_name.get_content(), expr.loc)
                    };
                (mutable, typ, typed_rust::ExprInner::Var(var_name))
            },

            rust::ExprInner::Ref(b, expr) => {
                let expected_typ = match expected_typ {
                    None => None,
                    Some(typ) => {
                        match &typ.content {
                            typed_rust::PostTypeInner::Ref(b2, typ) if b || !b2 => {
                                Some(&**typ)
                            },
                            _ => todo!()
                        }
                    },
                };
                let mut expr = type_checker(ctxt, expr, loc_ctxt, out, expected_typ).1;
                (false,
                 Box::new(expr.typed.clone()).to_ref(b),
                 typed_rust::ExprInner::Ref(b, expr))
            },

            rust::ExprInner::Index(e1, e2) => {
                let (affectable, e1) = type_checker(ctxt, e1, loc_ctxt, out, None);
                let e2 = type_checker(ctxt, e2, loc_ctxt, out, None).1;
                if let Some((index_function, return_type)) = get_index_function(ctxt, &e1.typed) {
                    if typed_rust::PostTypeInner::BuiltIn(BuiltinType::Int(true, Sizes::S32)) == e2.typed.content {
                        (affectable || e1.typed.is_mut_ref(),
                        return_type,
                        typed_rust::ExprInner::FunCall(Ident::from_str(index_function), vec![e1, e2]))
                    } else {
                        todo!()
                    }
                } else {
                    println!("didn't manage in get index in {:?}[{:?}]", e1, e2);
                    todo!()
                }
            },

            rust::ExprInner::BinaryOp(BinOperator::Set, e1, e2) => {
                let (affectable, e1) = type_checker(ctxt, e1, loc_ctxt, out, None);
                let e2 = type_checker(ctxt, e2, loc_ctxt, out, None).1;
                if affectable && are_compatible(&e1.typed, &e2.typed) {
                    (false,
                    typed_rust::PostType::unit(), 
                    typed_rust::ExprInner::Set(e1, e2))
                } else {
                    println!("not compatible for set {:?} {:?}", e1.typed, e2.typed);
                    todo!()
                }
            
            },

            rust::ExprInner::BinaryOp(binop, e1, e2) => {
                if let Some(fun_suffix) = arith_fun_name(binop) {
                    let e1 = type_checker(ctxt, e1, loc_ctxt, out, None).1;
                    let e2 = type_checker(ctxt, e2, loc_ctxt, out, None).1;
                    if let Some(typ_name) = type_int_name(&e1.typed) {
                        if &e1.typed.content == &e2.typed.content {
                            let mut name = String::from(typ_name);
                            name.push_str(fun_suffix);
                            (false, e1.typed.clone(),
                            typed_rust::ExprInner::FunCall(Ident::from_str(&name), vec![e1, e2]))
                        } else {
                            todo!()
                        }
                    } else {
                        todo!()
                    }
                } else if let Some(fun_suffix) = arith_cmp_fun(binop) {
                    let e1 = type_checker(ctxt, e1, loc_ctxt, out, None).1;
                    let e2 = type_checker(ctxt, e2, loc_ctxt, out, None).1;
                    if let Some(typ_name) = type_int_name(&e1.typed) {
                        if are_compatible(&e1.typed, &e2.typed) {
                            let mut name = String::from(typ_name);
                            name.push_str(fun_suffix);
                            (false, typed_rust::PostType::bool(),
                            typed_rust::ExprInner::FunCall(Ident::from_str(&name), vec![e1, e2]))
                        } else {
                            println!("not compatible for cmp {:?} {:?}", e1.typed.content, e2.typed.content);
                            todo!()
                        }
                    } else {
                        todo!()
                    }
                } else {
                    println!("operator not implemented {:?}", binop);
                    todo!()
                }
            },

            rust::ExprInner::UnaryOp(_, _) => todo!(),

            rust::ExprInner::FunCall(var_name, args) => {
                let typ =
                    match loc_ctxt.get_typ(&var_name) {
                        Some((_, typ)) => typ.clone(),
                        None => { match ctxt.get_typ(var_name.get_content()) {
                            Some(typ) => typ.clone(),
                            None => todo!()
                        }}
                };
                if let typed_rust::PostTypeInner::Fun(args_typ, output) = typ.content {
                    if args.len() != args_typ.len() {
                        panic!("not allowed")
                    }

                    let mut args2 = Vec::new();
                    for (expr, arg) in args.into_iter().zip(args_typ.iter()) {
                        let expr = type_checker(ctxt, expr, loc_ctxt, out, Some(arg)).1;
                        if are_compatible(arg, &expr.typed) {
                            args2.push(expr)
                        };
                    }
                    (false, *output,
                    typed_rust::ExprInner::FunCall(var_name, args2))

                } else {
                    panic!("not allowed")
                }
            },

            rust::ExprInner::MacroCall(name, mut args) if name.get_content() == "print" => {
                if args.len() == 1 {
                    match *args.pop().unwrap().content {
                        rust::ExprInner::String(s) => {
                            (false, typed_rust::PostType::unit(),
                            typed_rust::ExprInner::Print(s))
                        },
                        _ => todo!(),
                    }
                } else {
                    todo!()
                }
            },

            rust::ExprInner::MacroCall(name, vec) if name.get_content() == "vec" => {
                let mut vec2 = Vec::new();
                let el_expected_type : Option<&typed_rust::PostType> = match expected_typ {
                    None => None,
                    Some(typ) => {
                        match &typ.content {
                            typed_rust::PostTypeInner::IdentParametrized(id, args)
                                if id.get_content() == "Vec" && args.len() == 1 => {
                                    Some(&args[0])
                                },
                            _ => todo!(),
                        }
                    }
                };
                let mut typ = el_expected_type.map(|t| t.clone());
                for expr in vec.into_iter() {
                    let expr = type_checker(ctxt, expr, loc_ctxt, out, el_expected_type).1;
                    match &typ {
                        None => typ = Some(expr.typed.clone()),
                        Some(typ2) =>
                            if let Some(typ2) = biggest_compatible(typ2, &expr.typed) {
                                typ = Some(typ2);
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
                (false,
                typed_rust::PostType {
                    content : typed_rust::PostTypeInner::IdentParametrized(Ident::from_str("Vec"), vec![typ.unwrap()]),
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
                (false, bloc.last_type.clone(),
                typed_rust::ExprInner::Bloc(bloc))
            },

            rust::ExprInner::Deref(expr) => {
                let expr = type_checker(ctxt, expr, loc_ctxt, out, None).1;
                match &expr.typed.content {
                    typed_rust::PostTypeInner::Box(typ) => {
                        (false, *typ.clone(), typed_rust::ExprInner::Deref(expr))
                    },
                    typed_rust::PostTypeInner::Ref(mutable, typ) => {
                        (*mutable, *typ.clone(), typed_rust::ExprInner::Deref(expr))
                    },
                    _ => {println!("trying to deref {:?}", expr.typed); todo!() }
                }
            },

            rust::ExprInner::If(e1, e2, e3) => {
                let expr1 = type_checker(ctxt, e1, loc_ctxt, out, Some(&typed_rust::PostType::bool())).1;
                if expr1.typed.content != typed_rust::PostTypeInner::BuiltIn(BuiltinType::Bool) {
                    panic!("Type error")
                }
                let expr2 = type_checker(ctxt, e2, loc_ctxt, out, expected_typ).1;
                let expr3 = type_checker(ctxt, e3, loc_ctxt, out, expected_typ).1;

                if let Some(typ) = biggest_compatible(&expr2.typed, &expr3.typed) {
                    (false, typ, typed_rust::ExprInner::If(expr1, expr2, expr3))
                } else {
                    panic!("incompatible types")
                }
            },

            rust::ExprInner::BuildStruct(name, args) => {
                if let Some(mut struct_info) = ctxt.struct_infos(name.get_content()) {
                    let mut args2 = Vec::new();
                    for (name, expr) in args.into_iter() {
                        if let Some(typ) = struct_info.get_typ(name.get_content()) {
                            let expr = type_checker(ctxt, expr, loc_ctxt, out, Some(typ)).1;
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
                            (false,
                            ctxt.get_typ(name.get_content()).unwrap().clone(),
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
                let expected_typ_vec = match expected_typ {
                    None => None,
                    Some(typ) => {
                        match &typ.content {
                            typed_rust::PostTypeInner::Tuple(expected_typ_vec) => {
                                if expected_typ_vec.len() == vec1.len() {
                                    Some(expected_typ_vec)
                                } else {
                                    todo!()
                                }
                            },
                            _ => todo!(),
                        }
                    }
                };

                for (i, expr) in vec1.into_iter().enumerate() {
                    let expr = type_checker(ctxt, expr, loc_ctxt, out, expected_typ_vec.map(|v| &v[i])).1;
                    size += expr.typed.size;
                    vec_type.push(expr.typed.clone());
                    vec_expr.push(expr);
                }
                (false,
                typed_rust::PostType {
                    content : typed_rust::PostTypeInner::Tuple(vec_type),
                    size
                },
                typed_rust::ExprInner::Tuple(vec_expr))
            },

            rust::ExprInner::Proj(expr, Projector::Int(id)) => {
                todo!()
            },

            rust::ExprInner::Proj(expr, Projector::Name(name)) => {
                let (affectable, expr) = type_checker(ctxt, expr, loc_ctxt, out, None);
                match &expr.typed.content {
                    typed_rust::PostTypeInner::Struct(s) => {
                        match ctxt.get_struct(&s) {
                            None => panic!("should not happend"),
                            Some(struct_info) => 
                                if let Some(typ) = struct_info.get_field_typ(name.get_content()) {
                                    (affectable, typ.clone(), typed_rust::ExprInner::Proj(expr, Projector::Name(name)))
                                } else {
                                    todo!()
                                },
                        }
                    },
                    typed_rust::PostTypeInner::Ref(affectable, typ) => {
                        match &typ.content {
                            typed_rust::PostTypeInner::Struct(s) => {
                                match ctxt.get_struct(&s) {
                                    None => panic!("should not happend"),
                                    Some(struct_info) => 
                                        if let Some(typ) = struct_info.get_field_typ(name.get_content()) {
                                            (*affectable, typ.clone(), typed_rust::ExprInner::Proj(expr, Projector::Name(name)))
                                        } else {
                                            todo!()
                                        },
                                }
                            },
                            _ => todo!()
                        }
                    },
                    _ => todo!(),
                }
            },

            rust::ExprInner::Parenthesis(expr) => {
                return type_checker(ctxt, expr, loc_ctxt, out, expected_typ);
            },

            rust::ExprInner::String(s) => {
                (false,
                    typed_rust::PostType::string(),
                    typed_rust::ExprInner::String(s))
            },

            rust::ExprInner::Array(a) => todo!(),

        };
    (affectable,
    typed_rust::Expr {
        content : Box::new(content),
        typed : compatible_types(&translated_typ, found_type),
        loc : expr.loc,
    })
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
                let expr = type_checker(ctxt, expr, loc_ctxt, output, None).1;
                content.push(typed_rust::Instr::Expr(expr));
//                todo!();
            },
            rust::Instr::Binding(mutable, ident, expr) => {
                let mut expr = type_checker(ctxt, expr, loc_ctxt, output, None).1;
                loc_ctxt.add_var(&ident, mutable, &expr.typed);
                content.push(typed_rust::Instr::Binding(mutable, ident, expr));
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
                let expr = type_checker(ctxt, expr, loc_ctxt, output, Some(output)).1;
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

    match loc_ctxt.pop_layer() {
        | Some(_) => (),
        | None => panic!("should not happen"),
    };

    typed_rust::Bloc{ content, last_type }
}
