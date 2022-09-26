use super::context;
use super::types::*;
use crate::ast::common::*;
use crate::ast::{rust, typed_rust};
use std::str::FromStr;

fn compatible_types(
    type1: &Option<typed_rust::PostType>,
    type2: typed_rust::PostType,
) -> typed_rust::PostType {
    match type1 {
        None => type2,
        Some(_type1) => todo! {},
    }
}

fn get_index_function(
    ctxt: &context::GlobalContext,
    typ: &typed_rust::PostType,
) -> Option<(&'static str, typed_rust::PostType)> {
    match &typ.content {
        typed_rust::PostTypeInner::Struct(id, param) if id == "Vec" => {
            Some(("get_element_vec", param[0].clone()))
        }
        typed_rust::PostTypeInner::Ref(_, typ) => match &typ.content {
            typed_rust::PostTypeInner::Struct(id, param) if id == "Vec" => {
                Some(("get_element_vec", param[0].clone()))
            }
            _ => None,
        },
        _ => None,
    }
}

fn get_binop_fun_name<'a>(
    ctxt: &'a context::GlobalContext,
    binop: BinOperator,
    typ1: &typed_rust::PostType,
    typ2: &typed_rust::PostType,
) -> Option<String> {
    let (name, fun_suffix) = binop.get_trait_name();
    let name = String::from(name);
    match ctxt.has_trait(
        typ1,
        &context::Trait::Parametrized(name, Some(typ2.clone())),
    ) {
        None => None,
        Some(s) => {
            println!("prefix {} for {:?} {:?}", s, binop, binop.get_trait_name());
            let mut s = s.to_string();
            s.push_str(fun_suffix);
            Some(s)
        }
    }
}

fn get_unaop_fun_name(
    ctxt: &context::GlobalContext,
    unaop: UnaOperator,
    typ1: &typed_rust::PostType,
) -> Option<String> {
    let (name, fun_suffix) = unaop.get_trait_name();
    let name = String::from(name);
    match ctxt.has_trait(typ1, &context::Trait::Name(name)) {
        None => None,
        Some(s) => {
            println!("prefix {} for {:?} {:?}", s, unaop, unaop.get_trait_name());
            let mut s = s.to_string();
            s.push_str(fun_suffix);
            Some(s)
        }
    }
}

fn arith_fun_name(binop: BinOperator) -> Option<&'static str> {
    match binop {
        BinOperator::Add => Some("_add"),
        BinOperator::Div => Some("_div"),
        BinOperator::Mod => Some("_mod"),
        BinOperator::Sub => Some("_sub"),
        BinOperator::Mul => Some("_mul"),
        _ => None,
    }
}

fn arith_cmp_fun(binop: BinOperator) -> Option<&'static str> {
    match binop {
        BinOperator::Lower => Some("_lower"),
        BinOperator::LowerEq => Some("_lower_eq"),
        BinOperator::Greater => Some("_greater"),
        BinOperator::GreaterEq => Some("_greater_eq"),
        _ => None,
    }
}

fn bool_fun_name(binop: BinOperator) -> Option<&'static str> {
    match binop {
        BinOperator::And => Some("_and"),
        BinOperator::Or => Some("_or"),
        _ => None,
    }
}

pub fn type_checker(
    ctxt: &context::GlobalContext,
    expr: rust::Expr,
    loc_ctxt: &mut context::LocalContext,
    out: &typed_rust::PostType,
    expected_typ: Option<&typed_rust::PostType>,
) -> (bool, typed_rust::Expr) {
    let mut translated_typ = expr.typed.map(|t| translate_typ(t, ctxt));
    let (affectable, found_type, content) = match *expr.content {
        rust::ExprInner::Bool(b) => (
            false,
            typed_rust::PostType::bool(),
            typed_rust::ExprInner::Bool(b),
        ),

        rust::ExprInner::Int(i) => {
            if is_type_int(&translated_typ) {
                let typ = translated_typ.unwrap();
                translated_typ = None;
                (false, typ, typed_rust::ExprInner::Int(i))
            } else {
                (
                    false,
                    typed_rust::PostType::i32(),
                    typed_rust::ExprInner::Int(i),
                )
            }
        }

        rust::ExprInner::Var(var_name) => {
            let (mutable, typ) = match loc_ctxt.get_typ(&var_name) {
                Some((mutable, typ)) => (*mutable, typ.clone()),
                None => panic!(
                    "undefined variable {} at {:?}",
                    var_name.get_content(),
                    expr.loc
                ),
            };
            (mutable, typ, typed_rust::ExprInner::Var(var_name))
        }

        rust::ExprInner::Ref(b, expr) => {
            let expected_typ = match expected_typ {
                None => None,
                Some(typ) => match &typ.content {
                    typed_rust::PostTypeInner::Ref(b2, typ) if b || !b2 => Some(&**typ),
                    _ => todo!(),
                },
            };
            let (affectable, expr) = type_checker(ctxt, expr, loc_ctxt, out, expected_typ);
            println!("warning typing/expr.rs line 154");
            if b && !affectable {
                todo!()
            };

            (
                false,
                Box::new(expr.typed.clone()).to_ref(b),
                typed_rust::ExprInner::Ref(b, expr),
            )
        }

        rust::ExprInner::Index(e1, e2) => {
            let (affectable, e1) = type_checker(ctxt, e1, loc_ctxt, out, None);
            let e2 = type_checker(ctxt, e2, loc_ctxt, out, None).1;
            if let Some((index_function, return_type)) = get_index_function(ctxt, &e1.typed) {
                if typed_rust::PostTypeInner::BuiltIn(BuiltinType::Int(true, Sizes::S32))
                    == e2.typed.content
                {
                    (
                        affectable || e1.typed.is_mut_ref(),
                        return_type,
                        typed_rust::ExprInner::FunCall(
                            Ident::from_str(index_function).unwrap(),
                            vec![e1, e2],
                        ),
                    )
                } else {
                    todo!()
                }
            } else {
                println!("didn't manage in get index in {:?}[{:?}]", e1, e2);
                todo!()
            }
        }

        rust::ExprInner::BinaryOp(BinOperator::Set, e1, e2) => {
            let (affectable, e1) = type_checker(ctxt, e1, loc_ctxt, out, None);
            let e2 = type_checker(ctxt, e2, loc_ctxt, out, None).1;
            if affectable && are_compatible(&e1.typed, &e2.typed) {
                (
                    false,
                    typed_rust::PostType::unit(),
                    typed_rust::ExprInner::Set(e1, e2),
                )
            } else {
                println!("not compatible for set {:?} {:?}", e1.typed, e2.typed);
                todo!()
            }
        }

        rust::ExprInner::BinaryOp(binop, e1, e2) => {
            let e1 = type_checker(ctxt, e1, loc_ctxt, out, None).1;
            let e2 = type_checker(ctxt, e2, loc_ctxt, out, None).1;
            if let Some(fun_name) = get_binop_fun_name(ctxt, binop, &e1.typed, &e2.typed) {
                if let Some(fun_typ) = ctxt.get_typ(&fun_name) {
                    (
                        false,
                        fun_typ.fun_out_typ().unwrap().clone(),
                        typed_rust::ExprInner::FunCall(
                            Ident::from_str(&fun_name).unwrap(),
                            vec![e1, e2],
                        ),
                    )
                } else {
                    panic!("should not happen {}", fun_name);
                }
            } else {
                todo!()
            }
        }

        rust::ExprInner::UnaryOp(unaop, e1) => {
            let e1 = type_checker(ctxt, e1, loc_ctxt, out, None).1;
            if let Some(fun_name) = get_unaop_fun_name(ctxt, unaop, &e1.typed) {
                if let Some(fun_typ) = ctxt.get_typ(&fun_name) {
                    (
                        false,
                        fun_typ.fun_out_typ().unwrap().clone(),
                        typed_rust::ExprInner::FunCall(
                            Ident::from_str(&fun_name).unwrap(),
                            vec![e1],
                        ),
                    )
                } else {
                    panic!("should not happen {}", fun_name);
                }
            } else {
                println!("{:?} {:?}", unaop, e1);
                todo!()
            }
        }

        rust::ExprInner::FunCall(var_name, args) => {
            let typ = match loc_ctxt.get_typ(&var_name) {
                Some((_, typ)) => typ.clone(),
                None => match ctxt.get_typ(var_name.get_content()) {
                    Some(typ) => typ.clone(),
                    None => todo!(),
                },
            };
            if let typed_rust::PostTypeInner::Fun(freetypes, args_typ, output) = typ.content {
                if args.len() != args_typ.len() {
                    panic!("not allowed")
                }

                let mut args2 = Vec::new();
                for (expr, arg) in args.into_iter().zip(args_typ.iter()) {
                    let expr = type_checker(ctxt, expr, loc_ctxt, out, Some(arg)).1;
                    if are_compatible(arg, &expr.typed) {
                        args2.push(expr)
                    } else {
                        todo!()
                    };
                }
                (
                    false,
                    *output,
                    typed_rust::ExprInner::FunCall(var_name, args2),
                )
            } else {
                panic!("not allowed")
            }
        }

        rust::ExprInner::MacroCall(name, mut args) if name.get_content() == "print" => {
            if args.len() == 1 {
                match *args.pop().unwrap().content {
                    rust::ExprInner::String(s) => (
                        false,
                        typed_rust::PostType::unit(),
                        typed_rust::ExprInner::Print(s),
                    ),
                    _ => todo!(),
                }
            } else {
                todo!()
            }
        }

        rust::ExprInner::MacroCall(name, vec) if name.get_content() == "vec" => {
            let mut vec2 = Vec::new();
            let el_expected_type: Option<&typed_rust::PostType> = match expected_typ {
                None => None,
                Some(typ) => match &typ.content {
                    typed_rust::PostTypeInner::Struct(id, args)
                        if id == "Vec" && args.len() == 1 =>
                    {
                        Some(&args[0])
                    }
                    _ => todo!(),
                },
            };
            let mut typ = el_expected_type.cloned();
            for expr in vec.into_iter() {
                let expr = type_checker(ctxt, expr, loc_ctxt, out, el_expected_type).1;
                match &typ {
                    None => typ = Some(expr.typed.clone()),
                    Some(typ2) => {
                        if let Some(typ2) = biggest_compatible(typ2, &expr.typed) {
                            typ = Some(typ2);
                            vec2.push(expr)
                        } else {
                            todo!()
                        }
                    }
                }
            }
            (
                false,
                typed_rust::PostType {
                    content: typed_rust::PostTypeInner::Struct(
                        String::from("Vec"),
                        vec![typ.unwrap()],
                    ),
                },
                typed_rust::ExprInner::Vec(vec2),
            )
        }

        rust::ExprInner::MacroCall(_name, _vec) => {
            panic!("should not happen")
        }

        rust::ExprInner::Method(expr, name, args) => {
            let expr = type_checker(ctxt, expr, loc_ctxt, out, None);
            if name.get_content() == "len" && args.is_empty() {
                match &expr.1.typed.content {
                    typed_rust::PostTypeInner::Struct(id, args)
                        if id == "Vec" && args.len() == 1 =>
                    {
                        (
                            false,
                            ctxt.get_typ("i32").unwrap().clone(),
                            typed_rust::ExprInner::FunCall(
                                Ident::from_str("vec_len").unwrap(),
                                vec![expr.1.to_ref(false)],
                            ),
                        )
                    }
                    typed_rust::PostTypeInner::Ref(mutable, typ) => match &typ.content {
                        typed_rust::PostTypeInner::Struct(id, args)
                            if id == "Vec" && args.len() == 1 =>
                        {
                            (
                                false,
                                ctxt.get_typ("i32").unwrap().clone(),
                                typed_rust::ExprInner::FunCall(
                                    Ident::from_str("vec_len").unwrap(),
                                    vec![expr.1],
                                ),
                            )
                        }
                        _ => todo!(),
                    },
                    _ => {
                        todo!()
                    }
                }
            } else {
                todo!()
            }
        }

        rust::ExprInner::Bloc(bloc) => {
            let bloc = type_block(bloc, ctxt, loc_ctxt, out, expected_typ);
            (
                false,
                bloc.last_type.clone(),
                typed_rust::ExprInner::Bloc(bloc),
            )
        }

        rust::ExprInner::Deref(expr) => {
            let expr = type_checker(ctxt, expr, loc_ctxt, out, None).1;
            match &expr.typed.content {
                typed_rust::PostTypeInner::Box(typ) => {
                    (false, *typ.clone(), typed_rust::ExprInner::Deref(expr))
                }
                typed_rust::PostTypeInner::Ref(mutable, typ) => {
                    (*mutable, *typ.clone(), typed_rust::ExprInner::Deref(expr))
                }
                _ => {
                    println!("trying to deref {:?}", expr.typed);
                    todo!()
                }
            }
        }

        rust::ExprInner::If(e1, bloc1, bloc2) => {
            let expr1 =
                type_checker(ctxt, e1, loc_ctxt, out, Some(&typed_rust::PostType::bool())).1;
            if expr1.typed.content != typed_rust::PostTypeInner::BuiltIn(BuiltinType::Bool) {
                panic!("Type error")
            }
            let bloc1 = type_block(bloc1, ctxt, loc_ctxt, out, expected_typ);
            let bloc2 = type_block(bloc2, ctxt, loc_ctxt, out, expected_typ);

            if let Some(typ) = biggest_compatible(&bloc1.last_type, &bloc2.last_type) {
                (false, typ, typed_rust::ExprInner::If(expr1, bloc1, bloc2))
            } else {
                panic!("incompatible types")
            }
        }

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
                    Some(_name) => todo!(),
                    None => (
                        false,
                        ctxt.get_typ(name.get_content()).unwrap().clone(),
                        typed_rust::ExprInner::BuildStruct(name, args2),
                    ),
                }
            } else {
                todo!()
            }
        }

        rust::ExprInner::Tuple(vec1) => {
            let mut vec_expr = Vec::new();
            let mut vec_type = Vec::new();
            let expected_typ_vec = match expected_typ {
                None => None,
                Some(typ) => match &typ.content {
                    typed_rust::PostTypeInner::Tuple(expected_typ_vec) => {
                        if expected_typ_vec.len() == vec1.len() {
                            Some(expected_typ_vec)
                        } else {
                            todo!()
                        }
                    }
                    _ => todo!(),
                },
            };

            for (i, expr) in vec1.into_iter().enumerate() {
                let expr =
                    type_checker(ctxt, expr, loc_ctxt, out, expected_typ_vec.map(|v| &v[i])).1;
                vec_type.push(expr.typed.clone());
                vec_expr.push(expr);
            }
            (
                false,
                typed_rust::PostType {
                    content: typed_rust::PostTypeInner::Tuple(vec_type),
                },
                typed_rust::ExprInner::Tuple(vec_expr),
            )
        }

        rust::ExprInner::Proj(_expr, Projector::Int(_id)) => {
            todo!()
        }

        rust::ExprInner::Proj(expr, Projector::Name(name)) => {
            let (affectable, expr) = type_checker(ctxt, expr, loc_ctxt, out, None);
            match &expr.typed.content {
                typed_rust::PostTypeInner::Struct(s, vec) if vec.is_empty() => match ctxt.get_struct(s) {
                    None => panic!("should not happend"),
                    Some(struct_info) => {
                        if let Some(typ) = struct_info.get_field_typ(name.get_content()) {
                            (
                                affectable,
                                typ.clone(),
                                typed_rust::ExprInner::Proj(expr, Projector::Name(name)),
                            )
                        } else {
                            todo!()
                        }
                    }
                },
                typed_rust::PostTypeInner::Ref(affectable, typ) => match &typ.content {
                    typed_rust::PostTypeInner::Struct(s, vec) if vec.is_empty() => match ctxt.get_struct(s) {
                        None => panic!("should not happend"),
                        Some(struct_info) => {
                            if let Some(typ) = struct_info.get_field_typ(name.get_content()) {
                                (
                                    *affectable,
                                    typ.clone(),
                                    typed_rust::ExprInner::Proj(expr, Projector::Name(name)),
                                )
                            } else {
                                todo!()
                            }
                        }
                    },
                    _ => todo!(),
                },
                _ => todo!(),
            }
        }

        rust::ExprInner::Parenthesis(expr) => {
            return type_checker(ctxt, expr, loc_ctxt, out, expected_typ);
        }

        rust::ExprInner::String(s) => (
            false,
            typed_rust::PostType::string(),
            typed_rust::ExprInner::String(s),
        ),

        rust::ExprInner::Array(_a) => todo!(),
    };
    (
        affectable,
        typed_rust::Expr {
            content: Box::new(content),
            typed: compatible_types(&translated_typ, found_type),
            loc: expr.loc,
        },
    )
}

pub fn type_block(
    bloc: rust::Bloc,
    ctxt: &context::GlobalContext,
    loc_ctxt: &mut context::LocalContext,
    output: &typed_rust::PostType,
    expected_typ: Option<&typed_rust::PostType>,
) -> typed_rust::Bloc {
    loc_ctxt.add_layer();
    let mut content = Vec::new();
    let mut reachable = true;
    for instr in bloc.content {
        match instr {
            rust::Instr::Expr(b, expr) => {
                let expr = type_checker(ctxt, expr, loc_ctxt, output, None).1;
                content.push(typed_rust::Instr::Expr(b, expr));
                //                todo!();
            }
            rust::Instr::Binding(mutable, ident, expr) => {
                let expr = type_checker(ctxt, expr, loc_ctxt, output, None).1;
                loc_ctxt.add_var(&ident, mutable, &expr.typed);
                content.push(typed_rust::Instr::Binding(mutable, ident, expr));
                //                todo!();
            }
            rust::Instr::While(condition, bloc) => {
                let condition = type_checker(
                    ctxt,
                    condition,
                    loc_ctxt,
                    output,
                    Some(&typed_rust::PostType::bool()),
                )
                .1;
                if !are_compatible(&typed_rust::PostType::bool(), &condition.typed) {
                    todo!()
                };
                let bloc = type_block(bloc, ctxt, loc_ctxt, output, None);
                content.push(typed_rust::Instr::While(condition, bloc));
            }
            rust::Instr::Return(None) => match &output.content {
                typed_rust::PostTypeInner::Tuple(l) if l.is_empty() => {
                    content.push(typed_rust::Instr::Return(None));
                    reachable = false;
                }
                _ => todo!(),
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
    }

    let last_type = if reachable {
        match (content.pop(), expected_typ) {
            (None, None) => typed_rust::PostType::unit(),
            (None, Some(typ)) if typ.is_unit() => typed_rust::PostType::unit(),
            (None, _) => todo!(),
            (Some(instr), None) => {
                let typ = match &instr {
                    typed_rust::Instr::Expr(ComputedValue::Keep, e) => e.typed.clone(),
                    _ => typed_rust::PostType::unit(),
                };
                content.push(instr);
                typ
            }
            (Some(instr), Some(expected_typ)) => {
                let got_typ = match &instr {
                    typed_rust::Instr::Expr(ComputedValue::Keep, e) => e.typed.clone(),
                    _ => typed_rust::PostType::unit(),
                };
                content.push(instr);
                if !are_compatible(expected_typ, &got_typ) {
                    todo!()
                } else {
                    got_typ
                }
            }
        }
    } else {
        match (content.pop(), expected_typ) {
            (Some(instr), None) => content.push(instr),
            (Some(instr), Some(typ)) => {
                if let typed_rust::Instr::Expr(ComputedValue::Keep, expr) = &instr {
                    if !are_compatible(typ, &expr.typed) {
                        todo!()
                    }
                };
                content.push(instr)
            }
            _ => (),
        };
        typed_rust::PostType::diverge()
    };

    match loc_ctxt.pop_layer() {
        Some(_) => (),
        None => panic!("should not happen"),
    };

    typed_rust::Bloc { content, last_type }
}

/*[Expr(Expr {
    content: If(
        Expr {
            content: FunCall(Ident { name: "i32_eq", loc: Location { start: 18446744073709551615, end: 18446744073709551615 } },
                [Expr {
                    content: Var(Ident { name: "i", loc: Location { start: 85, end: 86 } }),
                    loc: Location { start: 85, end: 86 },
                    typed: PostType { content: BuiltIn(Int(true, S32)), size: 4 } },
                Expr {
                    content: Int(0),
                    loc: Location { start: 90, end: 91 },
                    typed: PostType { content: BuiltIn(Int(true, S32)), size: 1 } }]),
            loc: Location { start: 85, end: 91 },
            typed: PostType { content: BuiltIn(Bool), size: 1 } },
        Expr {
            content: Bloc(Bloc {
                content: [],
                expr: Some(Expr {
                        content: Proj(
                            Expr {
                                content: Var(Ident { name: "l", loc: Location { start: 94, end: 95 } }),
                                loc: Location { start: 94, end: 95 },
                                typed: PostType { content: Ref(false, PostType { content: Struct("L"), size: 12 }), size: 8 } },
                            Name(Ident { name: "head", loc: Location { start: 96, end: 100 } })),
                        loc: Location { start: 94, end: 100 },
                        typed: PostType { content: BuiltIn(Int(true, S32)), size: 4 } }) }),
            loc: Location { start: 92, end: 102 },
            typed: PostType { content: BuiltIn(Int(true, S32)), size: 4 } },
        Expr {
            content: Bloc(Bloc {
                content: [],
                expr: Some(Expr {
                    content: FunCall(Ident { name: "get", loc: Location { start: 110, end: 113 } },
                        [Expr {
                            content: Ref(false,
                                Expr {
                                    content: FunCall(Ident { name: "get_element_vec", loc: Location { start: 18446744073709551615, end: 18446744073709551615 } },
                                        [Expr {
                                            content: Proj(
                                                Expr {
                                                    content: Var(Ident { name: "l", loc: Location { start: 115, end: 116 } }),
                                                    loc: Location { start: 115, end: 116 },
                                                    typed: PostType { content: Ref(false, PostType { content: Struct("L"), size: 12 }), size: 8 } },
                                                Name(Ident { name: "next", loc: Location { start: 117, end: 121 } })),
                                            loc: Location { start: 115, end: 121 },
                                            typed: PostType { content: IdentParametrized(Ident { name: "Vec", loc: Location { start: 18446744073709551615, end: 18446744073709551615 } }, [PostType { content: Struct("L"), size: 12 }]), size: 8 } },
                                        Expr {
                                            content: Int(0),
                                            loc: Location { start: 122, end: 123 },
                                            typed: PostType { content: BuiltIn(Int(true, S32)), size: 1 } }]),
                                    loc: Location { start: 115, end: 124 },
                                    typed: PostType { content: Struct("L"), size: 12 } }),
                            loc: Location { start: 114, end: 124 },
                            typed: PostType { content: Ref(false, PostType { content: Struct("L"), size: 12 }), size: 8 } },
                        Expr {
                            content: FunCall(Ident { name: "i32_sub", loc: Location { start: 18446744073709551615, end: 18446744073709551615 } },
                                [Expr {
                                    content: Var(Ident { name: "i", loc: Location { start: 126, end: 127 } }),
                                    loc: Location { start: 126, end: 127 },
                                    typed: PostType { content: BuiltIn(Int(true, S32)), size: 4 } },
                                Expr {
                                    content: Int(1),
                                    loc: Location { start: 128, end: 129 },
                                    typed: PostType { content: BuiltIn(Int(true, S32)), size: 1 } }]),
                            loc: Location { start: 126, end: 129 },
                            typed: PostType { content: BuiltIn(Int(true, S32)), size: 4 } }]),
                    loc: Location { start: 110, end: 130 },
                    typed: PostType { content: BuiltIn(Int(true, S32)), size: 4 } }) }),
                loc: Location { start: 108, end: 132 },
                typed: PostType { content: BuiltIn(Int(true, S32)), size: 4 } }),
            loc: Location { start: 82, end: 132 },
            typed: PostType { content: BuiltIn(Int(true, S32)), size: 4 } })]

*/
