use super::context::{self, LocalContext};
use super::errors::TypeError;
use super::types::*;
use crate::ast::common::*;
use crate::ast::typed_rust::{ExprInner, PostType, PostTypeInner};
use crate::ast::{rust, typed_rust};
use std::collections::HashMap;

fn compatible_types(
    type1: &Option<typed_rust::PostType>,
    type2: typed_rust::PostType,
) -> typed_rust::PostType {
    match type1 {
        None => type2,
        Some(_type1) => {
            return type2;
            todo! {}
        }
    }
}

fn get_index_function(
    ctxt: &context::GlobalContext,
    typ: &typed_rust::PostType,
) -> Option<(PathUL<()>, typed_rust::PostType)> {
    match &typ.content {
        PostTypeInner::Ref(_, typ) => match &typ.content {
            PostTypeInner::Struct(id, param)
                if id.get_content().len() == 3
                    && id.get_content()[0] == NamePath::Name("std".to_string())
                    && id.get_content()[1] == NamePath::Name("vec".to_string())
                    && id.get_content()[2] == NamePath::Name("Vec".to_string()) =>
            {
                Some((
                    PathUL::from_vec(vec!["std", "vec", "Vec", "get"]),
                    param[0].clone(),
                ))
            }
            _ => None,
        },
        _ => None,
    }
}

fn get_binop_fun_name(
    ctxt: &context::GlobalContext,
    binop: BinOperator,
    typ1: &typed_rust::PostType,
    typ2: &typed_rust::PostType,
    loc: Location,
) -> Result<PathUL<()>, Vec<TypeError>> {
    let (name, fun_suffix) = binop.get_trait_name();
    let name = String::from(name);
    let trait_name = context::Trait::Parametrized(name, None);
    if !are_compatible(typ1, typ2) {
        todo!()
    }
    match ctxt.has_trait(typ1, &trait_name) {
        None => Err(vec![TypeError::does_not_impl_trait(loc, typ1, trait_name)]),
        Some(mut path) => {
            path.push(NamePath::Name(fun_suffix.to_string()));
            Ok(path)
        }
    }
}

fn get_unaop_fun_name(
    ctxt: &context::GlobalContext,
    unaop: UnaOperator,
    typ1: &typed_rust::PostType,
    loc: Location,
) -> Result<PathUL<()>, Vec<TypeError>> {
    let (name, fun_suffix) = unaop.get_trait_name();
    let name = String::from(name);
    let trait_name = context::Trait::Name(name);
    match ctxt.has_trait(typ1, &trait_name) {
        None => Err(vec![TypeError::does_not_impl_trait(loc, typ1, trait_name)]),
        Some(mut path) => {
            path.push(NamePath::Name(fun_suffix.to_string()));
            Ok(path)
        }
    }
}

enum IsTypeBinop {
    BuiltIn(TypedBinop),
    Land,
    Lor,
    NotBuiltIn,
}

fn to_typed_binop(binop: BinOperator, typ: &PostTypeInner) -> IsTypeBinop {
    match typ {
        PostTypeInner::BuiltIn(BuiltinType::Bool) => match binop {
            BinOperator::Eq => IsTypeBinop::BuiltIn(TypedBinop::Eq(Sizes::S8)),
            BinOperator::Ne => IsTypeBinop::BuiltIn(TypedBinop::Eq(Sizes::S8)),

            BinOperator::And => IsTypeBinop::Land,
            BinOperator::Or => IsTypeBinop::Lor,

            BinOperator::Lower => IsTypeBinop::BuiltIn(TypedBinop::Lower(false, Sizes::S8)),
            BinOperator::LowerEq => IsTypeBinop::BuiltIn(TypedBinop::LowerEq(false, Sizes::S8)),
            BinOperator::Greater => IsTypeBinop::BuiltIn(TypedBinop::Greater(false, Sizes::S8)),
            BinOperator::GreaterEq => IsTypeBinop::BuiltIn(TypedBinop::GreaterEq(false, Sizes::S8)),

            BinOperator::Set => panic!("ICE"),
            _ => IsTypeBinop::NotBuiltIn,
        },
        PostTypeInner::BuiltIn(BuiltinType::Int(signed, size)) => match binop {
            BinOperator::Add => IsTypeBinop::BuiltIn(TypedBinop::Add(*size)),
            BinOperator::Sub => IsTypeBinop::BuiltIn(TypedBinop::Sub(*size)),
            BinOperator::Mod => IsTypeBinop::BuiltIn(TypedBinop::Mod(*signed, *size)),
            BinOperator::Mul => IsTypeBinop::BuiltIn(TypedBinop::Mul(*signed, *size)),
            BinOperator::Div => IsTypeBinop::BuiltIn(TypedBinop::Div(*signed, *size)),

            BinOperator::BitAnd => IsTypeBinop::BuiltIn(TypedBinop::And(*size)),
            BinOperator::BitOr => IsTypeBinop::BuiltIn(TypedBinop::Or(*size)),

            BinOperator::Shl => IsTypeBinop::BuiltIn(TypedBinop::Shl(*size)),
            BinOperator::Shr => IsTypeBinop::BuiltIn(TypedBinop::Shr(*size)),

            BinOperator::Eq => IsTypeBinop::BuiltIn(TypedBinop::Eq(*size)),
            BinOperator::Ne => IsTypeBinop::BuiltIn(TypedBinop::Neq(*size)),

            BinOperator::Lower => IsTypeBinop::BuiltIn(TypedBinop::Lower(*signed, *size)),
            BinOperator::LowerEq => IsTypeBinop::BuiltIn(TypedBinop::LowerEq(*signed, *size)),
            BinOperator::Greater => IsTypeBinop::BuiltIn(TypedBinop::Greater(*signed, *size)),
            BinOperator::GreaterEq => IsTypeBinop::BuiltIn(TypedBinop::GreaterEq(*signed, *size)),

            BinOperator::Set => panic!("ICE"),
            _ => IsTypeBinop::NotBuiltIn,
        },
        _ => IsTypeBinop::NotBuiltIn,
    }
}

fn to_typed_unaop(unaop: UnaOperator, typ: &PostTypeInner) -> Option<TypedUnaop> {
    match typ {
        PostTypeInner::BuiltIn(BuiltinType::Bool) => match unaop {
            UnaOperator::Neg => None,
            UnaOperator::Not => Some(TypedUnaop::Not(Sizes::S8)),
        },
        PostTypeInner::BuiltIn(BuiltinType::Int(_, size)) => match unaop {
            UnaOperator::Neg => Some(TypedUnaop::Neg(*size)),
            UnaOperator::Not => Some(TypedUnaop::Not(*size)),
        },
        _ => None,
    }
}

fn build_type(types: &rust::TypeStorage, type_id: usize) -> Option<typed_rust::PostType> {
    match types.get(type_id).unwrap() {
        rust::Types::SameAs(type_id) => build_type(types, *type_id),
        rust::Types::Array(_, _) => todo!(),
        rust::Types::Bool => Some(typed_rust::PostType::bool()),
        rust::Types::Deref(_) => None,
        rust::Types::Ref(None, _type_id) => todo!(),
        rust::Types::Ref(Some(mutable), type_id) => Some(typed_rust::PostType {
            content: PostTypeInner::Ref(*mutable, Box::new(build_type(types, *type_id)?)),
        }),
        rust::Types::Enum(path, args) => {
            let mut args2 = Vec::new();
            for typ in args.iter() {
                args2.push(build_type(types, *typ)?);
            }
            Some(typed_rust::PostType {
                content: PostTypeInner::Enum(path.clone(), args2),
            })
        }
        rust::Types::Fun(args, out) => {
            let out = build_type(types, *out)?;
            let args: Option<Vec<typed_rust::PostType>> =
                args.iter().map(|t| build_type(types, *t)).collect();
            Some(typed_rust::PostType {
                content: PostTypeInner::Fun(Vec::new(), args?, Box::new(out)),
            })
        }
        rust::Types::Int(signed, size) => Some(typed_rust::PostType {
            content: PostTypeInner::BuiltIn(BuiltinType::Int(
                signed.unwrap_or(true),
                size.unwrap_or(Sizes::S32),
            )),
        }),
        rust::Types::Struct(name, args) => {
            let mut args2 = Vec::new();
            for typ in args.iter() {
                args2.push(build_type(types, *typ)?);
            }
            Some(typed_rust::PostType {
                content: PostTypeInner::Struct(name.clone(), args2),
            })
        }
        rust::Types::Tuple(types_vec) => {
            let mut vec = Vec::new();
            for typ in types_vec.iter() {
                vec.push(build_type(types, *typ)?)
            }
            Some(typed_rust::PostType {
                content: PostTypeInner::Tuple(vec),
            })
        }
        rust::Types::Unknown => None,
    }
}

fn get_coercion_info(typ1: &PostTypeInner, typ2: &PostTypeInner) -> Option<BuiltinType> {
    match (typ1, typ2) {
        (PostTypeInner::BuiltIn(bi), PostTypeInner::BuiltIn(_)) => Some(*bi),
        (
            PostTypeInner::BuiltIn(BuiltinType::Int(false, Sizes::SUsize)),
            PostTypeInner::Ref(_, _),
        )
        | (
            PostTypeInner::Ref(_, _),
            PostTypeInner::BuiltIn(BuiltinType::Int(false, Sizes::SUsize)),
        ) => {
            panic!("Needs to handle unsafe")
        }
        _ => None,
    }
}

pub fn type_checker(
    ctxt: &context::GlobalContext,
    expr: rust::Expr<usize>,
    loc_ctxt: &mut context::LocalContext,
    out: &typed_rust::PostType,
    expected_typ: Option<&typed_rust::PostType>,
    typing_info: &rust::TypeStorage,
) -> Result<(bool, typed_rust::Expr), Vec<TypeError>> {
    let mut translated_typ = build_type(typing_info, expr.typed);
    let (affectable, found_type, content) = match *expr.content {
        rust::ExprInner::Bool(b) => (
            false,
            typed_rust::PostType::bool(),
            typed_rust::ExprInner::Bool(b),
        ),

        rust::ExprInner::Int(i, _) => {
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

        rust::ExprInner::Constructor(path, exprs) => {
            let cleaned_path = path.cleaned();
            let enum_info = match ctxt.is_constructor(&cleaned_path) {
                Some(enum_info) => enum_info.clone(),
                None => todo!(),
            };
            let typ = match translated_typ {
                None => todo!(),
                Some(typ) => {
                    translated_typ = None;
                    typ
                }
            };
            match &typ.content {
                PostTypeInner::Enum(type_path, freetypes) => {
                    assert_eq!(enum_info.get_free_types().len(), freetypes.len());
                    assert_eq!(path.get_content().len(), type_path.get_content().len() + 1);
                    let mut hashmap = HashMap::new();
                    for (name, typ) in enum_info.get_free_types().iter().zip(freetypes.into_iter())
                    {
                        hashmap.insert(name.to_string(), typ.clone());
                    }
                    let constructor = path.get_last().unwrap().get_content();
                    let needed_types = enum_info.get_constructor(constructor).unwrap().1;
                    assert_eq!(needed_types.len(), exprs.len());

                    let mut exprs2 = Vec::new();
                    for (expr, typ) in exprs.into_iter().zip(needed_types.iter()) {
                        let typ = substitute(typ.clone(), &hashmap);
                        let expr =
                            type_checker(ctxt, expr, loc_ctxt, out, Some(&typ), typing_info)?.1;
                        if are_compatible(&typ, &expr.typed) {
                            exprs2.push(expr)
                        } else {
                            todo!()
                        };
                    }
                    (
                        false,
                        typ,
                        typed_rust::ExprInner::Constructor(cleaned_path, exprs2),
                    )
                }
                _ => panic!("ICE"),
            }
        }
        rust::ExprInner::PatternMatching(expr, patterns, fall) => {
            let expr = type_checker(ctxt, expr, loc_ctxt, out, None, typing_info)?.1;
            let (reference, path, free) = match &expr.typed.content {
                PostTypeInner::Enum(path, free_types) => (None, path, free_types),
                PostTypeInner::Ref(mutable, typ) => match &typ.content {
                    PostTypeInner::Enum(path, free_types) => (Some(*mutable), path, free_types),
                    _ => todo!(),
                },
                _ => todo!(),
            };
            let mut enum_info = ctxt.enum_info(path).unwrap();
            let mut free_types = HashMap::new();
            let free_names = enum_info.get_free_types();
            assert_eq!(free_names.len(), free.len());
            for (name, typ) in free_names.iter().zip(free.iter()) {
                assert!(free_types.insert(name.to_string(), typ).is_none())
            }
            let mut rows = Vec::new();
            let row_types = match translated_typ {
                None => todo!(),
                Some(typ) => {
                    translated_typ = None;
                    typ
                }
            };
            for row in patterns {
                let mut constructor = row.constructor.cleaned();
                let constructor_name = match constructor.pop() {
                    None => panic!("ICE"),
                    Some(NamePath::Specialisation(_)) => panic!("Weird"),
                    Some(NamePath::Name(id)) => id,
                };
                let types = match row.guard {
                    None => enum_info.update_constructor(&constructor_name),
                    Some(_) => enum_info.get_constructor(&constructor_name),
                };
                let types = match types {
                    None => panic!("ICE should have been checked by inferencer"),
                    Some((_, types)) => types,
                };
                loc_ctxt.add_layer();
                assert_eq!(row.arguments.len(), types.len());
                for ((mutable, ident), typ) in row.arguments.iter().zip(types.iter()) {
                    let typ = match reference {
                        None => typ.clone(),
                        Some(b) => typed_rust::PostType {
                            content: PostTypeInner::Ref(b, Box::new(typ.clone())),
                        },
                    };
                    loc_ctxt.add_var2(ident.get_content().to_string(), *mutable, typ)
                }
                let bloc = type_bloc(row.bloc, ctxt, loc_ctxt, out, Some(&row_types), typing_info)?;
                let guard = match row.guard {
                    None => None,
                    Some(expr) => {
                        Some(type_checker(ctxt, expr, loc_ctxt, out, expected_typ, typing_info)?.1)
                    }
                };

                loc_ctxt.pop_layer();

                constructor.push(NamePath::Name(constructor_name));

                rows.push(typed_rust::Pattern {
                    constructor,
                    arguments: row.arguments,
                    bloc,
                    guard,
                })
            }

            let fall = match fall {
                None => match enum_info.check_finished() {
                    None => None,
                    Some(id) => todo!(),
                },
                Some((mutable, ident, bloc)) => {
                    loc_ctxt.add_layer();
                    loc_ctxt.add_var(&ident, mutable, &expr.typed);

                    let bloc = type_bloc(bloc, ctxt, loc_ctxt, out, Some(&row_types), typing_info)?;
                    loc_ctxt.pop_layer();
                    if !are_compatible(&row_types, &bloc.last_type) {
                        todo!()
                    };
                    Some((mutable, ident, bloc))
                }
            };

            (
                false,
                row_types,
                ExprInner::PatternMatching(expr, rows, fall),
            )
        }

        rust::ExprInner::Var(var_name) => match loc_ctxt.get_typ(&var_name) {
            Some((mutable, typ)) => (*mutable, typ.clone(), typed_rust::ExprInner::Var(var_name)),
            None => panic!("ICE"),
        },

        rust::ExprInner::VarPath(var_name) => {
            let cleaned_path = var_name.cleaned();
            match (ctxt.get_fun(&var_name), ctxt.get_const_val(&cleaned_path)) {
                (None, None) => panic!(
                    "ICE : undefined variable {:?} at {:?} should be cought by inferencer",
                    var_name, expr.loc
                ),
                (Some(typ), None) => (
                    false,
                    typ.clone(),
                    typed_rust::ExprInner::VarPath(cleaned_path),
                ),
                (None, Some(constant)) => {
                    let expr = constant.get_expr();
                    (false, expr.typed, *expr.content)
                }
                (Some(_), Some(_)) => todo!(),
            }
        }

        rust::ExprInner::Ref(b, expr) => {
            let expected_typ = match expected_typ {
                None => None,
                Some(typ) => match &typ.content {
                    PostTypeInner::Ref(b2, typ) if b || !b2 => Some(&**typ),
                    _ => {
                        println!("{b} {typ:?}");
                        todo!()
                    }
                },
            };
            let (affectable, expr) =
                type_checker(ctxt, expr, loc_ctxt, out, expected_typ, typing_info)?;
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
            let (affectable, e1) = type_checker(ctxt, e1, loc_ctxt, out, None, typing_info)?;
            let e1 = if e1.typed.is_ref() {
                e1
            } else {
                e1.to_ref(affectable)
            };
            let e2 = type_checker(ctxt, e2, loc_ctxt, out, None, typing_info)?.1;
            if let Some((index_function, return_type)) = get_index_function(ctxt, &e1.typed) {
                if PostTypeInner::BuiltIn(BuiltinType::Int(false, Sizes::SUsize))
                    == e2.typed.content
                {
                    let is_mut_ref = e1.typed.is_mut_ref();
                    (
                        affectable || is_mut_ref,
                        return_type.clone(),
                        typed_rust::ExprInner::Deref(typed_rust::Expr {
                            content: Box::new(typed_rust::ExprInner::FunCallPath(
                                index_function,
                                vec![e1, e2],
                            )),
                            loc: expr.loc,
                            typed: return_type.to_ref(is_mut_ref),
                        }),
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
            let (affectable, e1) = type_checker(ctxt, e1, loc_ctxt, out, None, typing_info)?;
            let e2 = type_checker(ctxt, e2, loc_ctxt, out, None, typing_info)?.1;
            if affectable && are_compatible(&e1.typed, &e2.typed) {
                let e1 = match *e1.content {
                    typed_rust::ExprInner::Deref(expr) => expr,
                    _ => typed_rust::Expr {
                        typed: e1.typed.clone().to_ref(true),
                        loc: e1.loc,
                        content: Box::new(typed_rust::ExprInner::Ref(true, e1)),
                    },
                };
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
            let e1 = type_checker(ctxt, e1, loc_ctxt, out, None, typing_info)?.1;
            let e2 = type_checker(ctxt, e2, loc_ctxt, out, None, typing_info)?.1;
            let fun_name_cleaned = get_binop_fun_name(ctxt, binop, &e1.typed, &e2.typed, e1.loc)?;
            let fun_name = fun_name_cleaned.add_loc();
            if let Some(fun_typ) = ctxt.get_fun(&fun_name) {
                match to_typed_binop(binop, &e1.typed.content) {
                    IsTypeBinop::Land => {
                        let expr_false = typed_rust::Expr {
                            content: Box::new(typed_rust::ExprInner::Bool(false)),
                            loc: Location::default(),
                            typed: typed_rust::PostType::bool(),
                        };
                        let bloc_false = typed_rust::Bloc {
                            content: vec![typed_rust::Instr::Expr(ComputedValue::Keep, expr_false)],
                            last_type: typed_rust::PostType::bool(),
                        };
                        let bloc_e2 = typed_rust::Bloc {
                            content: vec![typed_rust::Instr::Expr(ComputedValue::Keep, e2)],
                            last_type: typed_rust::PostType::bool(),
                        };
                        (
                            false,
                            fun_typ.fun_out_typ().unwrap().clone(),
                            typed_rust::ExprInner::If(e1, bloc_e2, bloc_false),
                        )
                    }
                    IsTypeBinop::Lor => {
                        let expr_true = typed_rust::Expr {
                            content: Box::new(typed_rust::ExprInner::Bool(true)),
                            loc: Location::default(),
                            typed: typed_rust::PostType::bool(),
                        };
                        let bloc_true = typed_rust::Bloc {
                            content: vec![typed_rust::Instr::Expr(ComputedValue::Keep, expr_true)],
                            last_type: typed_rust::PostType::bool(),
                        };
                        let bloc_e2 = typed_rust::Bloc {
                            content: vec![typed_rust::Instr::Expr(ComputedValue::Keep, e2)],
                            last_type: typed_rust::PostType::bool(),
                        };
                        (
                            false,
                            fun_typ.fun_out_typ().unwrap().clone(),
                            typed_rust::ExprInner::If(e1, bloc_true, bloc_e2),
                        )
                    }
                    IsTypeBinop::BuiltIn(bin) => (
                        false,
                        fun_typ.fun_out_typ().unwrap().clone(),
                        typed_rust::ExprInner::BinOp(bin, e1, e2),
                    ),
                    IsTypeBinop::NotBuiltIn => (
                        false,
                        fun_typ.fun_out_typ().unwrap().clone(),
                        typed_rust::ExprInner::FunCallPath(fun_name_cleaned, vec![e1, e2]),
                    ),
                }
            } else {
                panic!("should not happen {:?}", fun_name);
            }
        }

        rust::ExprInner::UnaryOp(unaop, e1) => {
            let e1 = type_checker(ctxt, e1, loc_ctxt, out, None, typing_info)?.1;
            let fun_name_cleaned = get_unaop_fun_name(ctxt, unaop, &e1.typed, e1.loc)?;
            let fun_name = fun_name_cleaned.add_loc();
            if let Some(fun_typ) = ctxt.get_fun(&fun_name) {
                match to_typed_unaop(unaop, &e1.typed.content) {
                    Some(una) => (
                        false,
                        fun_typ.fun_out_typ().unwrap().clone(),
                        typed_rust::ExprInner::UnaOp(una, e1),
                    ),
                    None => (
                        false,
                        fun_typ.fun_out_typ().unwrap().clone(),
                        typed_rust::ExprInner::FunCallPath(fun_name_cleaned, vec![e1]),
                    ),
                }
            } else {
                panic!("should not happen {:?}", fun_name);
            }
        }

        rust::ExprInner::FunCall(specialisation, var_name, args) => {
            let typ = match loc_ctxt.get_typ(&var_name) {
                Some((_, typ)) => typ.clone(),
                None => panic!("ICE"),
            };
            if let PostTypeInner::Fun(freetypes, args_typ, output) = typ.content {
                if args.len() != args_typ.len() {
                    panic!("{:?} not allowed", var_name.get_content())
                }
                assert_eq!(specialisation.len(), freetypes.len());
                let mut hashmap = HashMap::new();
                for (name, type_id) in freetypes.iter().zip(specialisation.into_iter()) {
                    hashmap.insert(name.to_string(), build_type(typing_info, type_id).unwrap());
                }

                let mut args2 = Vec::new();
                for (expr, arg) in args.into_iter().zip(args_typ.iter()) {
                    let arg = substitute(arg.clone(), &hashmap);
                    let expr = type_checker(ctxt, expr, loc_ctxt, out, Some(&arg), typing_info)?.1;
                    if are_compatible(&arg, &expr.typed) {
                        args2.push(expr)
                    } else {
                        todo!()
                    };
                }
                let output = substitute(*output, &hashmap);
                (
                    false,
                    output,
                    typed_rust::ExprInner::FunCall(var_name, args2),
                )
            } else {
                panic!("not allowed")
            }
        }

        rust::ExprInner::FunCallPath(specialisation, path, args) => {
            let typ = match ctxt.get_fun(&path) {
                Some(typ) => typ.clone(),
                None => todo!(),
            };
            if let PostTypeInner::Fun(freetypes, args_typ, output) = typ.content {
                if args.len() != args_typ.len() {
                    panic!("{:?} not allowed", path)
                }
                assert_eq!(specialisation.len(), freetypes.len());
                let mut hashmap = HashMap::new();
                for (name, type_id) in freetypes.iter().zip(specialisation.into_iter()) {
                    hashmap.insert(name.to_string(), build_type(typing_info, type_id).unwrap());
                }

                let mut args2 = Vec::new();
                for (expr, arg) in args.into_iter().zip(args_typ.iter()) {
                    let arg = substitute(arg.clone(), &hashmap);
                    let expr = type_checker(ctxt, expr, loc_ctxt, out, Some(&arg), typing_info)?.1;
                    if are_compatible(&arg, &expr.typed) {
                        args2.push(expr)
                    } else {
                        todo!()
                    };
                }
                let output = substitute(*output, &hashmap);
                (
                    false,
                    output,
                    typed_rust::ExprInner::FunCallPath(path.cleaned(), args2),
                )
            } else {
                panic!("not allowed")
            }
        }

        rust::ExprInner::MacroCall(name, mut args) if name.get_content() == "print_ptr" => {
            if args.len() == 1 {
                let expr = args.pop().unwrap();
                let expr = type_checker(ctxt, expr, loc_ctxt, out, None, typing_info)?.1;
                assert!(matches!(expr.typed.content, PostTypeInner::Ref(_, _)));
                (
                    false,
                    typed_rust::PostType::unit(),
                    typed_rust::ExprInner::PrintPtr(expr),
                )
            } else {
                todo!()
            }
        }

        rust::ExprInner::MacroCall(name, mut args) if name.get_content() == "print_usize" => {
            if args.len() == 1 {
                let expr = args.pop().unwrap();
                let expr = type_checker(ctxt, expr, loc_ctxt, out, None, typing_info)?.1;
                assert!(matches!(
                    expr.typed.content,
                    PostTypeInner::BuiltIn(BuiltinType::Int(false, Sizes::SUsize))
                ));
                (
                    false,
                    typed_rust::PostType::unit(),
                    typed_rust::ExprInner::PrintPtr(expr),
                )
            } else {
                todo!()
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

        rust::ExprInner::MacroCall(_, _) => {
            panic!("should not happen")
        }

        rust::ExprInner::Method(_expr, _name, _args) => {
            todo!()
        }

        rust::ExprInner::Bloc(bloc) => {
            let bloc = type_bloc(bloc, ctxt, loc_ctxt, out, expected_typ, typing_info)?;
            (
                false,
                bloc.last_type.clone(),
                typed_rust::ExprInner::Bloc(bloc),
            )
        }

        rust::ExprInner::Deref(expr) => {
            let expr = type_checker(ctxt, expr, loc_ctxt, out, None, typing_info)?.1;
            match &expr.typed.content {
                PostTypeInner::Box(typ) => {
                    (false, *typ.clone(), typed_rust::ExprInner::Deref(expr))
                }
                PostTypeInner::Ref(mutable, typ) => {
                    (*mutable, *typ.clone(), typed_rust::ExprInner::Deref(expr))
                }
                _ => {
                    println!("trying to deref {:?}", expr.typed);
                    todo!()
                }
            }
        }

        rust::ExprInner::If(e1, bloc1, bloc2) => {
            let expr1 = type_checker(
                ctxt,
                e1,
                loc_ctxt,
                out,
                Some(&typed_rust::PostType::bool()),
                typing_info,
            )?
            .1;
            if expr1.typed.content != PostTypeInner::BuiltIn(BuiltinType::Bool) {
                panic!("Type error")
            }
            let bloc1 = type_bloc(bloc1, ctxt, loc_ctxt, out, expected_typ, typing_info)?;
            let bloc2 = type_bloc(bloc2, ctxt, loc_ctxt, out, expected_typ, typing_info)?;

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
                        let expr =
                            type_checker(ctxt, expr, loc_ctxt, out, Some(typ), typing_info)?.1;
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
                        typed_rust::PostType {
                            content: typed_rust::PostTypeInner::Struct(
                                ctxt.get_path(name.get_content()),
                                vec![],
                            ),
                        },
                        typed_rust::ExprInner::BuildStruct(
                            ctxt.get_path(name.get_content()),
                            args2,
                        ),
                    ),
                }
            } else {
                todo!()
            }
        }

        rust::ExprInner::BuildStructPath(path, args) => {
            let cleaned_path = path.cleaned();
            if let Some(mut struct_info) = ctxt.struct_path(&cleaned_path) {
                let mut args2 = Vec::new();
                for (name, expr) in args.into_iter() {
                    if let Some(typ) = struct_info.get_typ(name.get_content()) {
                        let expr =
                            type_checker(ctxt, expr, loc_ctxt, out, Some(typ), typing_info)?.1;
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
                        typed_rust::PostType {
                            content: typed_rust::PostTypeInner::Struct(
                                cleaned_path.clone(),
                                vec![],
                            ),
                        },
                        typed_rust::ExprInner::BuildStruct(cleaned_path, args2),
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
                    PostTypeInner::Tuple(expected_typ_vec) => {
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
                let expr = type_checker(
                    ctxt,
                    expr,
                    loc_ctxt,
                    out,
                    expected_typ_vec.map(|v| &v[i]),
                    typing_info,
                )?
                .1;
                vec_type.push(expr.typed.clone());
                vec_expr.push(expr);
            }
            (
                false,
                typed_rust::PostType {
                    content: PostTypeInner::Tuple(vec_type),
                },
                typed_rust::ExprInner::Tuple(vec_expr, 0),
            )
        }

        rust::ExprInner::Proj(expr, Projector::Int(id)) => {
            let (affectable, expr) = type_checker(ctxt, expr, loc_ctxt, out, None, typing_info)?;
            match &expr.typed.content {
                PostTypeInner::Tuple(types) => {
                    if let Some(typ) = types.get(id) {
                        (
                            affectable,
                            typ.clone(),
                            typed_rust::ExprInner::Proj(expr, Projector::Int(id)),
                        )
                    } else {
                        todo!()
                    }
                }
                PostTypeInner::Ref(affectable, typ) => match &typ.content {
                    PostTypeInner::Tuple(types) => {
                        if let Some(typ) = types.get(id) {
                            (
                                *affectable,
                                typ.clone(),
                                typed_rust::ExprInner::Proj(expr, Projector::Int(id)),
                            )
                        } else {
                            return Err(vec![TypeError::out_of_bound_tuple(
                                expr.loc,
                                id,
                                types.len(),
                            )]);
                        }
                    }
                    _ => return Err(vec![TypeError::expected_tuple2((**typ).clone(), expr.loc)]),
                },
                _ => return Err(vec![TypeError::expected_tuple2(expr.typed, expr.loc)]),
            }
        }

        rust::ExprInner::Proj(expr, Projector::Name(name)) => {
            let (affectable, expr) = type_checker(ctxt, expr, loc_ctxt, out, None, typing_info)?;
            match &expr.typed.content {
                PostTypeInner::Struct(s, vec) if vec.is_empty() => match ctxt.get_struct_path(s) {
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
                PostTypeInner::Ref(affectable, typ) => match &typ.content {
                    PostTypeInner::Struct(s, vec) if vec.is_empty() => {
                        match ctxt.get_struct_path(s) {
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
                        }
                    }
                    _ => todo!(),
                },
                _ => todo!(),
            }
        }

        rust::ExprInner::Parenthesis(expr) => {
            return type_checker(ctxt, expr, loc_ctxt, out, expected_typ, typing_info);
        }

        rust::ExprInner::String(s) => (
            false,
            typed_rust::PostType::string(),
            typed_rust::ExprInner::String(s),
        ),

        rust::ExprInner::Array(_a) => todo!(),
        rust::ExprInner::Coercion(expr, typ) => {
            let (_, expr) = type_checker(ctxt, expr, loc_ctxt, out, None, typing_info)?;
            let type_out = build_type(typing_info, typ).unwrap();
            let typ_in = match get_coercion_info(&expr.typed.content, &type_out.content) {
                None => todo!(),
                Some(t) => t,
            };

            let typ_out = match get_coercion_info(&type_out.content, &expr.typed.content) {
                None => todo!(),
                Some(t) => t,
            };

            (
                false,
                type_out,
                typed_rust::ExprInner::Coercion(expr, typ_in, typ_out),
            )
        }

        rust::ExprInner::While(condition, bloc) => {
            let condition = type_checker(
                ctxt,
                condition,
                loc_ctxt,
                out,
                Some(&typed_rust::PostType::bool()),
                typing_info,
            )?
            .1;
            if !are_compatible(&typed_rust::PostType::bool(), &condition.typed) {
                todo!()
            };
            let bloc = type_bloc(bloc, ctxt, loc_ctxt, out, None, typing_info)?;
            (
                false,
                typed_rust::PostType::unit(),
                typed_rust::ExprInner::While(condition, bloc),
            )
        }
        rust::ExprInner::Return(None) => {
            let typ = translated_typ.unwrap_or(typed_rust::PostType::unit());
            translated_typ = None;
            match &out.content {
                PostTypeInner::Tuple(l) if l.is_empty() => {
                    (false, typ, typed_rust::ExprInner::Return(None))
                }
                _ => todo!(),
            }
        }
        rust::ExprInner::Return(Some(expr)) => {
            let typ = translated_typ.unwrap_or(typed_rust::PostType::unit());
            translated_typ = None;
            let expr = type_checker(ctxt, expr, loc_ctxt, out, Some(out), typing_info)?.1;
            if are_compatible(out, &expr.typed) {
                (false, typ, typed_rust::ExprInner::Return(Some(expr)))
            } else {
                todo!()
            }
        }
    };
    Ok((
        affectable,
        typed_rust::Expr {
            content: Box::new(content),
            typed: compatible_types(&translated_typ, found_type),
            loc: expr.loc,
        },
    ))
}

pub fn type_const(
    expr: rust::Expr<usize>,
    ctxt: &context::GlobalContext,
    expected_typ: &PostType,
    typing_info: &rust::TypeStorage,
) -> Result<typed_rust::Expr, Vec<TypeError>> {
    let out = PostType::unit();
    let mut loc_ctxt = LocalContext::new(&Vec::new());
    Ok(type_checker(
        ctxt,
        expr,
        &mut loc_ctxt,
        &out,
        Some(expected_typ),
        typing_info,
    )?
    .1)
}

pub fn type_bloc(
    bloc: rust::Bloc<usize>,
    ctxt: &context::GlobalContext,
    loc_ctxt: &mut context::LocalContext,
    output: &typed_rust::PostType,
    expected_typ: Option<&typed_rust::PostType>,
    typing_info: &rust::TypeStorage,
) -> Result<typed_rust::Bloc, Vec<TypeError>> {
    loc_ctxt.add_layer();
    let mut content = Vec::new();
    let len = bloc.content.len();
    for (id, instr) in bloc.content.into_iter().enumerate() {
        match instr.content {
            rust::InstrInner::Expr(b, expr) => {
                let expr = type_checker(ctxt, expr, loc_ctxt, output, None, typing_info)?.1;
                let b = if id + 1 == len {
                    b
                } else {
                    ComputedValue::Drop
                };
                content.push(typed_rust::Instr::Expr(b, expr));
            }
            rust::InstrInner::Binding(mutable, ident, type_id, expr) => {
                let typ = match build_type(typing_info, type_id) {
                    None => todo!(),
                    Some(t) => t,
                };
                let expr = type_checker(ctxt, expr, loc_ctxt, output, Some(&typ), typing_info)?.1;
                if !are_compatible(&typ, &expr.typed) {
                    todo!()
                };
                loc_ctxt.add_var(&ident, mutable, &expr.typed);
                content.push(typed_rust::Instr::Binding(mutable, ident, expr));
            }
        }
    }

    let last_type = match (content.pop(), expected_typ) {
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
    };

    match loc_ctxt.pop_layer() {
        Some(_) => (),
        None => panic!("should not happen"),
    };

    Ok(typed_rust::Bloc { content, last_type })
}
