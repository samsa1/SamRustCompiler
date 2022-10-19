use super::context::GlobalContext;
use super::errors::TypeError;
use crate::ast::common::{
    BinOperator, BuiltinType, ComputedValue, Ident, Location, Projector, Sizes, UnaOperator,
};
use crate::ast::rust::*;
use crate::ast::typed_rust::{PostType, PostTypeInner};
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct LocalContext {
    vars: Vec<HashMap<String, (bool, usize)>>,
}

impl LocalContext {
    pub fn new(in_types: Vec<(String, (bool, usize))>) -> Result<Self, String> {
        let mut in_types2 = HashMap::new();
        for (name, type_id) in in_types.into_iter() {
            if in_types2.contains_key(&name) {
                return Err(name);
            }
            assert!(in_types2.insert(name, type_id).is_none())
        }

        Ok(Self {
            vars: vec![in_types2],
        })
    }

    pub fn add_layer(&mut self) {
        self.vars.push(HashMap::new())
    }

    pub fn pop_layer(&mut self) -> Option<HashMap<String, (bool, usize)>> {
        self.vars.pop()
    }

    pub fn get_typ(&self, var_name: &Ident) -> Option<usize> {
        for hashmap in self.vars.iter().rev() {
            if let Some((_, type_id)) = hashmap.get(var_name.get_content()) {
                return Some(*type_id);
            }
        }
        None
    }

    pub fn is_mut(&self, var_name: &Ident) -> Option<bool> {
        for hashmap in self.vars.iter().rev() {
            if let Some((mutable, _)) = hashmap.get(var_name.get_content()) {
                return Some(*mutable);
            }
        }
        None
    }

    pub fn add_var(&mut self, ident: &Ident, mutable: bool, type_id: usize) {
        if let Some(last) = self.vars.last_mut() {
            last.insert(ident.get_content().to_string(), (mutable, type_id));
        } else {
            panic!("should never happend")
        }
    }
}

fn get_struct_name(
    types: &TypeStorage,
    type_id: usize,
    loc: Location,
    affectable: bool,
) -> Result<Option<(bool, &str, &Vec<usize>)>, Vec<TypeError>> {
    match types.get(type_id) {
        None => panic!("ICE"),
        Some(Types::Struct(name, params)) => Ok(Some((affectable, name, params))),
        Some(Types::Unknown) => Ok(None),
        Some(Types::Ref(mutable, type_id)) => {
            get_struct_name(types, *type_id, loc, mutable.unwrap_or(true))
        }
        Some(typ) => Err(vec![TypeError::expected_struct(typ.clone(), loc)]),
    }
}

fn get_tuple(
    types: &TypeStorage,
    type_id: usize,
    loc: Location,
    affectable: bool,
) -> Result<Option<(bool, &Vec<usize>)>, Vec<TypeError>> {
    match types.get(type_id) {
        None => panic!("ICE"),
        Some(Types::Tuple(vec)) => Ok(Some((affectable, vec))),
        Some(Types::Unknown) => Ok(None),
        Some(Types::Ref(mutable, type_id)) => {
            get_tuple(types, *type_id, loc, mutable.unwrap_or(true))
        }
        Some(typ) => Err(vec![TypeError::expected_tuple(typ.clone(), loc)]),
    }
}

fn check_coherence(
    types: &mut TypeStorage,
    type_id: usize,
    type_2: Option<PreType>,
    loc: Location,
) -> Result<(), Vec<TypeError>> {
    match type_2 {
        None => Ok(()),
        Some(_) => todo!(),
    }
}

#[derive(Copy, Clone, Debug)]
enum UnificationMethod {
    Smallest,
    StrictSnd,
    StrictFst,
    NoRef,
}

fn compute_mut(
    mut1: bool,
    mut2: bool,
    unif_method: UnificationMethod,
) -> Result<bool, Vec<TypeError>> {
    match unif_method {
        UnificationMethod::Smallest => Ok(mut1 && mut2),
        UnificationMethod::StrictFst if !mut1 || mut2 => Ok(mut1),
        UnificationMethod::StrictSnd if mut1 || !mut2 => Ok(mut2),
        UnificationMethod::NoRef => panic!("ICE"),
        _ => Err(vec![]),
    }
}

// result must be "<= min(type_id1, type_id2)"
// but we also have "if type_id_ != output { types.get(type_id_) = SameAs(output) }"
fn make_coherent(
    types: &mut TypeStorage,
    type_id1: usize,
    type_id2: usize,
    loc: Location,
    unification_method: UnificationMethod,
) -> Result<usize, Vec<TypeError>> {
    let (typ1, typ2) = match (types.get(type_id1), types.get(type_id2)) {
        (Some(typ1), Some(typ2)) => (typ1, typ2),
        _ => panic!("ICE"),
    };
    //    println!("-> {:?} {:?} {:?}", typ1, typ2, unification_method);
    match (typ1, typ2) {
        (Types::Unknown, _) => {
            let type_id = types.new_ref_unmarked(type_id2);
            if type_id1 != type_id {
                types.set(type_id1, Types::SameAs(type_id))
            };
            Ok(type_id2)
        }
        (_, Types::Unknown) => {
            // we know that type_id1 != type_id2
            let type_id = types.new_ref_unmarked(type_id1);
            types.set(type_id2, Types::SameAs(type_id));
            Ok(type_id)
        }

        (Types::SameAs(type_id1b), Types::SameAs(type_id2b)) => {
            make_coherent(types, *type_id1b, *type_id2b, loc, unification_method)
        }
        (_, Types::SameAs(type_id2b)) => {
            make_coherent(types, type_id1, *type_id2b, loc, unification_method)
        }
        (Types::SameAs(type_id1b), _) => {
            make_coherent(types, *type_id1b, type_id2, loc, unification_method)
        }

        (Types::Bool, Types::Bool) => Ok(type_id1),
        (Types::Bool, _) | (_, Types::Bool) => Err(vec![TypeError::not_compatible(
            loc,
            typ1.clone(),
            typ2.clone(),
        )]),

        (Types::Int(b1, s1), Types::Int(b2, s2)) => {
            let b = match (b1, b2) {
                (None, None) => None,
                (Some(b), None) | (None, Some(b)) => Some(*b),
                (Some(b1), Some(b2)) if b1 == b2 => Some(*b1),
                (Some(true), Some(false)) => return Err(vec![TypeError::expected_unsigned(loc)]),
                (Some(false), Some(true)) => return Err(vec![TypeError::expected_signed(loc)]),
                _ => panic!("Cannot happen"),
            };
            let s = match (s1, s2) {
                (None, None) => None,
                (Some(s), None) | (None, Some(s)) => Some(*s),
                (Some(s1), Some(s2)) if s1 == s2 => Some(*s1),
                (Some(s1), Some(s2)) => {
                    return Err(vec![TypeError::incompatible_sizes(*s1, *s2, loc)])
                }
            };
            types.set(type_id2, Types::SameAs(type_id1));
            types.set(type_id1, Types::Int(b, s));
            Ok(type_id1)
        }
        (Types::Int(_, _), _) | (_, Types::Int(_, _)) => Err(vec![TypeError::not_compatible(
            loc,
            typ1.clone(),
            typ2.clone(),
        )]),

        (Types::Tuple(vec1), Types::Tuple(vec2)) if vec1.len() == vec2.len() => {
            let mut vec = Vec::new();
            for (t1, t2) in vec1.clone().into_iter().zip(vec2.clone().into_iter()) {
                vec.push(make_coherent(types, t1, t2, loc, unification_method)?);
            }
            Ok(types.insert_type(Types::Tuple(vec)))
        }
        (Types::Tuple(_), _) | (_, Types::Tuple(_)) => Err(vec![TypeError::not_compatible(
            loc,
            typ1.clone(),
            typ2.clone(),
        )]),

        (Types::Ref(mut1, type_id1b), Types::Ref(mut2, type_id2b)) => {
            match (mut1, mut2, unification_method) {
                (_, _, UnificationMethod::NoRef) => panic!("ICE"),
                (Some(mut2), Some(mut1), UnificationMethod::StrictFst)
                | (Some(mut1), Some(mut2), UnificationMethod::StrictSnd) => {
                    let mut2 = *mut2;
                    if !mut2 || *mut1 {
                        let type_id =
                            make_coherent(types, *type_id1b, *type_id2b, loc, unification_method)?;
                        Ok(types.insert_type(Types::refed(mut2, type_id)))
                    } else {
                        Err(vec![])
                    }
                }
                (Some(mut1), Some(mut2), UnificationMethod::Smallest) => {
                    let mutable = *mut1 && *mut2;
                    let type_id =
                        make_coherent(types, *type_id1b, *type_id2b, loc, unification_method)?;
                    Ok(types.insert_type(Types::refed(mutable, type_id)))
                }
                (Some(true), None, UnificationMethod::Smallest)
                | (Some(true), None, UnificationMethod::StrictSnd)
                | (None, Some(true), UnificationMethod::Smallest)
                | (None, Some(true), UnificationMethod::StrictFst) => {
                    let type_id =
                        make_coherent(types, *type_id1b, *type_id2b, loc, unification_method)?;
                    Ok(types.insert_type(Types::Ref(None, type_id)))
                }
                (Some(true), None, UnificationMethod::StrictFst)
                | (None, Some(true), UnificationMethod::StrictSnd) => {
                    let type_id1b = *type_id1b;
                    let type_id2b = *type_id2b;
                    let type_id =
                        make_coherent(types, type_id1b, type_id2b, loc, unification_method)?;
                    types.set(type_id1, Types::refed(true, type_id1b));
                    types.set(type_id2, Types::refed(true, type_id2b));
                    Ok(types.insert_type(Types::refed(true, type_id)))
                }

                (None, None, _) => {
                    let type_id =
                        make_coherent(types, *type_id1b, *type_id2b, loc, unification_method)?;
                    Ok(types.insert_type(Types::Ref(None, type_id)))
                }
                (None, Some(false), UnificationMethod::Smallest)
                | (Some(false), None, UnificationMethod::Smallest) => {
                    let type_id1b = *type_id1b;
                    let type_id2b = *type_id2b;
                    let type_id =
                        make_coherent(types, type_id1b, type_id2b, loc, unification_method)?;
                    Ok(types.insert_type(Types::refed(false, type_id)))
                }
                (None, Some(false), UnificationMethod::StrictFst)
                | (Some(false), None, UnificationMethod::StrictFst)
                | (None, Some(false), UnificationMethod::StrictSnd)
                | (Some(false), None, UnificationMethod::StrictSnd) => {
                    let type_id1b = *type_id1b;
                    let type_id2b = *type_id2b;
                    let type_id =
                        make_coherent(types, type_id1b, type_id2b, loc, unification_method)?;
                    //                    println!("{types:?}");
                    types.set(type_id1, Types::refed(false, type_id1b));
                    types.set(type_id2, Types::refed(false, type_id2b));
                    //                    println!("{types:?}");
                    Ok(types.insert_type(Types::refed(false, type_id)))
                } //                _ => todo!(),
            }
        }
        (Types::Ref(_, _), _) | (_, Types::Ref(_, _)) => {
            return Err(vec![TypeError::not_compatible(
                loc,
                typ1.clone(),
                typ2.clone(),
            )])
        }

        (Types::Struct(name1, vec1), Types::Struct(name2, vec2)) if vec1.len() == vec2.len() => {
            if name1 == name2 {
                let mut type_vec = Vec::new();
                let name = name1.clone();
                for (typ1, typ2) in vec1.clone().into_iter().zip(vec2.clone().into_iter()) {
                    type_vec.push(make_coherent(types, typ1, typ2, loc, unification_method)?)
                }
                Ok(types.insert_type(Types::Struct(name, type_vec)))
            } else {
                Err(vec![TypeError::not_compatible(
                    loc,
                    typ1.clone(),
                    typ2.clone(),
                )])
            }
        }
        (Types::Struct(_, _), _) | (_, Types::Struct(_, _)) => {
            Err(vec![TypeError::not_compatible(
                loc,
                typ1.clone(),
                typ2.clone(),
            )])
        }

        (t1, t2) => {
            println!("not implemented for {t1:?} {t2:?}");
            todo!()
        }
    }
}

fn try_deref(
    types: &mut TypeStorage,
    type_id: usize,
    loc: Location,
) -> Result<(bool, usize), Vec<TypeError>> {
    match types.get(type_id) {
        None => panic!("ICE"),
        Some(Types::SameAs(type_id)) => try_deref(types, *type_id, loc),
        Some(typ) => match typ.unref() {
            None => Err(vec![TypeError::cannot_unref(loc, typ.clone())]),
            Some((bool, type_id)) => Ok((bool, type_id)),
        },
    }
}

// Add type
fn add_type(
    types: &mut TypeStorage,
    post_type: &PostType,
    free_types: &HashMap<String, usize>,
) -> usize {
    match &post_type.content {
        PostTypeInner::Box(typ) => {
            let type_id = add_type(types, typ, free_types);
            types.insert_type(Types::boxed(type_id))
        }
        PostTypeInner::BuiltIn(BuiltinType::Bool) => types.insert_bool(),
        PostTypeInner::BuiltIn(BuiltinType::Int(signed, size)) => {
            types.insert_type(Types::Int(Some(*signed), Some(*size)))
        }
        PostTypeInner::Diverge => todo!(),
        //        PostTypeInner::Enum(_) => todo!(),
        PostTypeInner::Fun(free, type_vec, out_type) => {
            let mut free_types2 = free_types.clone();
            for name in free.iter() {
                free_types2.insert(name.to_string(), types.insert_type(Types::unknown()));
            }
            let type_vec = type_vec
                .iter()
                .map(|t| add_type(types, t, &free_types2))
                .collect();
            let out_type = add_type(types, out_type, &free_types2);
            types.insert_type(Types::Fun(type_vec, out_type))
        }
        PostTypeInner::Struct(param, args) => {
            let mut vec = Vec::new();
            for typ in args.iter() {
                vec.push(add_type(types, typ, free_types))
            }
            types.insert_type(Types::Struct(param.to_string(), vec))
        }
        PostTypeInner::Ref(mutable, typ) => {
            let type_id = add_type(types, typ, free_types);
            types.insert_type(Types::refed(*mutable, type_id))
        }
        PostTypeInner::String => types.insert_string(),
        PostTypeInner::Tuple(type_vec) => {
            let type_vec = type_vec
                .iter()
                .map(|t| add_type(types, t, free_types))
                .collect();
            types.insert_type(Types::Tuple(type_vec))
        }
        PostTypeInner::FreeType(name) => *free_types.get(name).unwrap(),
    }
}

fn is_ref(typ: &PostType) -> Option<bool> {
    match &typ.content {
        PostTypeInner::Ref(b, _) => Some(*b),
        _ => None,
    }
}

fn is_ref_typ(types: &TypeStorage, type_id: usize) -> Option<&Option<bool>> {
    match types.get(type_id).unwrap() {
        Types::SameAs(type_id) => is_ref_typ(types, *type_id),
        Types::Ref(opt, _) => Some(opt),
        _ => None,
    }
}

// Used in function call and in building structures
fn forces_to(
    types: &mut TypeStorage,
    type_id: usize,
    typ: &PostType,
    loc: Location,
    free_types: &HashMap<String, usize>,
    unification_method: UnificationMethod,
) -> Result<(), Vec<TypeError>> {
    let type_id2 = add_type(types, typ, free_types);
    make_coherent(types, type_id, type_id2, loc, unification_method)?;
    Ok(())

    /*if let PostTypeInner::FreeType(name) = &typ.content {
        let id = free_types.get(name).unwrap();
        make_coherent(types, type_id, *id, loc)?;
        return Ok(())
    };
    match types.get(type_id).unwrap() {
        Types::SameAs(type_id2) => forces_to(types, *type_id2, typ, loc, free_types),
        Types::Unknown => {
            let new_type_id = add_type(types, typ, free_types);
            types.set(type_id, Types::SameAs(new_type_id));
            Ok(())
        },
        Types::Bool => {
            match &typ.content {
                PostTypeInner::BuiltIn(BuiltinType::Bool) => Ok(()),
                _ => Err(vec![TypeError::unknown_error(loc)]),
            }
        },
        Types::Int(b1, s1) => {
            match &typ.content {
                PostTypeInner::BuiltIn(BuiltinType::Int(b2, s2)) => {
                    if (b1.is_none() || b1 == &Some(*b2)) && (s1.is_none() || s1 == &Some(*s2)) {
                        types.set(type_id, Types::Int(Some(*b2), Some(*s2)));
                        Ok(())
                    } else {
                        todo!()
                    }
                },
                _ => {println!("got {typ:?} and expected int"); Err(vec![TypeError::unknown_error(loc)])},
            }
        }
        Types::Struct(name, args) => {
            match &typ.content {
                PostTypeInner::Struct(name2, args2) => {
                    if name == name2 {
                        for (typ1, typ2) in args.clone().into_iter().zip(args2.iter()) {
                            forces_to(types, typ1, typ2, loc, free_types)?;
                        };
                        Ok(())
                    } else {
                        todo!()
                    }
                },
                typ2 => {println!("got {typ2:?} and expected {typ:?}"); Err(vec![TypeError::unknown_error(loc)])},
            }
        },

        Types::Ref(type_id) => {
            match &typ.content {
                PostTypeInner::Ref(_, typ) => {
                    forces_to(types, *type_id, typ, loc, free_types)
                },
                _ => todo!()
            }
        }
        typ2 => {println!("{typ2:?} forced to {typ:?}"); todo!()},
    }*/
}

fn type_expr(
    ctxt: &GlobalContext,
    local_ctxt: &mut LocalContext,
    top_expr: Expr,
    types: &mut TypeStorage,
    out_type: usize,
) -> Result<(bool, Expr<usize>), Vec<TypeError>> {
    //    println!("processing {top_expr:?}");
    let out = match *top_expr.content {
        ExprInner::Array(_) => todo!(),

        ExprInner::BinaryOp(BinOperator::Set, expr1, expr2) => {
            let (affectable, expr1) = type_expr(ctxt, local_ctxt, expr1, types, out_type)?;
            if affectable {
                let expr2 = type_expr(ctxt, local_ctxt, expr2, types, out_type)?.1;
                make_coherent(
                    types,
                    expr1.typed,
                    expr2.typed,
                    expr1.loc,
                    UnificationMethod::StrictFst,
                )?;
                let type_id = types.insert_unit();
                check_coherence(types, type_id, top_expr.typed, top_expr.loc)?;
                Ok((
                    false,
                    Expr {
                        content: Box::new(ExprInner::BinaryOp(BinOperator::Set, expr1, expr2)),
                        typed: type_id,
                        loc: top_expr.loc,
                    },
                ))
            } else {
                Err(vec![TypeError::cannot_affect(expr1.loc)])
            }
        }

        ExprInner::BinaryOp(op, expr1, expr2) => {
            let expr1 = type_expr(ctxt, local_ctxt, expr1, types, out_type)?.1;
            let expr2 = type_expr(ctxt, local_ctxt, expr2, types, out_type)?.1;
            if op == BinOperator::And || op == BinOperator::Or {
                let type_id = types.insert_bool();
                make_coherent(
                    types,
                    expr1.typed,
                    type_id,
                    expr1.loc,
                    UnificationMethod::NoRef,
                )?;
                make_coherent(
                    types,
                    expr2.typed,
                    type_id,
                    expr2.loc,
                    UnificationMethod::NoRef,
                )?;
                check_coherence(types, type_id, top_expr.typed, top_expr.loc)?;
                Ok((
                    false,
                    Expr {
                        content: Box::new(ExprInner::BinaryOp(op, expr1, expr2)),
                        loc: top_expr.loc,
                        typed: type_id,
                    },
                ))
            } else if op == BinOperator::Eq
                || op == BinOperator::Ne
                || op == BinOperator::Greater
                || op == BinOperator::GreaterEq
                || op == BinOperator::Lower
                || op == BinOperator::LowerEq
            {
                make_coherent(
                    types,
                    expr1.typed,
                    expr2.typed,
                    top_expr.loc,
                    UnificationMethod::Smallest,
                )?;
                let type_id = types.insert_bool();
                check_coherence(types, type_id, top_expr.typed, top_expr.loc)?;
                Ok((
                    false,
                    Expr {
                        content: Box::new(ExprInner::BinaryOp(op, expr1, expr2)),
                        loc: top_expr.loc,
                        typed: type_id,
                    },
                ))
            } else if op == BinOperator::Add
                || op == BinOperator::Sub
                || op == BinOperator::Mod
                || op == BinOperator::Mul
                || op == BinOperator::Div
            {
                make_coherent(
                    types,
                    expr1.typed,
                    expr2.typed,
                    top_expr.loc,
                    UnificationMethod::NoRef,
                )?;
                let type_id = expr1.typed;
                /*                let type_id2 = types.insert_type(Types::Int(None, None));
                make_coherent(
                    types,
                    type_id,
                    type_id2,
                    top_expr.loc,
                    UnificationMethod::NoRef,
                )?;*/
                check_coherence(types, type_id, top_expr.typed, top_expr.loc)?;
                Ok((
                    false,
                    Expr {
                        typed: type_id,
                        content: Box::new(ExprInner::BinaryOp(op, expr1, expr2)),
                        loc: top_expr.loc,
                    },
                ))
            } else {
                panic!("ICE")
            }
        }

        ExprInner::Bloc(bloc) => {
            let (type_id, bloc) = type_bloc(ctxt, local_ctxt, bloc, types, out_type, None)?;
            check_coherence(types, type_id, top_expr.typed, top_expr.loc)?;
            Ok((
                false,
                Expr {
                    content: Box::new(ExprInner::Bloc(bloc)),
                    typed: type_id,
                    loc: top_expr.loc,
                },
            ))
        }

        ExprInner::Bool(b) => {
            let type_id = types.insert_bool();
            check_coherence(types, type_id, top_expr.typed, top_expr.loc)?;
            Ok((
                false,
                Expr {
                    content: Box::new(ExprInner::Bool(b)),
                    loc: top_expr.loc,
                    typed: type_id,
                },
            ))
        }

        ExprInner::BuildStruct(struct_name, args) => {
            if let Some(struct_info) = ctxt.get_struct(struct_name.get_content()) {
                let mut struct_info = struct_info.clone();
                let mut args2 = Vec::new();
                for (name, expr) in args.into_iter() {
                    let expr = type_expr(ctxt, local_ctxt, expr, types, out_type)?.1;
                    if let Some(typ) = struct_info.get_typ(name.get_content()) {
                        forces_to(
                            types,
                            expr.typed,
                            typ,
                            top_expr.loc,
                            &HashMap::new(),
                            UnificationMethod::StrictSnd,
                        )?;
                        args2.push((name, expr));
                    } else {
                        return Err(vec![TypeError::struct_no_field(
                            top_expr.loc,
                            struct_name.content(),
                            name.content(),
                        )]);
                    }
                }
                match struct_info.check_finished() {
                    Some(field_name) => {
                        return Err(vec![TypeError::missing_field(
                            top_expr.loc,
                            struct_name.content(),
                            field_name,
                        )])
                    }
                    None => (),
                };
                let type_id = types.insert_type(Types::struct_from_str(struct_name.get_content()));
                check_coherence(types, type_id, top_expr.typed, top_expr.loc)?;
                Ok((
                    false,
                    Expr {
                        content: Box::new(ExprInner::BuildStruct(struct_name, args2)),
                        loc: top_expr.loc,
                        typed: type_id,
                    },
                ))
            } else {
                Err(vec![TypeError::unknown_struct(struct_name)])
            }
        }

        ExprInner::Deref(expr) => {
            let expr = type_expr(ctxt, local_ctxt, expr, types, out_type)?.1;
            let (affectable, type_id) = try_deref(types, expr.typed, expr.loc)?;
            check_coherence(types, type_id, top_expr.typed, top_expr.loc)?;
            Ok((
                affectable,
                Expr {
                    content: Box::new(ExprInner::Deref(expr)),
                    loc: top_expr.loc,
                    typed: type_id,
                },
            ))
        }

        ExprInner::FunCall(args, name, exprs) => {
            assert!(args.is_empty());
            //            println!("Fun {name:?}");
            match local_ctxt.get_typ(&name) {
                None => {
                    if let Some(typ) = ctxt.get_typ(name.get_content()) {
                        match &typ.content {
                            PostTypeInner::Fun(free, args, out) if args.len() == exprs.len() => {
                                let mut free_types = HashMap::new();
                                let mut vec_types = Vec::new();
                                for name in free.iter() {
                                    let type_id = types.insert_type(Types::unknown());
                                    vec_types.push(type_id);
                                    free_types.insert(name.clone(), type_id);
                                }
                                let mut exprs_out = Vec::new();
                                for (expr, typ) in exprs.into_iter().zip(args.iter()) {
                                    let expr =
                                        type_expr(ctxt, local_ctxt, expr, types, out_type)?.1;
                                    forces_to(
                                        types,
                                        expr.typed,
                                        typ,
                                        expr.loc,
                                        &free_types,
                                        UnificationMethod::StrictSnd,
                                    )?;
                                    exprs_out.push(expr)
                                }
                                let type_id = add_type(types, out, &free_types);
                                check_coherence(types, type_id, top_expr.typed, top_expr.loc)?;
                                Ok((
                                    false,
                                    Expr {
                                        content: Box::new(ExprInner::FunCall(
                                            vec_types, name, exprs_out,
                                        )),
                                        loc: top_expr.loc,
                                        typed: type_id,
                                    },
                                ))
                            }
                            PostTypeInner::Fun(_, args, _) => Err(vec![TypeError::wrong_nb_args(
                                top_expr.loc,
                                exprs.len(),
                                args.len(),
                            )]),
                            typ => Err(vec![TypeError::expected_fun(name.get_loc(), typ.clone())]),
                        }
                    } else {
                        Err(vec![TypeError::unknown_var(name)])
                    }
                }
                Some(type_id) => match types.get(type_id).unwrap() {
                    Types::Unknown => todo!(),
                    Types::Fun(args, type_id) => {
                        todo!()
                    }
                    _ => todo!(),
                },
            }
        }

        ExprInner::If(expr, bloc1, bloc2) => {
            let expr = type_expr(ctxt, local_ctxt, expr, types, out_type)?.1;
            let (type_id1, bloc1) = type_bloc(ctxt, local_ctxt, bloc1, types, out_type, None)?;
            let (type_id2, bloc2) = type_bloc(ctxt, local_ctxt, bloc2, types, out_type, None)?;
            let type_id = types.insert_bool();
            make_coherent(
                types,
                expr.typed,
                type_id,
                expr.loc,
                UnificationMethod::NoRef,
            )?;
            let type_id = make_coherent(
                types,
                type_id1,
                type_id2,
                top_expr.loc,
                UnificationMethod::Smallest,
            )?;
            check_coherence(types, type_id, top_expr.typed, top_expr.loc)?;
            Ok((
                false,
                Expr {
                    content: Box::new(ExprInner::If(expr, bloc1, bloc2)),
                    loc: top_expr.loc,
                    typed: type_id,
                },
            ))
        }

        ExprInner::Index(expr_val, expr_index) => {
            let (affectable, expr_val) = type_expr(ctxt, local_ctxt, expr_val, types, out_type)?;
            let expr_index = type_expr(ctxt, local_ctxt, expr_index, types, out_type)?.1;
            let type_id = types.insert_usize();
            make_coherent(
                types,
                expr_index.typed,
                type_id,
                expr_index.loc,
                UnificationMethod::NoRef,
            )?;
            if let Some((affectable, name, args)) =
                get_struct_name(types, expr_val.typed, expr_val.loc, affectable)?
            {
                if name == "Vec" && args.len() == 1 {
                    let type_id = args[0];
                    // can be improved
                    check_coherence(types, type_id, top_expr.typed, top_expr.loc)?;
                    Ok((
                        affectable,
                        Expr {
                            content: Box::new(ExprInner::Index(expr_val, expr_index)),
                            loc: top_expr.loc,
                            typed: type_id,
                        },
                    ))
                } else {
                    todo!()
                }
            } else {
                todo!()
            }
        }

        ExprInner::Int(int, None) => {
            let type_id = types.insert_type(Types::int());
            check_coherence(types, type_id, top_expr.typed, top_expr.loc)?;
            Ok((
                false,
                Expr {
                    content: Box::new(ExprInner::Int(int, None)),
                    loc: top_expr.loc,
                    typed: type_id,
                },
            ))
        }

        ExprInner::Int(int, Some((signed, size))) => {
            let type_id = types.insert_type(Types::Int(Some(signed), Some(size)));
            check_coherence(types, type_id, top_expr.typed, top_expr.loc)?;
            Ok((
                false,
                Expr {
                    content: Box::new(ExprInner::Int(int, Some((signed, size)))),
                    loc: top_expr.loc,
                    typed: type_id,
                },
            ))
        }

        ExprInner::MacroCall(name, mut exprs) if name.get_content() == "print" => {
            if exprs.len() == 1 {
                let expr = type_expr(ctxt, local_ctxt, exprs.pop().unwrap(), types, out_type)?.1;
                let type_id = types.insert_string();
                make_coherent(
                    types,
                    expr.typed,
                    type_id,
                    expr.loc,
                    UnificationMethod::NoRef,
                )?;
                let type_id = types.insert_unit();
                check_coherence(types, type_id, top_expr.typed, top_expr.loc)?;
                Ok((
                    false,
                    Expr {
                        content: Box::new(ExprInner::MacroCall(name, vec![expr])),
                        typed: type_id,
                        loc: top_expr.loc,
                    },
                ))
            } else {
                todo!()
            }
        }

        ExprInner::MacroCall(name, mut exprs)
            if name.get_content() == "print_ptr" && exprs.len() == 1 =>
        {
            let expr = type_expr(ctxt, local_ctxt, exprs.pop().unwrap(), types, out_type)?.1;
            let type_id = types.insert_type(Types::unknown());
            let type_id = types.insert_type(Types::refed(false, type_id));
            make_coherent(
                types,
                type_id,
                expr.typed,
                top_expr.loc,
                UnificationMethod::Smallest,
            )?;
            let type_id = types.insert_unit();
            check_coherence(types, type_id, top_expr.typed, top_expr.loc)?;
            Ok((
                false,
                Expr {
                    content: Box::new(ExprInner::MacroCall(name, vec![expr])),
                    typed: type_id,
                    loc: top_expr.loc,
                },
            ))
        }

        ExprInner::MacroCall(_, _) => todo!(),

        ExprInner::Method(expr, method_name, exprs) => {
            let mut expr = type_expr(ctxt, local_ctxt, expr, types, out_type)?.1;
            if let Some((_, struct_name, params)) =
                get_struct_name(types, expr.typed, expr.loc, false)?
            {
                if let Some(method) = ctxt.get_method_function(struct_name, &method_name).unwrap() {
                    let typ = ctxt.get_typ(method).unwrap();
                    match &typ.content {
                        PostTypeInner::Fun(frees, args, out) => {
                            assert_eq!(frees.len(), params.len());
                            let params = params.clone();
                            let mut free_types = HashMap::new();
                            for (name, id) in frees.iter().zip(params.iter()) {
                                assert!(free_types.insert(name.to_string(), *id).is_none());
                            }
                            let mut args = args.iter();
                            let fst = args.next().unwrap();
                            if let Some(b) = is_ref(fst) {
                                if is_ref_typ(types, expr.typed).is_none() {
                                    expr = Expr {
                                        typed: types.insert_type(Types::refed(b, expr.typed)),
                                        loc: expr.loc,
                                        content: Box::new(ExprInner::Ref(b, expr)),
                                    }
                                }
                            };
                            let mut exprs_out = vec![expr];
                            for (expr, typ) in exprs.into_iter().zip(args) {
                                let expr = type_expr(ctxt, local_ctxt, expr, types, out_type)?.1;
                                forces_to(
                                    types,
                                    expr.typed,
                                    typ,
                                    expr.loc,
                                    &free_types,
                                    UnificationMethod::StrictSnd,
                                )?;
                                exprs_out.push(expr)
                            }
                            let type_id = add_type(types, out, &free_types);
                            check_coherence(types, type_id, top_expr.typed, top_expr.loc)?;
                            Ok((
                                false,
                                Expr {
                                    content: Box::new(ExprInner::FunCall(
                                        params,
                                        Ident::new(method, method_name.get_loc()),
                                        exprs_out,
                                    )),
                                    loc: top_expr.loc,
                                    typed: type_id,
                                },
                            ))
                        }
                        _ => todo!(),
                    }
                } else {
                    todo!()
                }
            } else {
                todo!()
            }
        }

        ExprInner::Parenthesis(expr) => {
            let expr = type_expr(ctxt, local_ctxt, expr, types, out_type)?.1;
            check_coherence(types, expr.typed, top_expr.typed, top_expr.loc)?;
            Ok((false, expr))
        }

        ExprInner::Proj(expr, Projector::Int(proj_id)) => {
            let (affectable, expr) = type_expr(ctxt, local_ctxt, expr, types, out_type)?;
            let (affectable, type_id) = if let Some((affectable, tuple)) =
                get_tuple(types, expr.typed, expr.loc, affectable)?
            {
                if proj_id >= tuple.len() {
                    return Err(vec![TypeError::unknown_error(top_expr.loc)]);
                } else {
                    (affectable, tuple[proj_id])
                }
            } else {
                (true, types.insert_type(Types::unknown()))
            };
            check_coherence(types, type_id, top_expr.typed, top_expr.loc)?;
            Ok((
                affectable,
                Expr {
                    content: Box::new(ExprInner::Proj(expr, Projector::Int(proj_id))),
                    loc: top_expr.loc,
                    typed: type_id,
                },
            ))
        }

        ExprInner::Proj(expr, Projector::Name(proj_name)) => {
            let (affectable, expr) = type_expr(ctxt, local_ctxt, expr, types, out_type)?;
            let (affectable, type_id) = if let Some((affectable, struct_name, params)) =
                get_struct_name(types, expr.typed, expr.loc, affectable)?
            {
                assert!(params.is_empty());
                let struct_info = ctxt.get_struct(struct_name).unwrap();
                if let Some(field_typ) = struct_info.get_field_typ(proj_name.get_content()) {
                    (affectable, add_type(types, field_typ, &HashMap::new()))
                } else {
                    return Err(vec![TypeError::struct_no_field(
                        top_expr.loc,
                        struct_name.to_string(),
                        proj_name.content(),
                    )]);
                }
            } else {
                (true, types.insert_type(Types::unknown()))
            };
            check_coherence(types, type_id, top_expr.typed, top_expr.loc)?;
            Ok((
                affectable,
                Expr {
                    content: Box::new(ExprInner::Proj(expr, Projector::Name(proj_name))),
                    loc: top_expr.loc,
                    typed: type_id,
                },
            ))
        }

        ExprInner::Ref(mutable, expr) => {
            let (affectable, expr) = type_expr(ctxt, local_ctxt, expr, types, out_type)?;
            if mutable && !affectable {
                return Err(vec![TypeError::cannot_borrow_as_mut(top_expr.loc)]);
            }
            let type_id = types.insert_type(Types::Ref(Some(mutable), expr.typed));
            check_coherence(types, type_id, top_expr.typed, top_expr.loc)?;
            Ok((
                false,
                Expr {
                    content: Box::new(ExprInner::Ref(mutable, expr)),
                    loc: top_expr.loc,
                    typed: type_id,
                },
            ))
        }

        ExprInner::String(string) => {
            let type_id = types.insert_string();
            check_coherence(types, type_id, top_expr.typed, top_expr.loc)?;
            Ok((
                false,
                Expr {
                    content: Box::new(ExprInner::String(string)),
                    loc: top_expr.loc,
                    typed: type_id,
                },
            ))
        }

        ExprInner::Tuple(exprs) => {
            let mut vec_exprs = Vec::new();
            let mut vec_types = Vec::new();
            for expr in exprs.into_iter() {
                let expr = type_expr(ctxt, local_ctxt, expr, types, out_type)?.1;
                vec_types.push(expr.typed);
                vec_exprs.push(expr);
            }
            let type_id = types.insert_type(Types::tuple(vec_types));
            check_coherence(types, type_id, top_expr.typed, top_expr.loc)?;
            Ok((
                false,
                Expr {
                    content: Box::new(ExprInner::Tuple(vec_exprs)),
                    loc: top_expr.loc,
                    typed: type_id,
                },
            ))
        }

        ExprInner::UnaryOp(UnaOperator::Not, expr) => {
            let expr = type_expr(ctxt, local_ctxt, expr, types, out_type)?.1;
            let type_id = types.insert_bool();
            make_coherent(
                types,
                expr.typed,
                type_id,
                expr.loc,
                UnificationMethod::NoRef,
            )?;
            check_coherence(types, type_id, top_expr.typed, top_expr.loc)?;
            Ok((
                false,
                Expr {
                    content: Box::new(ExprInner::UnaryOp(UnaOperator::Not, expr)),
                    loc: top_expr.loc,
                    typed: type_id,
                },
            ))
        }

        ExprInner::UnaryOp(UnaOperator::Neg, expr) => {
            let expr = type_expr(ctxt, local_ctxt, expr, types, out_type)?.1;
            let type_id = types.insert_type(Types::Int(Some(true), None));
            make_coherent(
                types,
                expr.typed,
                type_id,
                expr.loc,
                UnificationMethod::NoRef,
            )?;
            check_coherence(types, type_id, top_expr.typed, top_expr.loc)?;
            Ok((
                false,
                Expr {
                    content: Box::new(ExprInner::UnaryOp(UnaOperator::Neg, expr)),
                    loc: top_expr.loc,
                    typed: type_id,
                },
            ))
        }

        ExprInner::Var(var_name) => {
            if let Some(type_id) = local_ctxt.get_typ(&var_name) {
                check_coherence(types, type_id, top_expr.typed, top_expr.loc)?;
                Ok((
                    local_ctxt.is_mut(&var_name).unwrap(),
                    Expr {
                        content: Box::new(ExprInner::Var(var_name)),
                        loc: top_expr.loc,
                        typed: type_id,
                    },
                ))
            } else {
                Err(vec![TypeError::unknown_var(var_name)])
            }
        }
    };
    /*    println!("");
        println!("{:?}", types);
        println!("{:?}", out);
    */
    out
}

fn type_bloc(
    ctxt: &GlobalContext,
    local_ctxt: &mut LocalContext,
    bloc: Bloc,
    types: &mut TypeStorage,
    out_type: usize,
    expected: Option<usize>,
) -> Result<(usize, Bloc<usize>), Vec<TypeError>> {
    local_ctxt.add_layer();
    let mut content = Vec::new();
    for instr in bloc.content.into_iter() {
        match instr {
            Instr::Binding(mutable, name, expr) => {
                let expr = type_expr(ctxt, local_ctxt, expr, types, out_type)?.1;
                local_ctxt.add_var(&name, mutable, expr.typed);
                content.push(Instr::Binding(mutable, name, expr))
            }

            Instr::Expr(drop, expr) => {
                let expr = type_expr(ctxt, local_ctxt, expr, types, out_type)?.1;
                content.push(Instr::Expr(drop, expr))
            }
            Instr::Return(None) => {
                let type_id = types.insert_unit();
                make_coherent(
                    types,
                    type_id,
                    out_type,
                    Location::default(),
                    UnificationMethod::NoRef,
                )?;
                content.push(Instr::Return(None));
            }
            Instr::Return(Some(expr)) => {
                let expr = type_expr(ctxt, local_ctxt, expr, types, out_type)?.1;
                make_coherent(
                    types,
                    expr.typed,
                    out_type,
                    expr.loc,
                    UnificationMethod::StrictSnd,
                )?;
                content.push(Instr::Return(Some(expr)))
            }

            Instr::While(expr, bloc) => {
                let expr = type_expr(ctxt, local_ctxt, expr, types, out_type)?.1;
                let type_id = types.insert_bool();
                make_coherent(
                    types,
                    expr.typed,
                    type_id,
                    expr.loc,
                    UnificationMethod::NoRef,
                )?;
                let bloc = type_bloc(ctxt, local_ctxt, bloc, types, out_type, None)?.1;
                content.push(Instr::While(expr, bloc))
            }
        };
        /*        println!("");
                println!("{types:?}");
                println!("{local_ctxt:?}");
                println!("{content:?}");
        */
    }

    let type_id = match content.pop() {
        None => types.insert_unit(),
        Some(instr) => {
            let type_id = match expected {
                None => match &instr {
                    Instr::Expr(ComputedValue::Keep, expr) => expr.typed,
                    _ => types.insert_unit(),
                },
                Some(type_id) => match &instr {
                    Instr::Expr(ComputedValue::Keep, expr) => make_coherent(
                        types,
                        type_id,
                        expr.typed,
                        bloc.loc,
                        UnificationMethod::StrictFst,
                    )?,
                    Instr::Return(_) => types.insert_type(Types::unknown()),
                    _ => {
                        let type_id2 = types.insert_unit();
                        make_coherent(types, type_id, type_id2, bloc.loc, UnificationMethod::NoRef)?
                    }
                },
            };
            content.push(instr);
            type_id
        }
    };
    local_ctxt.pop_layer();

    Ok((
        type_id,
        Bloc {
            content,
            loc: bloc.loc,
        },
    ))
}

pub fn type_funs(
    name_fun: &Ident,
    ctxt: &GlobalContext,
    args: &[(String, bool, &PostType)],
    out_type: &PostType,
    body: Bloc,
) -> Result<(Bloc<usize>, TypeStorage), Vec<TypeError>> {
    let mut types = TypeStorage::new();
    let out_type = add_type(&mut types, out_type, &HashMap::new());
    let vec = args
        .iter()
        .map(|(name, mutable, post_type)| {
            (
                name.to_string(),
                (*mutable, add_type(&mut types, post_type, &HashMap::new())),
            )
        })
        .collect();
    let mut local_ctxt = match LocalContext::new(vec) {
        Ok(ctxt) => ctxt,
        Err(name) => return Err(vec![TypeError::same_arg_name(name_fun.clone(), name)]),
    };

    let (_, bloc) = type_bloc(
        ctxt,
        &mut local_ctxt,
        body,
        &mut types,
        out_type,
        Some(out_type),
    )?;
    Ok((bloc, types))
}
