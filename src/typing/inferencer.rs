use super::context::{Const, GlobalContext};
use super::errors::TypeError;
use super::types::translate_typ;
use crate::ast::common::{
    BinOperator, BuiltinType, ComputedValue, Ident, Location, NamePath, PathUL, Projector,
    UnaOperator,
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
) -> Result<Option<(bool, &PathUL<()>, &Vec<usize>)>, Vec<TypeError>> {
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

fn get_method_caller_name(
    types: &TypeStorage,
    type_id: usize,
    loc: Location,
    affectable: bool,
) -> Result<Option<(bool, &PathUL<()>, &Vec<usize>)>, Vec<TypeError>> {
    match types.get(type_id) {
        None => panic!("ICE"),
        Some(Types::Struct(name, params)) => Ok(Some((affectable, name, params))),
        Some(Types::Enum(name, params)) => Ok(Some((affectable, name, params))),
        Some(Types::Unknown) => Ok(None),
        Some(Types::Ref(mutable, type_id)) => {
            get_method_caller_name(types, *type_id, loc, mutable.unwrap_or(true))
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
        Some(Types::SameAs(type_id)) => get_tuple(types, *type_id, loc, affectable),
        Some(Types::Tuple(vec)) => Ok(Some((affectable, vec))),
        Some(Types::Unknown) => Ok(None),
        Some(Types::Ref(mutable, type_id)) => {
            get_tuple(types, *type_id, loc, mutable.unwrap_or(true))
        }
        Some(typ) => Err(vec![TypeError::expected_tuple(typ.clone(), loc)]),
    }
}

fn get_enum_inner(
    types: &TypeStorage,
    type_id: usize,
    loc: Location,
) -> Result<Option<(&PathUL<()>, &Vec<usize>)>, Vec<TypeError>> {
    match types.get(type_id) {
        None => panic!("ICE"),
        Some(Types::SameAs(type_id)) => get_enum_inner(types, *type_id, loc),
        Some(Types::Enum(path, vec)) => Ok(Some((path, vec))),
        Some(Types::Unknown) => Ok(None),
        Some(typ) => Err(vec![TypeError::expected_tuple(typ.clone(), loc)]),
    }
}

fn get_enum(
    types: &TypeStorage,
    type_id: usize,
    loc: Location,
) -> Result<Option<(Option<Option<bool>>, Option<(&PathUL<()>, &Vec<usize>)>)>, Vec<TypeError>> {
    match types.get(type_id) {
        None => panic!("ICE"),
        Some(Types::SameAs(type_id)) => get_enum(types, *type_id, loc),
        Some(Types::Enum(path, vec)) => Ok(Some((None, Some((path, vec))))),
        Some(Types::Unknown) => Ok(None),
        Some(Types::Ref(mutable, type_id)) => Ok(Some((
            Some(*mutable),
            get_enum_inner(types, *type_id, loc)?,
        ))),
        Some(typ) => Err(vec![TypeError::expected_tuple(typ.clone(), loc)]),
    }
}

fn check_coherence(
    types: &mut TypeStorage,
    type_id: usize,
    type_2: Option<PreType>,
    loc: Location,
    g_ctxt: &GlobalContext,
) -> Result<(), Vec<TypeError>> {
    match type_2 {
        None => Ok(()),
        Some(typ) => {
            if let Some(pt) = translate_typ(typ, g_ctxt, types.get_frees()) {
                forces_to(
                    types,
                    type_id,
                    &pt,
                    loc,
                    &HashMap::new(),
                    UnificationMethod::StrictSnd,
                )
            } else {
                todo!()
            }
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum UnificationMethod {
    Smallest,
    StrictEq,
    StrictSnd,
    StrictFst,
    NoRef,
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
                (Some(mut1), Some(mut2), UnificationMethod::StrictEq) => {
                    if *mut1 == *mut2 {
                        let mut2 = *mut2;
                        let type_id =
                            make_coherent(types, *type_id1b, *type_id2b, loc, unification_method)?;
                        Ok(types.insert_type(Types::refed(mut2, type_id)))
                    } else {
                        todo!()
                    }
                }
                (Some(mut2), Some(mut1), UnificationMethod::StrictFst)
                | (Some(mut1), Some(mut2), UnificationMethod::StrictSnd) => {
                    let mut2 = *mut2;
                    if !mut2 || *mut1 {
                        let type_id =
                            make_coherent(types, *type_id1b, *type_id2b, loc, unification_method)?;
                        Ok(types.insert_type(Types::refed(mut2, type_id)))
                    } else {
                        Err(vec![TypeError::wrong_mutability(loc, mut2, *mut1)])
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
                (Some(true), None, UnificationMethod::StrictEq)
                | (None, Some(true), UnificationMethod::StrictEq)
                | (Some(true), None, UnificationMethod::StrictFst)
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
                (Some(false), None, UnificationMethod::StrictEq)
                | (None, Some(false), UnificationMethod::StrictEq)
                | (None, Some(false), UnificationMethod::StrictFst)
                | (Some(false), None, UnificationMethod::StrictFst)
                | (None, Some(false), UnificationMethod::StrictSnd)
                | (Some(false), None, UnificationMethod::StrictSnd) => {
                    let type_id1b = *type_id1b;
                    let type_id2b = *type_id2b;
                    let type_id =
                        make_coherent(types, type_id1b, type_id2b, loc, unification_method)?;
                    types.set(type_id1, Types::refed(false, type_id1b));
                    types.set(type_id2, Types::refed(false, type_id2b));
                    Ok(types.insert_type(Types::refed(false, type_id)))
                }
            }
        }
        (Types::Ref(_, _), _) | (_, Types::Ref(_, _)) => Err(vec![TypeError::not_compatible(
            loc,
            typ1.clone(),
            typ2.clone(),
        )]),

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

        (Types::Enum(name1, vec1), Types::Enum(name2, vec2)) if vec1.len() == vec2.len() => {
            if name1 == name2 {
                let mut type_vec = Vec::new();
                let name = name1.clone();
                for (typ1, typ2) in vec1.clone().into_iter().zip(vec2.clone().into_iter()) {
                    type_vec.push(make_coherent(types, typ1, typ2, loc, unification_method)?)
                }
                Ok(types.insert_type(Types::Enum(name, type_vec)))
            } else {
                Err(vec![TypeError::not_compatible(
                    loc,
                    typ1.clone(),
                    typ2.clone(),
                )])
            }
        }
        (Types::Enum(_, _), _) | (_, Types::Enum(_, _)) => Err(vec![TypeError::not_compatible(
            loc,
            typ1.clone(),
            typ2.clone(),
        )]),

        (Types::Fun(args1, out1), Types::Fun(args2, out2)) if args1.len() == args2.len() => {
            let args1 = args1.clone();
            let args2 = args2.clone();
            let out = make_coherent(types, *out1, *out2, loc, UnificationMethod::StrictEq)?;
            let mut type_vec = Vec::new();
            for (type_id1, type_id2) in args1.into_iter().zip(args2.into_iter()) {
                type_vec.push(make_coherent(
                    types,
                    type_id1,
                    type_id2,
                    loc,
                    UnificationMethod::StrictEq,
                )?)
            }
            Ok(types.insert_type(Types::Fun(type_vec, out)))
        }

        (Types::Fun(_, _), _) | (_, Types::Fun(_, _)) => Err(vec![TypeError::not_compatible(
            loc,
            typ1.clone(),
            typ2.clone(),
        )]),

        (Types::Free(s1), Types::Free(s2)) if s1 == s2 => Ok(type_id1),
        (Types::Free(_), _) | (_, Types::Free(_)) => Err(vec![TypeError::not_compatible(
            loc,
            typ1.clone(),
            typ2.clone(),
        )]),

        (Types::Array(_, _), Types::Array(_, _)) => todo!(),

        (Types::Array(_, _), _) | (_, Types::Array(_, _)) => Err(vec![TypeError::not_compatible(
            loc,
            typ1.clone(),
            typ2.clone(),
        )]),
        (Types::Deref(_), Types::Deref(_)) => todo!(),
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
        PostTypeInner::Box(_) => todo!(),
        PostTypeInner::BuiltIn(BuiltinType::Bool) => types.insert_bool(),
        PostTypeInner::BuiltIn(BuiltinType::Int(signed, size)) => {
            types.insert_type(Types::Int(Some(*signed), Some(*size)))
        }
        PostTypeInner::Diverge => todo!(),
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
            types.insert_type(Types::Struct(param.clone(), vec))
        }
        PostTypeInner::Enum(param, args) => {
            let mut vec = Vec::new();
            for typ in args.iter() {
                vec.push(add_type(types, typ, free_types))
            }
            types.insert_type(Types::Enum(param.clone(), vec))
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
        PostTypeInner::FreeType(name) => match free_types.get(name) {
            Some(i) => *i,
            None => types.insert_type(Types::Free(name.to_string())),
        },
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
}

fn type_expr(
    ctxt: &GlobalContext,
    local_ctxt: &mut LocalContext,
    top_expr: Expr,
    types: &mut TypeStorage,
    out_type: usize,
) -> Result<(bool, Expr<usize>), Vec<TypeError>> {
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
                check_coherence(types, type_id, top_expr.typed, top_expr.loc, ctxt)?;
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
                check_coherence(types, type_id, top_expr.typed, top_expr.loc, ctxt)?;
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
                check_coherence(types, type_id, top_expr.typed, top_expr.loc, ctxt)?;
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
                || op == BinOperator::Shl
                || op == BinOperator::Shr
                || op == BinOperator::BitAnd
                || op == BinOperator::BitOr
            {
                make_coherent(
                    types,
                    expr1.typed,
                    expr2.typed,
                    top_expr.loc,
                    UnificationMethod::NoRef,
                )?;
                let type_id = expr1.typed;
                check_coherence(types, type_id, top_expr.typed, top_expr.loc, ctxt)?;
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
            check_coherence(types, type_id, top_expr.typed, top_expr.loc, ctxt)?;
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
            check_coherence(types, type_id, top_expr.typed, top_expr.loc, ctxt)?;
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
                            PathUL::new(vec![NamePath::Name(struct_name.content())]),
                            name.content(),
                        )]);
                    }
                }
                match struct_info.check_finished() {
                    Some(field_name) => {
                        return Err(vec![TypeError::missing_field(
                            top_expr.loc,
                            PathUL::new(vec![NamePath::Name(struct_name.content())]),
                            field_name,
                        )])
                    }
                    None => (),
                };
                let type_id = types.insert_type(Types::Struct(
                    ctxt.get_path(struct_name.get_content()),
                    vec![],
                ));
                check_coherence(types, type_id, top_expr.typed, top_expr.loc, ctxt)?;
                Ok((
                    false,
                    Expr {
                        content: Box::new(ExprInner::BuildStruct(struct_name, args2)),
                        loc: top_expr.loc,
                        typed: type_id,
                    },
                ))
            } else {
                Err(vec![TypeError::unknown_struct(
                    struct_name.get_loc(),
                    ctxt.get_path(struct_name.get_content()),
                )])
            }
        }

        ExprInner::BuildStructPath(struct_path, args) => {
            assert!(struct_path.get_content().len() > 1);
            let cleaned_path = struct_path.cleaned();
            if let Some(struct_info) = ctxt.get_struct_path(&cleaned_path) {
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
                            cleaned_path,
                            name.content(),
                        )]);
                    }
                }
                match struct_info.check_finished() {
                    Some(field_name) => {
                        return Err(vec![TypeError::missing_field(
                            top_expr.loc,
                            cleaned_path,
                            field_name,
                        )])
                    }
                    None => (),
                };
                let type_id = types.insert_type(Types::Struct(cleaned_path, vec![]));
                check_coherence(types, type_id, top_expr.typed, top_expr.loc, ctxt)?;
                Ok((
                    false,
                    Expr {
                        content: Box::new(ExprInner::BuildStructPath(struct_path, args2)),
                        loc: top_expr.loc,
                        typed: type_id,
                    },
                ))
            } else {
                Err(vec![TypeError::unknown_struct(
                    struct_path.get_loc(),
                    cleaned_path,
                )])
            }
        }

        ExprInner::Constructor(raw_path, exprs) => {
            // untested section because can never happen unless a new pass is implemented before typing
            let mut path = raw_path.cleaned();
            let constructor = path.pop();
            let enum_info = ctxt.get_enum(&path);
            let (enum_info, constructor) = match (enum_info, constructor) {
                (Some(ei), Some(NamePath::Name(c))) => (ei, c),
                (_, None) => panic!("ICE"),
                (_, Some(NamePath::Specialisation(_))) => panic!("Unhandled case"),
                (None, _) => todo!(),
            };
            let types_expected = match enum_info.get_constructor(&constructor) {
                None => todo!(),
                Some((_, types)) => types,
            };
            let mut free_types = HashMap::new();
            let mut vec_free_types = Vec::new();
            for name in enum_info.get_free_types().iter() {
                let type_id = types.insert_type(Types::unknown());
                free_types.insert(name.clone(), type_id);
                vec_free_types.push(type_id);
            }
            let mut exprs_out = Vec::new();
            for (expr, typ) in exprs.into_iter().zip(types_expected.iter()) {
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
            let type_id = types.insert_type(Types::Enum(path, vec_free_types));
            Ok((
                false,
                Expr {
                    content: Box::new(ExprInner::Constructor(raw_path, exprs_out)),
                    loc: top_expr.loc,
                    typed: type_id,
                },
            ))
        }

        ExprInner::Deref(expr) => {
            let expr = type_expr(ctxt, local_ctxt, expr, types, out_type)?.1;
            let (affectable, type_id) = try_deref(types, expr.typed, expr.loc)?;
            check_coherence(types, type_id, top_expr.typed, top_expr.loc, ctxt)?;
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
            match local_ctxt.get_typ(&name) {
                None => {
                    if let Some((free, args, out)) =
                        ctxt.get_top_fun(name.get_content()).map(|fi| fi.get_typ())
                    {
                        if args.len() == exprs.len() {
                            let mut free_types = HashMap::new();
                            let mut vec_types = Vec::new();
                            for name in free.iter() {
                                let type_id = types.insert_type(Types::unknown());
                                vec_types.push(type_id);
                                free_types.insert(name.clone(), type_id);
                            }
                            let mut exprs_out = Vec::new();
                            for (expr, typ) in exprs.into_iter().zip(args.iter()) {
                                let expr = type_expr(ctxt, local_ctxt, expr, types, out_type)?.1;
                                forces_to(
                                    types,
                                    expr.typed,
                                    typ,
                                    expr.loc,
                                    &free_types,
                                    UnificationMethod::StrictSnd,
                                )?;
                                exprs_out.push(expr);
                                println!("finished this expr");
                            }
                            let type_id = add_type(types, out, &free_types);
                            check_coherence(types, type_id, top_expr.typed, top_expr.loc, ctxt)?;
                            Ok((
                                false,
                                Expr {
                                    content: Box::new(ExprInner::FunCallPath(
                                        vec_types,
                                        ctxt.get_path(name.get_content()).add_loc(),
                                        exprs_out,
                                    )),
                                    loc: top_expr.loc,
                                    typed: type_id,
                                },
                            ))
                        } else {
                            Err(vec![TypeError::wrong_nb_args(
                                name.get_loc(),
                                exprs.len(),
                                args.len(),
                            )])
                        }
                    } else {
                        Err(vec![TypeError::unknown_var(name)])
                    }
                }
                Some(type_id) if args.is_empty() => match types.get(type_id).unwrap() {
                    Types::Unknown if args.is_empty() => todo!(),
                    Types::Fun(args_type_id, out_type_id) if args.is_empty() => {
                        let out_type_id = *out_type_id;
                        let mut exprs_out = Vec::new();
                        for (expr, typ) in exprs.into_iter().zip(args_type_id.clone().into_iter()) {
                            let expr = type_expr(ctxt, local_ctxt, expr, types, out_type)?.1;
                            make_coherent(
                                types,
                                expr.typed,
                                typ,
                                expr.loc,
                                UnificationMethod::StrictSnd,
                            )?;
                            exprs_out.push(expr)
                        }
                        check_coherence(types, out_type_id, top_expr.typed, top_expr.loc, ctxt)?;
                        Ok((
                            false,
                            Expr {
                                content: Box::new(ExprInner::FunCall(Vec::new(), name, exprs_out)),
                                loc: top_expr.loc,
                                typed: out_type_id,
                            },
                        ))
                    }
                    _ => todo!(),
                },
                Some(_) => todo!(),
            }
        }

        ExprInner::FunCallPath(args, path, exprs) => {
            assert!(args.is_empty());
            match (ctxt.get_fun(&path), ctxt.is_constructor(&path.cleaned())) {
                (Some(info), None) => {
                    let (free, args, out) = info.get_typ();
                    if args.len() == exprs.len() {
                        let mut free_types = HashMap::new();
                        let mut vec_types = Vec::new();
                        for name in free.iter() {
                            let type_id = types.insert_type(Types::unknown());
                            vec_types.push(type_id);
                            free_types.insert(name.clone(), type_id);
                        }
                        let mut exprs_out = Vec::new();
                        for (expr, typ) in exprs.into_iter().zip(args.iter()) {
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
                        check_coherence(types, type_id, top_expr.typed, top_expr.loc, ctxt)?;
                        Ok((
                            false,
                            Expr {
                                content: Box::new(ExprInner::FunCallPath(
                                    vec_types, path, exprs_out,
                                )),
                                loc: top_expr.loc,
                                typed: type_id,
                            },
                        ))
                    } else {
                        Err(vec![TypeError::wrong_nb_args(
                            top_expr.loc,
                            exprs.len(),
                            args.len(),
                        )])
                    }
                }
                (None, Some(enum_info)) => {
                    let mut free_types = HashMap::new();
                    let mut vec_types = Vec::new();
                    for name in enum_info.get_free_types().iter() {
                        let type_id = types.insert_type(Types::unknown());
                        vec_types.push(type_id);
                        free_types.insert(name.clone(), type_id);
                    }
                    let mut exprs_out = Vec::new();
                    let mut path2 = path.cleaned();
                    let constructor = match path2.pop().unwrap() {
                        NamePath::Name(id) => id,
                        _ => panic!("ICE"),
                    };
                    for (expr, typ) in exprs
                        .into_iter()
                        .zip(enum_info.get_constructor(&constructor).unwrap().1.iter())
                    {
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
                    let type_id = types.insert_type(Types::Enum(path2, vec_types));
                    check_coherence(types, type_id, top_expr.typed, top_expr.loc, ctxt)?;
                    Ok((
                        false,
                        Expr {
                            content: Box::new(ExprInner::Constructor(path, exprs_out)),
                            loc: top_expr.loc,
                            typed: type_id,
                        },
                    ))
                }
                (Some(_), Some(_)) => todo!(),
                (None, None) => Err(vec![TypeError::unknown_path(path)]),
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
            check_coherence(types, type_id, top_expr.typed, top_expr.loc, ctxt)?;
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
                if name.get_content().len() == 3
                    && name.get_content()[0] == NamePath::Name("std".to_string())
                    && name.get_content()[1] == NamePath::Name("vec".to_string())
                    && name.get_content()[2] == NamePath::Name("Vec".to_string())
                    && args.len() == 1
                {
                    let type_id = args[0];
                    // can be improved
                    check_coherence(types, type_id, top_expr.typed, top_expr.loc, ctxt)?;
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
            check_coherence(types, type_id, top_expr.typed, top_expr.loc, ctxt)?;
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
            check_coherence(types, type_id, top_expr.typed, top_expr.loc, ctxt)?;
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
                check_coherence(types, type_id, top_expr.typed, top_expr.loc, ctxt)?;
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
            check_coherence(types, type_id, top_expr.typed, top_expr.loc, ctxt)?;
            Ok((
                false,
                Expr {
                    content: Box::new(ExprInner::MacroCall(name, vec![expr])),
                    typed: type_id,
                    loc: top_expr.loc,
                },
            ))
        }

        ExprInner::MacroCall(name, mut exprs)
            if name.get_content() == "print_usize" && exprs.len() == 1 =>
        {
            let expr = type_expr(ctxt, local_ctxt, exprs.pop().unwrap(), types, out_type)?.1;
            let type_id = types.insert_type(Types::usize());
            make_coherent(
                types,
                type_id,
                expr.typed,
                top_expr.loc,
                UnificationMethod::Smallest,
            )?;
            let type_id = types.insert_unit();
            check_coherence(types, type_id, top_expr.typed, top_expr.loc, ctxt)?;
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
                get_method_caller_name(types, expr.typed, expr.loc, false)?
            {
                if let Some(method) = ctxt.get_method_function(struct_name, &method_name) {
                    let method = method.add_loc();
                    let (frees, args, out) = ctxt.get_fun(&method).unwrap().get_typ();
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
                    let type_id = add_type(types, &*out, &free_types);
                    check_coherence(types, type_id, top_expr.typed, top_expr.loc, ctxt)?;
                    Ok((
                        false,
                        Expr {
                            content: Box::new(ExprInner::FunCallPath(params, method, exprs_out)),
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

        ExprInner::Parenthesis(expr) => {
            let expr = type_expr(ctxt, local_ctxt, expr, types, out_type)?.1;
            check_coherence(types, expr.typed, top_expr.typed, top_expr.loc, ctxt)?;
            Ok((false, expr))
        }

        ExprInner::PatternMatching(expr, cases, fall) => {
            let expr = type_expr(ctxt, local_ctxt, expr, types, out_type)?.1;
            let (reference, mut enum_info, free) = match get_enum(types, expr.typed, expr.loc)? {
                None => todo!(),
                Some((_, None)) => todo!(),
                Some((reference, Some((path, free)))) => {
                    (reference, ctxt.enum_info(path).unwrap(), free)
                }
            };
            let mut free_types = HashMap::new();
            let free_names = enum_info.get_free_types();
            assert_eq!(free_names.len(), free.len());
            for (name, type_id) in free_names.iter().zip(free.iter()) {
                assert!(free_types.insert(name.to_string(), *type_id).is_none())
            }

            let mut type_id_out = types.insert_type(Types::unknown());
            let mut rows = Vec::new();
            for row in cases {
                local_ctxt.add_layer();
                let cons = match row.constructor.last() {
                    None => todo!(),
                    Some(id) => {
                        println!("{:?} {:?}", row.constructor, id);
                        if row.guard.is_none() {
                            enum_info.update_constructor(id.get_content())
                        } else {
                            enum_info.get_constructor(id.get_content())
                        }
                    }
                };
                let arg_types = match cons {
                    None => todo!(),
                    Some((true, _)) => todo!(),
                    Some((false, types)) => types,
                };
                if arg_types.len() != row.arguments.len() {
                    todo!()
                }
                for ((mutable, arg), post_type) in row.arguments.iter().zip(arg_types.iter()) {
                    let type_id = add_type(types, post_type, &free_types);
                    let type_id = match reference {
                        None => type_id,
                        Some(opt) => types.insert_type(Types::Ref(opt, type_id)),
                    };
                    local_ctxt.add_var(arg, *mutable, type_id)
                }

                let guard = match row.guard {
                    None => None,
                    Some(expr) => {
                        let expr = type_expr(ctxt, local_ctxt, expr, types, out_type)?.1;
                        let type_id = types.insert_bool();
                        make_coherent(
                            types,
                            type_id,
                            expr.typed,
                            expr.loc,
                            UnificationMethod::NoRef,
                        )?;
                        Some(expr)
                    }
                };

                let (type_id_row, row_bloc) =
                    type_bloc(ctxt, local_ctxt, row.bloc, types, out_type, None)?;

                local_ctxt.pop_layer();
                type_id_out = make_coherent(
                    types,
                    type_id_out,
                    type_id_row,
                    row_bloc.loc,
                    UnificationMethod::Smallest,
                )?;

                rows.push(Pattern {
                    constructor: row.constructor,
                    arguments: row.arguments,
                    guard,
                    bloc: row_bloc,
                })
            }
            let fall = match fall {
                None => match enum_info.check_finished() {
                    None => None,
                    Some(id) => return Err(vec![TypeError::incomplete_match(expr.loc, id)]),
                },
                Some((mutable, ident, bloc)) => {
                    local_ctxt.add_layer();
                    local_ctxt.add_var(&ident, mutable, expr.typed);

                    let (type_id2, bloc) =
                        type_bloc(ctxt, local_ctxt, bloc, types, out_type, None)?;
                    local_ctxt.pop_layer();
                    type_id_out = make_coherent(
                        types,
                        type_id_out,
                        type_id2,
                        bloc.loc,
                        UnificationMethod::Smallest,
                    )?;
                    Some((mutable, ident, bloc))
                }
            };
            Ok((
                false,
                Expr {
                    content: Box::new(ExprInner::PatternMatching(expr, rows, fall)),
                    loc: top_expr.loc,
                    typed: type_id_out,
                },
            ))
        }

        ExprInner::Proj(expr, Projector::Int(proj_id)) => {
            let (affectable, expr) = type_expr(ctxt, local_ctxt, expr, types, out_type)?;
            let (affectable, type_id) = if let Some((affectable, tuple)) =
                get_tuple(types, expr.typed, expr.loc, affectable)?
            {
                if proj_id >= tuple.len() {
                    return Err(vec![TypeError::out_of_bound_tuple(
                        top_expr.loc,
                        proj_id,
                        tuple.len(),
                    )]);
                } else {
                    (affectable, tuple[proj_id])
                }
            } else {
                (true, types.insert_type(Types::unknown()))
            };
            check_coherence(types, type_id, top_expr.typed, top_expr.loc, ctxt)?;
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
                let struct_info = ctxt.get_struct_path(struct_name).unwrap();
                if let Some(field_typ) = struct_info.get_field_typ(proj_name.get_content()) {
                    (affectable, add_type(types, field_typ, &HashMap::new()))
                } else {
                    return Err(vec![TypeError::struct_no_field(
                        top_expr.loc,
                        struct_name.clone(),
                        proj_name.content(),
                    )]);
                }
            } else {
                (true, types.insert_type(Types::unknown()))
            };
            check_coherence(types, type_id, top_expr.typed, top_expr.loc, ctxt)?;
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
            check_coherence(types, type_id, top_expr.typed, top_expr.loc, ctxt)?;
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
            check_coherence(types, type_id, top_expr.typed, top_expr.loc, ctxt)?;
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
            check_coherence(types, type_id, top_expr.typed, top_expr.loc, ctxt)?;
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
            check_coherence(types, type_id, top_expr.typed, top_expr.loc, ctxt)?;
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
            check_coherence(types, type_id, top_expr.typed, top_expr.loc, ctxt)?;
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
                check_coherence(types, type_id, top_expr.typed, top_expr.loc, ctxt)?;
                Ok((
                    local_ctxt.is_mut(&var_name).unwrap(),
                    Expr {
                        content: Box::new(ExprInner::Var(var_name)),
                        loc: top_expr.loc,
                        typed: type_id,
                    },
                ))
            } else {
                match (
                    ctxt.get_top_fun(var_name.get_content()),
                    ctxt.get_top_const_val(var_name.get_content()),
                ) {
                    (Some(fun_info), None) => {
                        let (free, args, out_type) = fun_info.get_typ();
                        let mut free_types2 = HashMap::new();
                        for name in free.iter() {
                            free_types2
                                .insert(name.to_string(), types.insert_type(Types::unknown()));
                        }
                        let type_vec = args
                            .iter()
                            .map(|t| add_type(types, t, &free_types2))
                            .collect();
                        let out_type = add_type(types, out_type, &free_types2);
                        let type_id = types.insert_type(Types::Fun(type_vec, out_type));
                        Ok((
                            false,
                            Expr {
                                content: Box::new(ExprInner::VarPath(
                                    ctxt.get_path(var_name.get_content()).add_loc(),
                                )),
                                loc: top_expr.loc,
                                typed: type_id,
                            },
                        ))
                    }

                    (None, Some(Const { typ, .. })) => {
                        let type_id = add_type(types, typ, &HashMap::new());
                        Ok((
                            false,
                            Expr {
                                content: Box::new(ExprInner::VarPath(
                                    ctxt.get_path(var_name.get_content()).add_loc(),
                                )),
                                loc: top_expr.loc,
                                typed: type_id,
                            },
                        ))
                    }
                    (None, None) => Err(vec![TypeError::unknown_var(var_name)]),
                    (Some(_), Some(_)) => todo!(),
                }
            }
        }

        ExprInner::VarPath(var_path) => {
            let mut cleaned_path = var_path.cleaned();
            match (
                ctxt.get_fun(&var_path),
                ctxt.get_const_val(&cleaned_path),
                ctxt.is_constructor(&cleaned_path),
            ) {
                (Some(fun_info), None, None) => {
                    let (free, args, out_type) = fun_info.get_typ();
                    if !free.is_empty() {
                        panic!("Feature not handled")
                    }
                    let free_types2 = HashMap::new();
                    let type_vec = args
                        .iter()
                        .map(|t| add_type(types, t, &free_types2))
                        .collect();
                    let out_type = add_type(types, out_type, &free_types2);
                    let type_id = types.insert_type(Types::Fun(type_vec, out_type));
                    Ok((
                        false,
                        Expr {
                            content: Box::new(ExprInner::VarPath(var_path)),
                            loc: top_expr.loc,
                            typed: type_id,
                        },
                    ))
                }
                (None, Some(Const { typ, .. }), None) => {
                    let type_id = add_type(types, typ, &HashMap::new());
                    Ok((
                        false,
                        Expr {
                            content: Box::new(ExprInner::VarPath(var_path)),
                            loc: top_expr.loc,
                            typed: type_id,
                        },
                    ))
                }
                (None, None, Some(info)) => {
                    cleaned_path.pop();
                    let free_types = info
                        .get_free_types()
                        .iter()
                        .map(|_| types.insert_type(Types::unknown()))
                        .collect();
                    let type_id = types.insert_type(Types::Enum(cleaned_path, free_types));
                    Ok((
                        false,
                        Expr {
                            content: Box::new(ExprInner::Constructor(var_path, Vec::new())),
                            loc: top_expr.loc,
                            typed: type_id,
                        },
                    ))
                }
                (None, None, None) => Err(vec![TypeError::unknown_path(var_path)]),
                (Some(_), Some(_), _) => todo!(),
                (Some(_), None, Some(_)) => todo!(),
                (None, Some(_), Some(_)) => todo!(),
            }
        }

        ExprInner::Coercion(expr, typ) => {
            let (_, expr) = type_expr(ctxt, local_ctxt, expr, types, out_type)?;
            let type_id = types.insert_type(Types::unknown());
            check_coherence(types, type_id, typ, top_expr.loc, ctxt)?;
            check_coherence(types, type_id, top_expr.typed, top_expr.loc, ctxt)?;
            Ok((
                false,
                Expr {
                    content: Box::new(ExprInner::Coercion(expr, type_id)),
                    loc: top_expr.loc,
                    typed: type_id,
                },
            ))
        }

        ExprInner::Return(None) => {
            let type_id = types.insert_unit();
            make_coherent(
                types,
                type_id,
                out_type,
                Location::default(),
                UnificationMethod::NoRef,
            )?;
            let type_id = types.insert_type(Types::unknown());
            Ok((
                false,
                Expr {
                    content: Box::new(ExprInner::Return(None)),
                    loc: top_expr.loc,
                    typed: type_id,
                },
            ))
        }

        ExprInner::Return(Some(expr)) => {
            let expr = type_expr(ctxt, local_ctxt, expr, types, out_type)?.1;
            make_coherent(
                types,
                expr.typed,
                out_type,
                expr.loc,
                UnificationMethod::StrictSnd,
            )?;
            let type_id = types.insert_type(Types::unknown());
            Ok((
                false,
                Expr {
                    content: Box::new(ExprInner::Return(Some(expr))),
                    loc: top_expr.loc,
                    typed: type_id,
                },
            ))
        }

        ExprInner::While(expr, bloc) => {
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
            Ok((
                false,
                Expr {
                    content: Box::new(ExprInner::While(expr, bloc)),
                    loc: top_expr.loc,
                    typed: types.insert_unit(),
                },
            ))
        }
    };
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
        let instr_content = match instr.content {
            InstrInner::Binding(mutable, name, typ, expr) => {
                let expr = type_expr(ctxt, local_ctxt, expr, types, out_type)?.1;
                let type_id = match typ {
                    None => expr.typed,
                    Some(typ) => {
                        let type_id = types.insert_type(Types::unknown());
                        check_coherence(types, type_id, Some(typ), expr.loc, ctxt)?;
                        make_coherent(
                            types,
                            expr.typed,
                            type_id,
                            expr.loc,
                            UnificationMethod::StrictSnd,
                        )?;
                        type_id
                    }
                };
                local_ctxt.add_var(&name, mutable, expr.typed);
                InstrInner::Binding(mutable, name, type_id, expr)
            }

            InstrInner::Expr(drop, expr) => {
                let expr = type_expr(ctxt, local_ctxt, expr, types, out_type)?.1;
                InstrInner::Expr(drop, expr)
            }
        };
        content.push(Instr {
            content: instr_content,
            loc: instr.loc,
        })
    }

    let type_id = match content.pop() {
        None => types.insert_unit(),
        Some(instr) => {
            let type_id = match expected {
                None => match &instr.content {
                    InstrInner::Expr(ComputedValue::Keep, expr) => expr.typed,
                    _ => types.insert_unit(),
                },
                Some(type_id) => match &instr.content {
                    InstrInner::Expr(ComputedValue::Keep, expr) => make_coherent(
                        types,
                        type_id,
                        expr.typed,
                        bloc.loc,
                        UnificationMethod::StrictFst,
                    )?,
                    _ => {
                        let type_id2 = types.insert_unit();
                        make_coherent(
                            types,
                            type_id2,
                            type_id,
                            instr.loc,
                            UnificationMethod::NoRef,
                        )?
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

pub fn type_const(
    expr: Expr,
    ctxt: &GlobalContext,
    expected_typ: &PostType,
) -> Result<(TypeStorage, Expr<usize>), Vec<TypeError>> {
    let mut local_ctxt = LocalContext::new(Vec::new()).unwrap();
    let mut types = TypeStorage::new(&Vec::new());
    let out_type = types.insert_unit();
    let expr = type_expr(ctxt, &mut local_ctxt, expr, &mut types, out_type)?.1;
    forces_to(
        &mut types,
        expr.typed,
        expected_typ,
        expr.loc,
        &HashMap::new(),
        UnificationMethod::StrictSnd,
    )?;
    Ok((types, expr))
}

pub fn type_funs(
    name_fun: &Ident,
    ctxt: &GlobalContext,
    free: &Vec<String>,
    args: &[(String, bool, &PostType)],
    out_type: &PostType,
    body: Bloc,
) -> Result<(Bloc<usize>, TypeStorage), Vec<TypeError>> {
    let mut types = TypeStorage::new(free);
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
