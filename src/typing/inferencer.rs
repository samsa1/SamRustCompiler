use crate::ast::rust::*;
use crate::ast::typed_rust::{PostType, PostTypeInner};
use crate::ast::common::{BinOperator, UnaOperator, Ident, BuiltinType, Sizes, Projector, Location};
use super::context::GlobalContext;
use std::collections::HashMap;

#[derive(Debug)]
pub enum TypeErrorInfo {
    ExpectedStruct,
    ExpectedTuple,
    Unkown,
    NotCompatible(Types, Types),
    TryUnref(Types),
    UndeclaredVariable(String),
}

#[derive(Debug)]
pub struct TypeError {
    loc : Location,
    info : TypeErrorInfo,
}

impl TypeError {
    fn new(loc : Location, info : TypeErrorInfo) -> Self {
        Self {
            loc,
            info,
        }
    }

    fn expected_struct(loc : Location) -> Self {
        Self {
            loc,
            info : TypeErrorInfo::ExpectedStruct,
        }
    }

    fn expected_tuple(loc : Location) -> Self {
        Self {
            loc,
            info : TypeErrorInfo::ExpectedTuple,
        }
    }

    fn unknown_error(loc : Location) -> Self {
        todo!();
        Self {
            loc,
            info : TypeErrorInfo::Unkown,
        }
    }

    fn not_compatible(loc : Location, typ1 : Types, typ2 : Types) -> Self {
        Self {
            loc,
            info : TypeErrorInfo::NotCompatible(typ1, typ2)
        }
    }

    fn cannot_unref(loc : Location, typ : Types) -> Self {
        Self {
            loc,
            info : TypeErrorInfo::TryUnref(typ)
        }

    }

    fn unknown_var(id : Ident) -> Self {
        Self {
            loc : id.get_loc(),
            info : TypeErrorInfo::UndeclaredVariable(id.content())
        }
    }
}

#[derive(Clone, Debug)]
pub struct LocalContext {
    vars: Vec<HashMap<String, usize>>,
}

impl LocalContext {
    pub fn new(in_types: Vec<(String, usize)>) -> Self {
        let mut in_types2 = HashMap::new();
        for (name, type_id) in in_types.into_iter() {
            assert!(in_types2
                .insert(name, type_id)
                .is_none())
        }

        Self {
            vars: vec![in_types2],
        }
    }

    pub fn add_layer(&mut self) {
        self.vars.push(HashMap::new())
    }

    pub fn pop_layer(&mut self) -> Option<HashMap<String, usize>> {
        self.vars.pop()
    }

    pub fn get_typ(&self, var_name: &Ident) -> Option<usize> {
        for hashmap in self.vars.iter().rev() {
            if let Some(type_id) = hashmap.get(var_name.get_content()) {
                return Some(*type_id);
            }
        }
        None
    }

    pub fn add_var(&mut self, ident: &Ident, type_id : usize) {
        if let Some(last) = self.vars.last_mut() {
            last.insert(ident.get_content().to_string(), type_id);
        } else {
            panic!("should never happend")
        }
    }

}

fn get_struct_name(types : &TypeStorage, type_id : usize, loc : Location) -> Result<Option<&str>, Vec<TypeError>> {
    match types.get(type_id) {
        None => panic!("ICE"),
        Some(Types::Struct(name)) => Ok(Some(name)),
        Some(Types::Unknown) => Ok(None),
        Some(Types::Ref(type_id)) => get_struct_name(types, *type_id, loc),
        _ => Err(vec![TypeError::expected_struct(loc)]),
    }
}

fn get_tuple(types : &TypeStorage, type_id : usize, loc : Location) -> Result<Option<&Vec<usize>>, Vec<TypeError>> {
    match types.get(type_id) {
        None => panic!("ICE"),
        Some(Types::Tuple(vec)) => Ok(Some(vec)),
        Some(Types::Unknown) => Ok(None),
        Some(Types::Ref(type_id)) => get_tuple(types, *type_id, loc),
        _ => Err(vec![TypeError::expected_tuple(loc)]),
    }
}

fn check_coherence(types : &mut TypeStorage, type_id : usize, type_2 : Option<PreType>, loc : Location) -> Result<(), Vec<TypeError>> {
    match type_2 {
        None => Ok(()),
        Some(_) => todo!(),
    }
}

// result must be "<= min(type_id1, type_id2)"
// but we also have "if type_id_ != output { types.get(type_id_) = SameAs(output) }"
fn make_coherent(types : &mut TypeStorage, type_id1 : usize, type_id2 : usize, loc : Location) -> Result<usize, Vec<TypeError>> {
    let (typ1, typ2) = match (types.get(type_id1), types.get(type_id2)) {
        (Some(typ1), Some(typ2)) => (typ1, typ2),
        _ => panic!("ICE")
    };
    let id = match (typ1, typ2) {
        (Types::Unknown, _) => type_id2,
        (_, Types::Unknown) => type_id1,

        (Types::SameAs(type_id1b), Types::SameAs(type_id2b)) => 
            make_coherent(types, *type_id1b, *type_id2b, loc)?,
        (_, Types::SameAs(type_id2b)) =>
            make_coherent(types, type_id1, *type_id2b, loc)?,
        (Types::SameAs(type_id1b), _) =>
            make_coherent(types, *type_id1b, type_id2, loc)?,

        (Types::Bool, Types::Bool) => type_id1,
        (Types::Bool, _) | (_, Types::Bool) => return Err(vec![TypeError::not_compatible(loc, typ1.clone(), typ2.clone())]),

        (Types::Int(b1, s1), Types::Int(b2, s2)) => {
            let b = match (b1, b2) {
                (None, None) => None,
                (Some(b), None) | (None, Some(b)) => Some(*b),
                (Some(b1), Some(b2)) if b1 == b2 => Some(*b1),
                _ => return Err(vec![TypeError::unknown_error(loc)]),
            };
            let s = match (s1, s2) {
                (None, None) => None,
                (Some(s), None) | (None, Some(s)) => Some(*s),
                (Some(s1), Some(s2)) if s1 == s2 => Some(*s1),
                _ => return Err(vec![TypeError::unknown_error(loc)]),
            };
            types.set(type_id1, Types::Int(b, s));
            type_id1
        },
        (Types::Int(_, _), _) | (_, Types::Int(_, _)) => return Err(vec![TypeError::not_compatible(loc, typ1.clone(), typ2.clone())]),

        (Types::Tuple(vec1), Types::Tuple(vec2)) if vec1.len() == vec2.len() => {
            let mut vec = Vec::new();
            for (t1, t2) in vec1.clone().into_iter().zip(vec2.clone().into_iter()) {
                vec.push(make_coherent(types, t1, t2, loc)?);
            }
            types.set(type_id1, Types::Tuple(vec));
            type_id1
        }
        (Types::Tuple(_), _) | (_, Types::Tuple(_)) => return Err(vec![TypeError::not_compatible(loc, typ1.clone(), typ2.clone())]),

        (Types::Ref(type_id1b), Types::Ref(type_id2b)) => {
            let type_id = make_coherent(types, *type_id1b, *type_id2b, loc)?;
            types.set(type_id1, Types::refed(type_id));
            type_id1
        },
        (Types::Ref(_), _) | (_, Types::Ref(_)) => return Err(vec![TypeError::not_compatible(loc, typ1.clone(), typ2.clone())]),

        (t1, t2) => {println!("not implemented for {t1:?} {t2:?}"); todo!()},
    };
    if type_id1 != id { types.set(type_id1, Types::SameAs(id)); };
    if type_id2 != id { types.set(type_id2, Types::SameAs(id)); };
    Ok(id)

}

fn try_deref(types : &mut TypeStorage, type_id : usize, loc : Location) -> Result<usize, Vec<TypeError>> {
    match types.get(type_id) {
        None => panic!("ICE"),
        Some(Types::SameAs(type_id)) => try_deref(types, *type_id, loc),
        Some(typ) => match typ.unref() {
            None => Err(vec![TypeError::cannot_unref(loc, typ.clone())]),
            Some(type_id) => Ok(type_id)
        }
    }
}

// Add type 
fn add_type(types : &mut TypeStorage, post_type : &PostType, free_types : &HashMap<String, usize>) -> usize {
    match &post_type.content {
        PostTypeInner::Box(typ) => {
            let type_id = add_type(types, typ, free_types);
            types.insert_type(Types::boxed(type_id))
        },
        PostTypeInner::BuiltIn(BuiltinType::Bool) => types.insert_bool(),
        PostTypeInner::BuiltIn(BuiltinType::Int(signed, size)) => types.insert_type(Types::Int(Some(*signed), Some(*size))),
        PostTypeInner::Diverge => todo!(),
        PostTypeInner::Enum(_) => todo!(),
        PostTypeInner::Fun(free, type_vec, out_type) => {
            let mut free_types2 = free_types.clone();
            for name in free.iter() {
                free_types2.insert(name.to_string(), types.insert_type(Types::unknown()));
            }
            let type_vec = type_vec.iter().map(|t| add_type(types, t, &free_types2)).collect();
            let out_type = add_type(types, out_type, &free_types2);
            types.insert_type(Types::Fun(type_vec, out_type))
        },
        PostTypeInner::IdentParametrized(_, _) => todo!(),
        PostTypeInner::Ref(_, typ) => {
            let type_id = add_type(types, typ, free_types);
            types.insert_type(Types::refed(type_id))
        },
        PostTypeInner::String => types.insert_string(),
        PostTypeInner::Struct(s) => types.insert_type(Types::Struct(s.to_string())),
        PostTypeInner::Tuple(type_vec) => {
            let type_vec = type_vec.iter().map(|t| add_type(types, t, free_types)).collect();
            types.insert_type(Types::Tuple(type_vec))
        },
        PostTypeInner::FreeType(name) => {
            *free_types.get(name).unwrap()
        },
    }
}

// Used in function call and in building structures
fn forces_to(types : &mut TypeStorage, type_id : usize, typ : &PostType, loc : Location, free_types : &HashMap<String, usize>) -> Result<(), Vec<TypeError>> {
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
                _ => Err(vec![TypeError::unknown_error(loc)]),
            }
        }
        _ => todo!(),
    }
}

fn type_expr(ctxt : &GlobalContext, local_ctxt : &mut LocalContext, top_expr : Expr, types : &mut TypeStorage, out_type : usize) -> Result<Expr<usize>, Vec<TypeError>> {
    match *top_expr.content {
        ExprInner::Array(_) => todo!(),

        ExprInner::BinaryOp(BinOperator::Set, expr1, expr2) => {
            let expr1 = type_expr(ctxt, local_ctxt, expr1, types, out_type)?;
            let expr2 = type_expr(ctxt, local_ctxt, expr2, types, out_type)?;
            make_coherent(types, expr1.typed, expr2.typed, expr1.loc)?;
            let type_id = types.insert_unit();
            check_coherence(types, type_id, top_expr.typed, top_expr.loc)?;
            Ok(Expr {
                content : Box::new(ExprInner::BinaryOp(BinOperator::Set, expr1, expr2)),
                typed : type_id,
                loc : top_expr.loc,
            })
        },

        ExprInner::BinaryOp(op, expr1, expr2) => {
            let expr1 = type_expr(ctxt, local_ctxt, expr1, types, out_type)?;
            let expr2 = type_expr(ctxt, local_ctxt, expr2, types, out_type)?;
            if op == BinOperator::And || op == BinOperator::Or {
                types.forces_to(expr1.typed, Types::bool()).unwrap();
                types.forces_to(expr2.typed, Types::bool()).unwrap();
                let type_id = types.insert_bool();
                check_coherence(types, type_id, top_expr.typed, top_expr.loc)?;
                Ok(Expr {
                    content : Box::new(ExprInner::BinaryOp(op, expr1, expr2)),
                    loc : top_expr.loc,
                    typed : type_id,
                })
            } else if op == BinOperator::Eq || op == BinOperator::Ne
                    || op == BinOperator::Greater || op == BinOperator::GreaterEq
                    || op == BinOperator::Lower || op == BinOperator::LowerEq {
                make_coherent(types, expr1.typed, expr2.typed, top_expr.loc)?;
                let type_id = types.insert_bool();
                check_coherence(types, type_id, top_expr.typed, top_expr.loc)?;
                Ok(Expr {
                    content : Box::new(ExprInner::BinaryOp(op, expr1, expr2)),
                    loc : top_expr.loc,
                    typed : type_id,
                })
            } else if op == BinOperator::Add || op == BinOperator::Sub
                    || op == BinOperator::Mod || op == BinOperator::Mul
                    || op == BinOperator::Div {
                let type_id = make_coherent(types, expr1.typed, expr2.typed, top_expr.loc)?;
                check_coherence(types, type_id, top_expr.typed, top_expr.loc)?;
                Ok(Expr {
                    typed : type_id,
                    content : Box::new(ExprInner::BinaryOp(op, expr1, expr2)),
                    loc : top_expr.loc,
                })
            } else { panic!("ICE") }
        },
        
        ExprInner::Bloc(bloc) => {
            let (type_id, bloc) = type_bloc(ctxt, local_ctxt, bloc, types, out_type, None)?;
            check_coherence(types, type_id, top_expr.typed, top_expr.loc)?;
            Ok(Expr {
                content : Box::new(ExprInner::Bloc(bloc)),
                typed : type_id,
                loc : top_expr.loc,
            })
        },

        ExprInner::Bool(b) => {
            let type_id = types.insert_bool();
            check_coherence(types, type_id, top_expr.typed, top_expr.loc)?;
            Ok(Expr {
                content : Box::new(ExprInner::Bool(b)),
                loc : top_expr.loc,
                typed : type_id
            })
        },

        ExprInner::BuildStruct(struct_name, args) => {
            if let Some(struct_info) = ctxt.get_struct(struct_name.get_content()) {
                let mut struct_info = struct_info.clone();
                let mut args2 = Vec::new();
                for (name, expr) in args.into_iter() {
                    let expr = type_expr(ctxt, local_ctxt, expr, types, out_type)?;
                    if let Some(typ) = struct_info.get_typ(name.get_content()) {
                        forces_to(types, expr.typed, typ, top_expr.loc, &HashMap::new())?;
                        args2.push((name, expr));
                    } else {
                        todo!()
                    }
                };
                match struct_info.check_finished() {
                    Some(_) => todo!(),
                    None => (),
                };
                let type_id = types.insert_type(Types::struct_from_str(struct_name.get_content()));
                check_coherence(types, type_id, top_expr.typed, top_expr.loc)?;
                Ok(Expr {
                    content : Box::new(ExprInner::BuildStruct(struct_name, args2)),
                    loc : top_expr.loc,
                    typed : type_id,
                })
            } else {
                Err(vec![TypeError::unknown_error(top_expr.loc)])
            }
        },

        ExprInner::Deref(expr) => {
            let expr = type_expr(ctxt, local_ctxt, expr, types, out_type)?;
            let type_id = try_deref(types, expr.typed, expr.loc)?;
            check_coherence(types, type_id, top_expr.typed, top_expr.loc)?;
            Ok(Expr {
                content : Box::new(ExprInner::Deref(expr)),
                loc : top_expr.loc,
                typed : type_id,
            })
        },

        ExprInner::FunCall(name, exprs) => {
            match local_ctxt.get_typ(&name) {
                None => {
                    if let Some(typ) = ctxt.get_typ(name.get_content()) {
                        match &typ.content {
                            PostTypeInner::Fun(free, args, out) if args.len() == exprs.len() => {
                                let mut free_types = HashMap::new();
                                for name in free.iter() {
                                    free_types.insert(name.clone(), types.insert_type(Types::unknown()));
                                }
                                let mut exprs_out = Vec::new();
                                for (expr, typ) in exprs.into_iter().zip(args.iter()) {
                                    let expr = type_expr(ctxt, local_ctxt, expr, types, out_type)?;
                                    forces_to(types, expr.typed, typ, expr.loc, &free_types)?;
                                    exprs_out.push(expr)
                                };
                                let type_id = add_type(types, out, &free_types);
                                check_coherence(types, type_id, top_expr.typed, top_expr.loc)?;
                                Ok(Expr {
                                    content : Box::new(ExprInner::FunCall(name, exprs_out)),
                                    loc : top_expr.loc,
                                    typed : type_id,
                                })
                            },
                            _ => Err(vec![TypeError::unknown_error(name.get_loc())])
                        }
                    } else {
                        Err(vec![TypeError::unknown_var(name)])
                    }
                }
                Some(type_id) => {
                    match types.get(type_id).unwrap() {
                        Types::Unknown => todo!(),
                        Types::Fun(args, type_id) => {
                            todo!()
                        },
                        _ => todo!(),
                    }
                },
            }
        },

        ExprInner::If(expr, bloc1, bloc2) => {
            let expr = type_expr(ctxt, local_ctxt, expr, types, out_type)?;
            let (type_id1, bloc1) = type_bloc(ctxt, local_ctxt, bloc1, types, out_type, None)?;
            let (type_id2, bloc2) = type_bloc(ctxt, local_ctxt, bloc2, types, out_type, None)?;
            types.forces_to(expr.typed, Types::bool()).unwrap();
            let type_id = make_coherent(types, type_id1, type_id2, top_expr.loc)?;
            check_coherence(types, type_id, top_expr.typed, top_expr.loc)?;
            Ok(Expr {
                content : Box::new(ExprInner::If(expr, bloc1, bloc2)),
                loc : top_expr.loc,
                typed : type_id,
            })
        },
        ExprInner::Index(expr_val, expr_index) => {
            let expr_val = type_expr(ctxt, local_ctxt, expr_val, types, out_type)?;
            let expr_index = type_expr(ctxt, local_ctxt, expr_index, types, out_type)?;
            types.forces_to(expr_index.typed, Types::usize()).unwrap();
            let type_id = types.insert_type(Types::unknown());
            // can be improved
            check_coherence(types, type_id, top_expr.typed, top_expr.loc)?;
            Ok(Expr {
                content : Box::new(ExprInner::Index(expr_val, expr_index)),
                loc : top_expr.loc,
                typed : type_id,
            })
        },

        ExprInner::Int(int) => {
            let type_id = types.insert_type(Types::int());
            check_coherence(types, type_id, top_expr.typed, top_expr.loc)?;
            Ok(Expr {
                content : Box::new(ExprInner::Int(int)),
                loc : top_expr.loc,
                typed : type_id,
            })
        },

        ExprInner::MacroCall(_, _) => todo!(),

        ExprInner::Method(_, _, _) => todo!(),

        ExprInner::Parenthesis(expr) => {
            let expr = type_expr(ctxt, local_ctxt, expr, types, out_type)?;
            check_coherence(types, expr.typed, top_expr.typed, top_expr.loc)?;
            Ok(expr)
        },

        ExprInner::Proj(expr, Projector::Int(proj_id)) => {
            let expr = type_expr(ctxt, local_ctxt, expr, types, out_type)?;
            let type_id = if let Some(tuple) = get_tuple(types, expr.typed, expr.loc)? {
                if proj_id >= tuple.len() {
                    return Err(vec![TypeError::unknown_error(top_expr.loc)])
                } else {
                    tuple[proj_id]
                }
            } else {
                types.insert_type(Types::unknown())
            };
            check_coherence(types, type_id, top_expr.typed, top_expr.loc)?;
            Ok(Expr {
                content : Box::new(ExprInner::Proj(expr, Projector::Int(proj_id))),
                loc : top_expr.loc,
                typed : type_id,
            })
        },

        ExprInner::Proj(expr, Projector::Name(proj_name)) => {
            let expr = type_expr(ctxt, local_ctxt, expr, types, out_type)?;
            let type_id = 
            if let Some(struct_name) = get_struct_name(types, expr.typed, expr.loc)? {
                let struct_info = ctxt.get_struct(struct_name).unwrap();
                if let Some(field_typ) = struct_info.get_field_typ(proj_name.get_content()) {
                    add_type(types, field_typ, &HashMap::new())
                } else {
                    return Err(vec![TypeError::unknown_error(top_expr.loc)])
                }
            } else {
                types.insert_type(Types::unknown())
            };
            check_coherence(types, type_id, top_expr.typed, top_expr.loc)?;
            Ok(Expr {
                content : Box::new(ExprInner::Proj(expr, Projector::Name(proj_name))),
                loc : top_expr.loc,
                typed : type_id
            })
        },

        ExprInner::Ref(mutable, expr) => {
            let expr = type_expr(ctxt, local_ctxt, expr, types, out_type)?;
            let type_id = types.insert_type(Types::Ref(expr.typed));
            check_coherence(types, type_id, top_expr.typed, top_expr.loc)?;
            Ok(Expr {
                content : Box::new(ExprInner::Ref(mutable, expr)),
                loc : top_expr.loc,
                typed : type_id
            })
        },

        ExprInner::String(string) => {
            let type_id = types.insert_string();
            check_coherence(types, type_id, top_expr.typed, top_expr.loc)?;
            Ok(Expr {
                content : Box::new(ExprInner::String(string)),
                loc : top_expr.loc,
                typed : type_id
            })
        },

        ExprInner::Tuple(exprs) => {
            let mut vec_exprs = Vec::new();
            let mut vec_types = Vec::new();
            for expr in exprs.into_iter() {
                let expr = type_expr(ctxt, local_ctxt, expr, types, out_type)?;
                vec_types.push(expr.typed);
                vec_exprs.push(expr);
            };
            let type_id = types.insert_type(Types::tuple(vec_types));
            check_coherence(types, type_id, top_expr.typed, top_expr.loc)?;
            Ok(Expr {
                content : Box::new(ExprInner::Tuple(vec_exprs)),
                loc : top_expr.loc,
                typed : type_id,
            })
        },

        ExprInner::UnaryOp(_, _) => todo!(),

        ExprInner::Var(var_name) => {
            if let Some(type_id) = local_ctxt.get_typ(&var_name) {
                check_coherence(types, type_id, top_expr.typed, top_expr.loc)?;
                Ok(Expr {
                    content : Box::new(ExprInner::Var(var_name)),
                    loc : top_expr.loc,
                    typed : type_id,
                })
            } else {
                todo!()
            }
        },
    }
}

fn type_bloc(ctxt : &GlobalContext, local_ctxt : &mut LocalContext, bloc : Bloc, types : &mut TypeStorage, out_type : usize, expected : Option<usize>) -> Result<(usize, Bloc<usize>), Vec<TypeError>> {
    local_ctxt.add_layer();
    let mut content = Vec::new();
    for instr in bloc.content.into_iter() {
        match instr {
            Instr::Binding(mutable, name, expr) => {
                let expr = type_expr(ctxt, local_ctxt, expr, types, out_type)?;
                local_ctxt.add_var(&name, expr.typed);
                content.push(Instr::Binding(mutable, name, expr))
            },

            Instr::Expr(drop, expr) => {
                let expr = type_expr(ctxt, local_ctxt, expr, types, out_type)?;
                content.push(Instr::Expr(drop, expr))
            },
            Instr::Return(None) => {
                let type_id = types.insert_unit();
                make_coherent(types, type_id, out_type, Location::default())?;
                content.push(Instr::Return(None));
            },
            Instr::Return(Some(expr)) => {
                let expr = type_expr(ctxt, local_ctxt, expr, types, out_type)?;
                make_coherent(types, expr.typed, out_type, expr.loc)?;
                content.push(Instr::Return(Some(expr)))
            },

            Instr::While(expr, bloc) => {
                let expr = type_expr(ctxt, local_ctxt, expr, types, out_type)?;
                let type_id = types.insert_bool();
                make_coherent(types, expr.typed, type_id, expr.loc)?;
                let bloc = type_bloc(ctxt, local_ctxt, bloc, types, out_type, None)?.1;
                content.push(Instr::While(expr, bloc))
            },
        };
        println!("");
        println!("{types:?}");
        println!("{local_ctxt:?}");
        println!("{content:?}");
    };

    let type_id = match content.pop() {
        None => types.insert_unit(),
        Some(instr) => {
            let type_id = match expected {
                None => match &instr {
                    Instr::Expr(true, expr) => expr.typed,
                    _ => types.insert_unit(),
                },
                Some(type_id) => {
                    match &instr {
                        Instr::Expr(true, expr) => {
                            make_coherent(types, type_id, expr.typed, bloc.loc)?
                        },
                        Instr::Return(_) => types.insert_type(Types::unknown()),
                        _ => {
                            let type_id2 = types.insert_unit();
                            make_coherent(types, type_id, type_id2, bloc.loc)?
                        }
                    }
                }
            };
            content.push(instr);
            type_id
        } 
    };

    Ok((type_id, Bloc {
        content,
        loc : bloc.loc
    }))
}

pub fn type_funs(ctxt : &GlobalContext, args : &[(String, &PostType)], out_type : &PostType, body : Bloc) -> Result<(Bloc<usize>, TypeStorage), Vec<TypeError>> {
    let mut types = TypeStorage::new();
    let out_type = add_type(&mut types, out_type, &HashMap::new());
    let vec = args.iter().map(|(name, post_type)| (name.to_string(), add_type(&mut types, post_type, &HashMap::new()))).collect();
    let mut local_ctxt = LocalContext::new(vec);
    let (_, bloc) = type_bloc(ctxt, &mut local_ctxt, body, &mut types, out_type, Some(out_type))?;
    Ok((bloc, types))
}
