use super::context;
use crate::ast::{rust, typed_rust};
use crate::ast::common::*;
use super::types::translate_typ;

pub fn test_borrowing(typ : &typed_rust::PostType, b : bool) {
    if b && !typ.mutable {
        panic!("can't borrow mut")
    }

    match typ.content {
        typed_rust::PostTypeInner::Ref(_) => panic!("not allowed"),
        _ => ()
    }
}

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

pub fn type_checker(ctxt : &context::GlobalContext, expr : rust::Expr, loc_ctxt : &mut context::LocalContext) -> typed_rust::Expr {
    let mut translated_typ = expr.typed.map(|t| translate_typ(t, ctxt));
    let (found_type, content) = 
        match *expr.content {
            rust::ExprInner::Bool(b) => {
                (typed_rust::PostTypeInner::BuiltIn(BuiltinType::Bool).to_nonmut(), typed_rust::ExprInner::Bool(b))
            },

            rust::ExprInner::Int(i) => {
                if is_type_int(&translated_typ) {
                    let typ = translated_typ.unwrap();
                    translated_typ = None;
                    (typ, typed_rust::ExprInner::Int(i))
                } else {
                    (typed_rust::PostTypeInner::BuiltIn(BuiltinType::Int(true, Sizes::S32)).to_nonmut(), typed_rust::ExprInner::Int(i))
                }
            },

            rust::ExprInner::Var(var_name) => {
                let typ =
                    match loc_ctxt.get_typ(&var_name) {
                        | Some(typ) => typ.clone(),
                        | None => panic!("undefined variable")
                    };
                (typ, typed_rust::ExprInner::Var(var_name))
            },

            rust::ExprInner::Ref(b, expr) => {
                let mut expr = type_checker(ctxt, expr, loc_ctxt);
                test_borrowing(&expr.typed, b);
                expr.typed.mutable = b;
                (typed_rust::PostTypeInner::Ref(Box::new(expr.typed.clone())).to_nonmut(),
                typed_rust::ExprInner::Ref(b, expr))
            },

            rust::ExprInner::FunCall(_expr, _args) => {
                todo!()
          /*      let expr = type_checker(ctxt, expr, loc_ctxt);
                if let typed_rust::PostTypeInner::Fun(args_typ, output) = &expr.typed.content {
                    if args.len() != args_typ.len() {
                        panic!("not allowed")
                    }

                    let mut args2 = Vec::new();
                    for (expr, arg) in args.into_iter().zip(args_typ.iter()) {
                        let expr = type_checker(ctxt, expr, loc_ctxt);
                        if &expr.typed == arg {
                            args2.push(expr)
                        };
                    }
                    (*output.clone(),
                    typed_rust::ExprInner::FunCall(expr, args2))

                } else {
                    panic!("not allowed")
                }*/
            },

            rust::ExprInner::MacroCall(_, _) => panic!("should not occur"),

            rust::ExprInner::Method(_, _, _) => todo!(),

            rust::ExprInner::Bloc(_bloc) => todo!(),

            rust::ExprInner::Deref(_expr) => todo!(),

            rust::ExprInner::If(e1, e2, e3) => {
                let expr1 = type_checker(ctxt, e1, loc_ctxt);
                if expr1.typed.content != typed_rust::PostTypeInner::BuiltIn(BuiltinType::Bool) {
                    panic!("Type error")
                }
                let expr2 = type_checker(ctxt, e2, loc_ctxt);
                let expr3 = type_checker(ctxt, e3, loc_ctxt);

                if let Some(typ) = biggest_compatible(&expr2.typed, &expr3.typed) {
                    (typ, typed_rust::ExprInner::If(expr1, expr2, expr3))
                } else {
                    panic!("incompatible types")
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


pub fn type_block(bloc : rust::Bloc,
    ctxt : &context::GlobalContext,
    loc_ctxt : &mut context::LocalContext,
    output : &typed_rust::PostType)
-> typed_rust::Bloc {
    loc_ctxt.add_layer();
    let mut last_typ = typed_rust::PostType::unit();
    let mut content = Vec::new();
    for instr in bloc.content {
        match instr {
            rust::Instr::Expr(expr) => {
                let expr = type_checker(ctxt, expr, loc_ctxt);
                last_typ = expr.typed.clone();
                content.push(typed_rust::Instr::Expr(expr));
                todo!();
            },
            rust::Instr::Binding(b, ident, expr) => {
                let mut expr = type_checker(ctxt, expr, loc_ctxt);
                if b && !expr.typed.mutable {
                    panic!("not allowed")
                }
                expr.typed.mutable = b;
                loc_ctxt.add_var(&ident, &expr.typed);
                last_typ = typed_rust::PostType::unit();
                content.push(typed_rust::Instr::Binding(b, ident, expr));
                todo!();
            },
            _ => todo!()
        }
    };

    let out_values =
        match loc_ctxt.pop_layer() {
            | Some(map) => map,
            | None => panic!("should not happen"),
        };
    typed_rust::Bloc{ content, values : out_values, }
}
