use crate::ast::{rust, typed_rust};
use crate::ast::common::{BuildinType, Sizes, Ident};

mod context;

fn compatible_types(type1 : &Option<rust::PreType>, type2 : typed_rust::PostType) -> typed_rust::PostType {
    match type1 {
        None => type2,
        Some(type1) => type2
    }
}

fn biggest_compatible(typ1 : &typed_rust::PostType, typ2 : &typed_rust::PostType) -> Option<typed_rust::PostType> {
    if typ1 == typ2 {
        Some(typ1.clone())
    } else {
        None
    }
}

pub fn is_type_int(type_name : &Option<rust::PreType>) -> Option<BuildinType> {
    if let Some(typ) = type_name {
        match &typ.content {
            | rust::PreTypeInner::IdentParametrized(_, _) => None,
            | rust::PreTypeInner::Ref(_) => None,
            | rust::PreTypeInner::Ident(s) =>
                match s.get_content() {
                    "u64" => Some(BuildinType::Int(false, Sizes::S64)),
                    "i64" => Some(BuildinType::Int(true, Sizes::S64)),
                    "u32" => Some(BuildinType::Int(false, Sizes::S32)),
                    "i32" => Some(BuildinType::Int(true, Sizes::S32)),
                    "usize" => Some(BuildinType::Int(false, Sizes::SUsize)),
                    "isize" => Some(BuildinType::Int(true, Sizes::SUsize)),
                    _ => None
                },
            | _ => todo!()
        }
    } else {
        None
    }
}

pub fn test_borrowing(typ : &typed_rust::PostType, b : bool) {
    if b && !typ.mutable {
        panic!("can't borrow mut")
    }

    match typ.content {
        typed_rust::PostTypeInner::Ref(_) => panic!("not allowed"),
        _ => ()
    }
}

pub fn type_block(bloc : rust::Bloc,
        ctxt : &context::GlobalContext,
        loc_ctxt : &mut context::LocalContext)
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
            },
            _ => todo!()
        }
    };

    let out_values =
        match loc_ctxt.pop_layer() {
            | Some(map) => map,
            | None => panic!("should not happen"),
        };
    typed_rust::Bloc{ content, values : out_values }
}

pub fn type_checker(ctxt : &context::GlobalContext, expr : rust::Expr, loc_ctxt : &mut context::LocalContext) -> typed_rust::Expr {
    let (found_type, content) = 
        match *expr.content {
            rust::ExprInner::Bool(b) => {
                (typed_rust::PostTypeInner::BuildIn(BuildinType::Bool).to_nonmut(), typed_rust::ExprInner::Bool(b))
            },

            rust::ExprInner::Int(i) => {
                if let Some(typ) = is_type_int(&expr.typed) {
                    (typed_rust::PostTypeInner::BuildIn(typ).to_nonmut(), typed_rust::ExprInner::Int(i))
                } else {
                    (typed_rust::PostTypeInner::BuildIn(BuildinType::Int(true, Sizes::S32)).to_nonmut(), typed_rust::ExprInner::Int(i))
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

            rust::ExprInner::FunCall(expr, args) => {
                let expr = type_checker(ctxt, expr, loc_ctxt);
                if let typed_rust::PostTypeInner::Fun(args_typ, output) = &expr.typed.content {
                    if args.len() != args_typ.len() {
                        panic!("not allowed")
                    }

                    let mut args2 = Vec::new();
                    let mut i = 0;
                    for expr in args {
                        let expr = type_checker(ctxt, expr, loc_ctxt);
                        if &expr.typed == &args_typ[i] {
                            args2.push(expr)
                        };
                        i += 1;
                    }
                    (*output.clone(),
                    typed_rust::ExprInner::FunCall(expr, args2))

                } else {
                    panic!("not allowed")
                }
            },

            rust::ExprInner::MacroCall(_, _) => panic!("should not occur"),

            rust::ExprInner::Method(_, _, _) => todo!(),

            rust::ExprInner::Bloc(bloc) => todo!(),

            rust::ExprInner::Deref(expr) => todo!(),

            rust::ExprInner::If(e1, e2, e3) => {
                let expr1 = type_checker(ctxt, e1, loc_ctxt);
                if expr1.typed.content != typed_rust::PostTypeInner::BuildIn(BuildinType::Bool) {
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
        };
    typed_rust::Expr {
        content : Box::new(content),
        typed : compatible_types(&expr.typed, found_type),
        loc : expr.loc,
    }
}