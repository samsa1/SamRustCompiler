use crate::ast::common::*;
use crate::ast::typed_rust::*;

fn is_ref(
    mutable: bool,
    mut top_expr: Expr,
    context: &mut Vec<Instr>,
    counter: &mut IdCounter,
) -> Expr {
    match *top_expr.content {
        ExprInner::Set(_, _)
//        | ExprInner::Constructor(_, _)
        | ExprInner::Tuple(_)
        | ExprInner::Print(_)
//        | ExprInner::Vec(_)
        | ExprInner::Bloc(_)
        | ExprInner::BuildStruct(_, _)
        | ExprInner::FunCall(_, _)
        | ExprInner::If(_, _, _)
        | ExprInner::Int(_)
        | ExprInner::BinOp(_, _, _)
        | ExprInner::UnaOp(_, _)
        | ExprInner::Ref(_, _) => {
            top_expr = rewrite_expr(top_expr, context, counter);
            let name = counter.new_name();
            let id = Ident::new_from(name, top_expr.loc.start(), top_expr.loc.end());
            context.push(Instr::Binding(
                mutable,
                id.clone(),
                Expr {
                    loc: top_expr.loc,
                    typed: top_expr.typed.clone(),
                    content: top_expr.content,
                },
            ));
            top_expr.content = Box::new(ExprInner::Var(id));
            top_expr
        }
        ExprInner::Var(_) => top_expr,
        ExprInner::Proj(expr, proj) => Expr {
            content: Box::new(ExprInner::Proj(
                is_ref(mutable, expr, context, counter),
                proj,
            )),
            ..top_expr
        },
        ExprInner::Bool(_) => top_expr,
        ExprInner::Deref(_) => top_expr,
        ExprInner::String(_) => top_expr,

    }
}

fn rewrite_expr(top_expr: Expr, context: &mut Vec<Instr>, counter: &mut IdCounter) -> Expr {
    match *top_expr.content {
        ExprInner::Set(expr1, expr2) => Expr {
            content: Box::new(ExprInner::Set(
                rewrite_expr(expr1, context, counter),
                rewrite_expr(expr2, context, counter),
            )),
            ..top_expr
        },

        //        ExprInner::Constructor(_, _) => todo!(),
        ExprInner::Tuple(exprs) => Expr {
            content: Box::new(ExprInner::Tuple(
                exprs
                    .into_iter()
                    .map(|e| is_ref(false, e, context, counter))
                    .collect(),
            )),
            ..top_expr
        },

        /*        ExprInner::Vec(exprs) => Expr {
            content: Box::new(ExprInner::Vec(
                exprs
                    .into_iter()
                    .map(|e| is_ref(false, e, context, counter))
                    .collect(),
            )),
            ..top_expr
        },*/
        ExprInner::Print(str) => Expr {
            content: Box::new(ExprInner::Print(str)),
            ..top_expr
        },

        ExprInner::Ref(mutable, expr) => Expr {
            content: Box::new(ExprInner::Ref(
                mutable,
                is_ref(mutable, expr, context, counter),
            )),
            ..top_expr
        },

        ExprInner::Int(_) | ExprInner::Bool(_) | ExprInner::Var(_) | ExprInner::String(_) => {
            top_expr
        }

        ExprInner::BuildStruct(name, exprs) => Expr {
            content: Box::new(ExprInner::BuildStruct(
                name,
                exprs
                    .into_iter()
                    .map(|(n, e)| (n, is_ref(false, e, context, counter)))
                    .collect(),
            )),
            ..top_expr
        },

        ExprInner::If(expr, bloc1, bloc2) => Expr {
            content: Box::new(ExprInner::If(
                rewrite_expr(expr, context, counter),
                rewrite_bloc(bloc1, counter),
                rewrite_bloc(bloc2, counter),
            )),
            ..top_expr
        },

        ExprInner::Bloc(bloc) => Expr {
            content: Box::new(ExprInner::Bloc(rewrite_bloc(bloc, counter))),
            ..top_expr
        },

        ExprInner::Tuple(exprs) => Expr {
            content: Box::new(ExprInner::Tuple(
                exprs
                    .into_iter()
                    .map(|e| rewrite_expr(e, context, counter))
                    .collect(),
            )),
            ..top_expr
        },

        ExprInner::FunCall(name, exprs) => Expr {
            content: Box::new(ExprInner::FunCall(
                name,
                exprs
                    .into_iter()
                    .map(|e| is_ref(false, e, context, counter))
                    .collect(),
            )),
            ..top_expr
        },

        ExprInner::Deref(expr) => Expr {
            content: Box::new(ExprInner::Deref(rewrite_expr(expr, context, counter))),
            ..top_expr
        },

        ExprInner::Proj(expr, proj) => Expr {
            content: Box::new(ExprInner::Proj(is_ref(false, expr, context, counter), proj)),
            ..top_expr
        },

        ExprInner::BinOp(binop, expr1, expr2) => Expr {
            content: Box::new(ExprInner::BinOp(
                binop,
                is_ref(false, expr1, context, counter),
                is_ref(false, expr2, context, counter),
            )),
            ..top_expr
        },

        ExprInner::UnaOp(unaop, expr) => Expr {
            content: Box::new(ExprInner::UnaOp(
                unaop,
                is_ref(false, expr, context, counter),
            )),
            ..top_expr
        },
    }
}

fn rewrite_bloc(bloc: Bloc, counter: &mut IdCounter) -> Bloc {
    let mut vec_out = Vec::new();
    for instr in bloc.content.into_iter() {
        match instr {
            Instr::Expr(drop, expr) => {
                let expr = rewrite_expr(expr, &mut vec_out, counter);
                vec_out.push(Instr::Expr(drop, expr))
            }
            Instr::Return(None) => vec_out.push(Instr::Return(None)),
            Instr::Return(Some(expr)) => {
                let expr = rewrite_expr(expr, &mut vec_out, counter);
                vec_out.push(Instr::Return(Some(expr)))
            }
            Instr::While(expr, bloc) => {
                let expr = rewrite_expr(expr, &mut vec_out, counter);
                let bloc = rewrite_bloc(bloc, counter);
                vec_out.push(Instr::While(expr, bloc));
            }
            Instr::Binding(mutable, name, expr) => {
                let expr = rewrite_expr(expr, &mut vec_out, counter);
                vec_out.push(Instr::Binding(mutable, name, expr))
            }
        }
    }
    Bloc {
        content: vec_out,
        ..bloc
    }
}

fn rewrite_fun(mut fun_decl: DeclFun) -> DeclFun {
    DeclFun {
        content: rewrite_bloc(fun_decl.content, &mut fun_decl.id_counter),
        ..fun_decl
    }
}

pub fn rewrite_file(file: File) -> File {
    File {
        funs: file.funs.into_iter().map(|fun| rewrite_fun(fun)).collect(),
        ..file
    }
}
