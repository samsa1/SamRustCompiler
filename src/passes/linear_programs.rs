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
        | ExprInner::Tuple(_, _)
        | ExprInner::Print(_)
        | ExprInner::PrintPtr(_)
        | ExprInner::Bloc(_)
        | ExprInner::BuildStruct(_, _)
        | ExprInner::Constructor(_, _)
        | ExprInner::FunCall(_, _)
        | ExprInner::FunCallPath(_, _, _)
        | ExprInner::TraitFun(_, _, _, _)
        | ExprInner::If(_, _, _)
        | ExprInner::Coercion(_, _, _)
        | ExprInner::PatternMatching(_, _, _)
        | ExprInner::BinOp(_, _, _)
        | ExprInner::UnaOp(_, _)
        | ExprInner::Return(_)
        | ExprInner::While(_, _)
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
        ExprInner::Var(_) | ExprInner::VarPath(_) => top_expr,
        ExprInner::Proj(expr, proj) => Expr {
            content: Box::new(ExprInner::Proj(
                is_ref(mutable, expr, context, counter),
                proj,
            )),
            ..top_expr
        },
        ExprInner::Int(_) => top_expr,
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

        ExprInner::Tuple(exprs, pad) => Expr {
            content: Box::new(ExprInner::Tuple(
                exprs
                    .into_iter()
                    .map(|e| is_ref(false, e, context, counter))
                    .collect(),
                pad,
            )),
            ..top_expr
        },

        ExprInner::Print(str) => Expr {
            content: Box::new(ExprInner::Print(str)),
            ..top_expr
        },

        ExprInner::PrintPtr(expr) => {
            println!("print_ptr -> {:?}", expr);
            let expr = is_ref(false, expr, context, counter);
            println!("          -> {:?}", expr);
            Expr {
                content: Box::new(ExprInner::PrintPtr(expr)),
                ..top_expr
            }
        }

        ExprInner::Ref(mutable, expr) => Expr {
            content: Box::new(ExprInner::Ref(
                mutable,
                is_ref(mutable, expr, context, counter),
            )),
            ..top_expr
        },

        ExprInner::Int(_)
        | ExprInner::Bool(_)
        | ExprInner::Var(_)
        | ExprInner::VarPath(_)
        | ExprInner::String(_) => top_expr,

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
        ExprInner::Constructor(name, exprs) => Expr {
            content: Box::new(ExprInner::Constructor(
                name,
                exprs
                    .into_iter()
                    .map(|e| is_ref(false, e, context, counter))
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

        ExprInner::FunCallPath(specialisation, name, exprs) => Expr {
            content: Box::new(ExprInner::FunCallPath(
                specialisation,
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

        ExprInner::PatternMatching(expr, patterns, fall) => {
            let expr = rewrite_expr(expr, context, counter);
            let name = counter.new_name();
            let id = Ident::new_from(name, top_expr.loc.start(), top_expr.loc.end());
            let new_expr = Expr {
                content: Box::new(ExprInner::Var(id.clone())),
                loc: expr.loc,
                typed: expr.typed.clone(),
            };
            context.push(Instr::Binding(false, id, expr));
            Expr {
                content: Box::new(ExprInner::PatternMatching(
                    new_expr,
                    patterns
                        .into_iter()
                        .map(|patt| rewrite_pattern(patt, counter))
                        .collect(),
                    fall.map(|(b, id, bloc)| (b, id, rewrite_bloc(bloc, counter))),
                )),
                ..top_expr
            }
        }

        ExprInner::TraitFun(_, _, _, _) => panic!("Should have already been handled"),

        ExprInner::BinOp(binop, expr1, expr2) => Expr {
            content: Box::new(ExprInner::BinOp(
                binop,
                rewrite_expr(expr1, context, counter),
                rewrite_expr(expr2, context, counter),
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

        ExprInner::Coercion(expr, typ1, typ2) => Expr {
            content: Box::new(ExprInner::Coercion(
                is_ref(false, expr, context, counter),
                typ1,
                typ2,
            )),
            ..top_expr
        },

        ExprInner::Return(None) => top_expr,
        ExprInner::Return(Some(expr)) => Expr {
            content: Box::new(ExprInner::Return(Some(rewrite_expr(
                expr, context, counter,
            )))),
            ..top_expr
        },
        ExprInner::While(expr, bloc) => Expr {
            content: Box::new(ExprInner::While(expr, rewrite_bloc(bloc, counter))),
            ..top_expr
        },
    }
}

fn rewrite_pattern(patt: Pattern, counter: &mut IdCounter) -> Pattern {
    Pattern {
        constructor_id: patt.constructor_id,
        constructor: patt.constructor,
        arguments: patt.arguments,
        guard: patt.guard,
        bloc: rewrite_bloc(patt.bloc, counter),
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

fn rewrite_file(file: File) -> File {
    File {
        funs: file.funs.into_iter().map(rewrite_fun).collect(),
        ..file
    }
}

pub fn rewrite(m: crate::frontend::Module<File>) -> crate::frontend::Module<File> {
    let content = rewrite_file(m.content);
    let submodules = m
        .submodules
        .into_iter()
        .map(|(k, (b, m_inner))| (k, (b, rewrite(m_inner))))
        .collect();
    crate::frontend::Module::build(content, submodules)
}
