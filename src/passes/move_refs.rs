use crate::ast::rust::*;
use crate::ast::common::*;

fn is_ref(mutable : bool, mut top_expr : Expr, context : &mut Vec<Instr>, counter : &mut IdCounter) -> Expr {
    match *top_expr.content {
        ExprInner::Bool(_) | ExprInner::Bloc(_)
        | ExprInner::BuildStruct(_, _)
        | ExprInner::FunCall(_, _) | ExprInner::If(_, _, _)
        | ExprInner::Int(_) | ExprInner::BinaryOp(_, _, _)
        | ExprInner::Ref(_, _) | ExprInner::UnaryOp(_, _)
        | ExprInner::MacroCall(_, _)
        | ExprInner::Tuple(_) | ExprInner::Array(_)
            => {
            top_expr = rewrite_expr(top_expr, context, counter);
            let name = counter.new_name();
            let id = Ident::new_from(name, top_expr.loc.start(), top_expr.loc.end());
            context.push(Instr::Binding(mutable, id.clone(), Expr {
                    loc : top_expr.loc,
                    typed : top_expr.typed.clone(),
                    content : top_expr.content,
            }));
            top_expr.content = Box::new(ExprInner::Var(id));
            top_expr
        }
        ExprInner::Var(_) => top_expr,
        ExprInner::Proj(expr, proj) => {
            Expr {
                content : Box::new(ExprInner::Proj(is_ref(mutable, expr, context, counter), proj)),
                ..top_expr
            }
        },
        ExprInner::Deref(_) => top_expr,
        ExprInner::String(_) => top_expr,

        ExprInner::Method(expr, name, args) => todo!(),

        ExprInner::Index(expr1, expr2) => {
            let expr1 = is_ref(true, expr1, context, counter);
            let expr2 = rewrite_expr(expr2, context, counter);
            Expr {
                content : Box::new(ExprInner::Index(expr1, expr2)),
                ..top_expr
            }
        },

        ExprInner::Parenthesis(expr) => is_ref(mutable, expr, context, counter),

    }
}

fn rewrite_expr(top_expr : Expr, context : &mut Vec<Instr>, counter : &mut IdCounter) -> Expr {
    match *top_expr.content {
        ExprInner::Method(expr, name, args) => {
            Expr {
                content : Box::new(ExprInner::Method(
                    is_ref(true, expr, context, counter),
                    name,
                    args.into_iter().map(|e| rewrite_expr(e, context, counter)).collect()
                )),
                ..top_expr
            }
        },

        ExprInner::Array(vec) => {
            Expr {
                content : Box::new(ExprInner::Array(vec.into_iter().map(|e| rewrite_expr(e, context, counter)).collect())),
                ..top_expr
            }
        },

        ExprInner::BinaryOp(op, expr1, expr2) => {
            Expr {
                content : Box::new(ExprInner::BinaryOp(op, rewrite_expr(expr1, context, counter), rewrite_expr(expr2, context, counter))),
                ..top_expr
            }
        },

        ExprInner::UnaryOp(op, expr) => {
            Expr {
                content : Box::new(ExprInner::UnaryOp(op, rewrite_expr(expr, context, counter))),
                ..top_expr
            }
        },

        ExprInner::MacroCall(name, exprs) => {
            Expr {
                content : Box::new(ExprInner::MacroCall(name,
                    exprs.into_iter().map(|e| rewrite_expr(e, context, counter)).collect()
                    )),
                ..top_expr
            }
        },

        ExprInner::Index(expr1, expr2) => {
            let expr1 = is_ref(true, expr1, context, counter);
            let expr2 = rewrite_expr(expr2, context, counter);
            Expr {
                content : Box::new(ExprInner::Index(expr1, expr2)),
                ..top_expr
            }
        },

        ExprInner::Parenthesis(expr) => rewrite_expr(expr, context, counter),

        ExprInner::Ref(mutable, expr) => {
            Expr {
                content : Box::new(ExprInner::Ref(mutable, is_ref(mutable, expr, context, counter))),
                ..top_expr
            }
        },

        ExprInner::Int(_) | ExprInner::Bool(_) | ExprInner::Var(_)
        | ExprInner::String(_) => top_expr,

        ExprInner::BuildStruct(name, exprs) => {
            Expr {
                content : Box::new(ExprInner::BuildStruct(name,
                    exprs.into_iter().map(|(n, e)| (n, rewrite_expr(e, context, counter))).collect()
                    )),
                ..top_expr
            }
        },

        ExprInner::If(expr, bloc1, bloc2) => {
            Expr {
                content : Box::new(ExprInner::If(
                    rewrite_expr(expr, context, counter),
                    rewrite_bloc(bloc1, counter),
                    rewrite_bloc(bloc2, counter))),
                ..top_expr
            }
        },

        ExprInner::Bloc(bloc) => {
            Expr {
                content : Box::new(ExprInner::Bloc(rewrite_bloc(bloc, counter))),
                ..top_expr
            }
        },

        ExprInner::Tuple(exprs) => {
            Expr {
                content : Box::new(ExprInner::Tuple(
                    exprs.into_iter().map(|e| rewrite_expr(e, context, counter)).collect()
                    )),
                ..top_expr
            }
        },

        ExprInner::FunCall(name, exprs) => {
            Expr {
                content : Box::new(ExprInner::FunCall(name,
                    exprs.into_iter().map(|e| rewrite_expr(e, context, counter)).collect()
                    )),
                ..top_expr
            }
        },

        ExprInner::Deref(expr) => {
            Expr {
                content : Box::new(ExprInner::Deref(rewrite_expr(expr, context, counter))),
                ..top_expr
            }
        },

        ExprInner::Proj(expr, proj) => {
            Expr {
                content : Box::new(ExprInner::Proj(is_ref(false, expr, context, counter), proj)),
                ..top_expr
            }
        },

    }
}

fn rewrite_bloc(bloc : Bloc, counter : &mut IdCounter) -> Bloc {
    let mut vec_out = Vec::new();
    for instr in bloc.content.into_iter() {
        match instr {
            Instr::Expr(drop, expr) => {
                let expr = rewrite_expr(expr, &mut vec_out, counter);
                vec_out.push(Instr::Expr(drop, expr))
            },
            Instr::Return(None) => vec_out.push(Instr::Return(None)),
            Instr::Return(Some(expr)) => {
                let expr = rewrite_expr(expr, &mut vec_out, counter);
                vec_out.push(Instr::Return(Some(expr)))
            },
            Instr::While(expr, bloc) => {
                let expr = rewrite_expr(expr, &mut vec_out, counter);
                let bloc = rewrite_bloc(bloc, counter);
                vec_out.push(Instr::While(expr, bloc));
            },
            Instr::Binding(mutable, name, expr) => {
                let expr = rewrite_expr(expr, &mut vec_out, counter);
                vec_out.push(Instr::Binding(mutable, name, expr))
            },
        }
    }
    Bloc {
        content : vec_out,
        ..bloc
    }
}

fn rewrite_fun(mut fun_decl : DeclFun) -> DeclFun {
    DeclFun {
        content : rewrite_bloc(fun_decl.content, &mut fun_decl.id_counter),
        ..fun_decl
    }
}

pub fn rewrite_decl(decl : Decl) -> Decl {
    match decl {
        Decl::Struct(_) => decl,
        Decl::Fun(decl_fun) => Decl::Fun(rewrite_fun(decl_fun)),
    }
}

pub fn rewrite_file(file : File) -> File {
    File {
        content : file.content.into_iter().map(|decl| rewrite_decl(decl)).collect(),
        ..file
    }
}