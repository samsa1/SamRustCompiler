use crate::ast::common::*;
use crate::ast::rust::*;

fn rewrite_expr(top_expr: Expr, counter: &mut IdCounter) -> Expr {
    match *top_expr.content {
        ExprInner::Method(expr, name, args) => Expr {
            content: Box::new(ExprInner::Method(
                rewrite_expr(expr, counter),
                name,
                args.into_iter().map(|e| rewrite_expr(e, counter)).collect(),
            )),
            ..top_expr
        },

        ExprInner::Array(vec) => Expr {
            content: Box::new(ExprInner::Array(
                vec.into_iter().map(|e| rewrite_expr(e, counter)).collect(),
            )),
            ..top_expr
        },

        ExprInner::BinaryOp(op, expr1, expr2) => Expr {
            content: Box::new(ExprInner::BinaryOp(
                op,
                rewrite_expr(expr1, counter),
                rewrite_expr(expr2, counter),
            )),
            ..top_expr
        },

        ExprInner::UnaryOp(op, expr) => Expr {
            content: Box::new(ExprInner::UnaryOp(op, rewrite_expr(expr, counter))),
            ..top_expr
        },

        ExprInner::MacroCall(name, exprs) if name.get_content() == "print" => {
            eprintln!("Didn't unfold a print macro");
            Expr {
                content: Box::new(ExprInner::MacroCall(name, exprs)),
                loc: top_expr.loc,
                typed: top_expr.typed,
            }
        }

        ExprInner::MacroCall(name, exprs) if name.get_content() == "vec" => {
            let mut vec = Vec::new();
            let vec_name = counter.new_name();
            vec.push(Instr::Binding(
                true,
                Ident::new(&vec_name, top_expr.loc),
                Expr {
                    content: Box::new(ExprInner::FunCall(
                        vec![],
                        Ident::new("std::vec::Vec::new", top_expr.loc),
                        Vec::new(),
                    )),
                    loc: top_expr.loc,
                    typed: None,
                },
            ));
            for expr in exprs.into_iter() {
                let expr = rewrite_expr(expr, counter);
                vec.push(Instr::Expr(
                    ComputedValue::Drop,
                    Expr {
                        content: Box::new(ExprInner::Method(
                            Expr::var(&vec_name, top_expr.loc),
                            Ident::new("push", top_expr.loc),
                            vec![expr],
                        )),
                        loc: top_expr.loc,
                        typed: None,
                    },
                ))
            }
            vec.push(Instr::Expr(
                ComputedValue::Keep,
                Expr::var(&vec_name, top_expr.loc),
            ));
            Expr {
                content: Box::new(ExprInner::Bloc(Bloc {
                    content: vec,
                    loc: top_expr.loc,
                })),
                loc: top_expr.loc,
                typed: top_expr.typed,
            }
        }

        ExprInner::MacroCall(name, exprs) => {
            let exprs = exprs
                .into_iter()
                .map(|e| rewrite_expr(e, counter))
                .collect();

            Expr {
                content: Box::new(ExprInner::MacroCall(name, exprs)),
                ..top_expr
            }
            //            todo!()
        }

        ExprInner::Index(expr1, expr2) => {
            let expr1 = rewrite_expr(expr1, counter);
            let expr2 = rewrite_expr(expr2, counter);
            Expr {
                content: Box::new(ExprInner::Index(expr1, expr2)),
                ..top_expr
            }
        }

        ExprInner::Parenthesis(expr) => rewrite_expr(expr, counter),

        ExprInner::Ref(mutable, expr) => Expr {
            content: Box::new(ExprInner::Ref(mutable, rewrite_expr(expr, counter))),
            ..top_expr
        },

        ExprInner::Int(_, _) | ExprInner::Bool(_) | ExprInner::Var(_) | ExprInner::String(_) => {
            top_expr
        }

        ExprInner::BuildStruct(name, exprs) => Expr {
            content: Box::new(ExprInner::BuildStruct(
                name,
                exprs
                    .into_iter()
                    .map(|(n, e)| (n, rewrite_expr(e, counter)))
                    .collect(),
            )),
            ..top_expr
        },

        ExprInner::If(expr, bloc1, bloc2) => Expr {
            content: Box::new(ExprInner::If(
                rewrite_expr(expr, counter),
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
                    .map(|e| rewrite_expr(e, counter))
                    .collect(),
            )),
            ..top_expr
        },

        ExprInner::FunCall(args, name, exprs) => Expr {
            content: Box::new(ExprInner::FunCall(
                args,
                name,
                exprs
                    .into_iter()
                    .map(|e| rewrite_expr(e, counter))
                    .collect(),
            )),
            ..top_expr
        },

        ExprInner::Deref(expr) => Expr {
            content: Box::new(ExprInner::Deref(rewrite_expr(expr, counter))),
            ..top_expr
        },

        ExprInner::Proj(expr, proj) => Expr {
            content: Box::new(ExprInner::Proj(rewrite_expr(expr, counter), proj)),
            ..top_expr
        },
    }
}

fn rewrite_bloc(bloc: Bloc, counter: &mut IdCounter) -> Bloc {
    let mut vec_out = Vec::new();
    for instr in bloc.content.into_iter() {
        match instr {
            Instr::Expr(drop, expr) => {
                let expr = rewrite_expr(expr, counter);
                vec_out.push(Instr::Expr(drop, expr))
            }
            Instr::Return(None) => vec_out.push(Instr::Return(None)),
            Instr::Return(Some(expr)) => {
                let expr = rewrite_expr(expr, counter);
                vec_out.push(Instr::Return(Some(expr)))
            }
            Instr::While(expr, bloc) => {
                let expr = rewrite_expr(expr, counter);
                let bloc = rewrite_bloc(bloc, counter);
                vec_out.push(Instr::While(expr, bloc));
            }
            Instr::Binding(mutable, name, expr) => {
                let expr = rewrite_expr(expr, counter);
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

pub fn rewrite_decl(decl: Decl) -> Decl {
    match decl {
        Decl::Struct(_) => decl,
        Decl::Fun(decl_fun) => Decl::Fun(rewrite_fun(decl_fun)),
        Decl::Impl(decl_impl) => Decl::Impl(DeclImpl {
            content: decl_impl.content.into_iter().map(rewrite_fun).collect(),
            ..decl_impl
        }),
    }
}

pub fn rewrite_file(file: File) -> File {
    File {
        content: file.content.into_iter().map(rewrite_decl).collect(),
        ..file
    }
}
