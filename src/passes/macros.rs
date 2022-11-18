use crate::ast::common::*;
use crate::ast::rust::*;

fn rewrite_patt(patt: Pattern, counter: &mut IdCounter) -> Pattern {
    Pattern {
        bloc: rewrite_bloc(patt.bloc, counter),
        guard: patt.guard.map(|top_expr| rewrite_expr(top_expr, counter)),
        ..patt
    }
}

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
            vec.push(Instr {
                content: InstrInner::Binding(
                    true,
                    Ident::new(&vec_name, top_expr.loc),
                    None,
                    Expr {
                        content: Box::new(ExprInner::FunCallPath(
                            vec![],
                            Path::new(
                                vec![
                                    NamePath::Name(Ident::new("std", top_expr.loc)),
                                    NamePath::Name(Ident::new("vec", top_expr.loc)),
                                    NamePath::Name(Ident::new("Vec", top_expr.loc)),
                                    NamePath::Name(Ident::new("new", top_expr.loc)),
                                ],
                                top_expr.loc,
                            ),
                            Vec::new(),
                        )),
                        loc: top_expr.loc,
                        typed: None,
                    },
                ),
                loc: Location::default(),
            });
            for expr in exprs.into_iter() {
                let expr = rewrite_expr(expr, counter);
                vec.push(Instr {
                    loc: Location::default(),
                    content: InstrInner::Expr(
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
                    ),
                })
            }
            vec.push(Instr {
                content: InstrInner::Expr(ComputedValue::Keep, Expr::var(&vec_name, top_expr.loc)),
                loc: top_expr.loc,
            });
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

        ExprInner::Int(_, _)
        | ExprInner::Bool(_)
        | ExprInner::Var(_)
        | ExprInner::VarPath(_)
        | ExprInner::String(_) => top_expr,

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

        ExprInner::BuildStructPath(name, exprs) => Expr {
            content: Box::new(ExprInner::BuildStructPath(
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
        ExprInner::Constructor(path, exprs) => Expr {
            content: Box::new(ExprInner::Constructor(
                path,
                exprs
                    .into_iter()
                    .map(|e| rewrite_expr(e, counter))
                    .collect(),
            )),
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

        ExprInner::FunCallPath(args, name, exprs) => Expr {
            content: Box::new(ExprInner::FunCallPath(
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
        ExprInner::PatternMatching(expr, rows, opt) => {
            let expr = rewrite_expr(expr, counter);
            let opt = opt.map(|(b, id, bloc)| (b, id, rewrite_bloc(bloc, counter)));
            let rows = rows
                .into_iter()
                .map(|patt| rewrite_patt(patt, counter))
                .collect();
            Expr {
                content: Box::new(ExprInner::PatternMatching(expr, rows, opt)),
                ..top_expr
            }
        }
        ExprInner::Coercion(expr, typ) => Expr {
            content: Box::new(ExprInner::Coercion(rewrite_expr(expr, counter), typ)),
            ..top_expr
        },

        ExprInner::Return(None) => top_expr,
        ExprInner::Return(Some(expr)) => Expr {
            content: Box::new(ExprInner::Return(Some(rewrite_expr(expr, counter)))),
            ..top_expr
        },
        ExprInner::While(expr, bloc) => Expr {
            content: Box::new(ExprInner::While(
                rewrite_expr(expr, counter),
                rewrite_bloc(bloc, counter),
            )),
            ..top_expr
        },
    }
}

fn rewrite_bloc(bloc: Bloc, counter: &mut IdCounter) -> Bloc {
    let mut vec_out = Vec::new();
    for instr in bloc.content.into_iter() {
        let content = match instr.content {
            InstrInner::Expr(drop, expr) => {
                let expr = rewrite_expr(expr, counter);
                InstrInner::Expr(drop, expr)
            }
            InstrInner::Binding(mutable, name, typ, expr) => {
                let expr = rewrite_expr(expr, counter);
                InstrInner::Binding(mutable, name, typ, expr)
            }
        };
        vec_out.push(Instr { content, ..instr })
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
        Decl::Enum(_) | Decl::Struct(_) => decl,
        Decl::Fun(decl_fun) => Decl::Fun(rewrite_fun(decl_fun)),
        Decl::Impl(decl_impl) => Decl::Impl(DeclImpl {
            content: decl_impl.content.into_iter().map(rewrite_fun).collect(),
            ..decl_impl
        }),
        Decl::Const(decl_const) => Decl::Const(DeclConst {
            expr: rewrite_expr(decl_const.expr, &mut IdCounter::new()),
            ..decl_const
        }),
    }
}

fn rewrite_file(file: File) -> File {
    File {
        content: file.content.into_iter().map(rewrite_decl).collect(),
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
