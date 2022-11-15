use crate::ast::common::*;
use crate::ast::rust::*;

fn is_ref(
    mutable: bool,
    mut top_expr: Expr,
    context: &mut Vec<Instr>,
    counter: &mut IdCounter,
) -> Expr {
    match *top_expr.content {
        ExprInner::Bool(_)
        | ExprInner::Bloc(_)
        | ExprInner::BuildStruct(_, _)
        | ExprInner::BuildStructPath(_, _)
        | ExprInner::FunCall(_, _, _)
        | ExprInner::FunCallPath(_, _, _)
        | ExprInner::If(_, _, _)
        | ExprInner::Int(_, _)
        | ExprInner::BinaryOp(_, _, _)
        | ExprInner::Ref(_, _)
        | ExprInner::UnaryOp(_, _)
        | ExprInner::MacroCall(_, _)
        | ExprInner::Tuple(_)
        | ExprInner::Array(_)
        | ExprInner::Return(_)
        | ExprInner::While(_, _)
        | ExprInner::Coercion(_, _)
        | ExprInner::Method(_, _, _) => {
            top_expr = rewrite_expr(top_expr, context, counter);
            let name = counter.new_name();
            let id = Ident::new_from(name, top_expr.loc.start(), top_expr.loc.end());
            context.push(Instr {
                loc: Location::default(),
                content: InstrInner::Binding(
                    mutable,
                    id.clone(),
                    None,
                    Expr {
                        loc: top_expr.loc,
                        typed: top_expr.typed.clone(),
                        content: top_expr.content,
                    },
                ),
            });
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
        ExprInner::Deref(_) => top_expr,
        ExprInner::String(_) => top_expr,

        ExprInner::Index(expr1, expr2) => {
            let expr1 = is_ref(true, expr1, context, counter);
            let expr2 = rewrite_expr(expr2, context, counter);
            Expr {
                content: Box::new(ExprInner::Index(expr1, expr2)),
                ..top_expr
            }
        }

        ExprInner::Parenthesis(expr) => is_ref(mutable, expr, context, counter),
    }
}

fn rewrite_expr(top_expr: Expr, context: &mut Vec<Instr>, counter: &mut IdCounter) -> Expr {
    match *top_expr.content {
        ExprInner::Method(expr, name, args) => Expr {
            content: Box::new(ExprInner::Method(
                is_ref(true, expr, context, counter),
                name,
                args.into_iter()
                    .map(|e| rewrite_expr(e, context, counter))
                    .collect(),
            )),
            ..top_expr
        },

        ExprInner::Array(vec) => Expr {
            content: Box::new(ExprInner::Array(
                vec.into_iter()
                    .map(|e| rewrite_expr(e, context, counter))
                    .collect(),
            )),
            ..top_expr
        },

        ExprInner::BinaryOp(op, expr1, expr2) => Expr {
            content: Box::new(ExprInner::BinaryOp(
                op,
                rewrite_expr(expr1, context, counter),
                rewrite_expr(expr2, context, counter),
            )),
            ..top_expr
        },

        ExprInner::UnaryOp(op, expr) => Expr {
            content: Box::new(ExprInner::UnaryOp(op, rewrite_expr(expr, context, counter))),
            ..top_expr
        },

        ExprInner::MacroCall(name, exprs) => Expr {
            content: Box::new(ExprInner::MacroCall(
                name,
                exprs
                    .into_iter()
                    .map(|e| rewrite_expr(e, context, counter))
                    .collect(),
            )),
            ..top_expr
        },

        ExprInner::Index(expr1, expr2) => {
            let expr1 = is_ref(true, expr1, context, counter);
            let expr2 = rewrite_expr(expr2, context, counter);
            Expr {
                content: Box::new(ExprInner::Index(expr1, expr2)),
                ..top_expr
            }
        }

        ExprInner::Parenthesis(expr) => rewrite_expr(expr, context, counter),

        ExprInner::Ref(mutable, expr) => Expr {
            content: Box::new(ExprInner::Ref(
                mutable,
                is_ref(mutable, expr, context, counter),
            )),
            ..top_expr
        },

        ExprInner::Int(_, _) | ExprInner::Bool(_) | ExprInner::Var(_) | ExprInner::VarPath(_) | ExprInner::String(_) => {
            top_expr
        }

        ExprInner::BuildStruct(name, exprs) => Expr {
            content: Box::new(ExprInner::BuildStruct(
                name,
                exprs
                    .into_iter()
                    .map(|(n, e)| (n, rewrite_expr(e, context, counter)))
                    .collect(),
            )),
            ..top_expr
        },

        ExprInner::BuildStructPath(name, exprs) => Expr {
            content: Box::new(ExprInner::BuildStructPath(
                name,
                exprs
                    .into_iter()
                    .map(|(n, e)| (n, rewrite_expr(e, context, counter)))
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

        ExprInner::FunCall(args, name, exprs) => Expr {
            content: Box::new(ExprInner::FunCall(
                args,
                name,
                exprs
                    .into_iter()
                    .map(|e| rewrite_expr(e, context, counter))
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
                    .map(|e| rewrite_expr(e, context, counter))
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

        ExprInner::Coercion(expr, typ) => Expr {
            content: Box::new(ExprInner::Coercion(
                rewrite_expr(expr, context, counter),
                typ,
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
            content: Box::new(ExprInner::While(
                rewrite_expr(expr, context, counter),
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
                let expr = rewrite_expr(expr, &mut vec_out, counter);
                InstrInner::Expr(drop, expr)
            }
            InstrInner::Binding(mutable, name, typ, expr) => {
                let expr = rewrite_expr(expr, &mut vec_out, counter);
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
        Decl::Const(_) | Decl::Struct(_) => decl,
        Decl::Fun(decl_fun) => Decl::Fun(rewrite_fun(decl_fun)),
        Decl::Impl(decl_impl) => Decl::Impl(DeclImpl {
            content: decl_impl.content.into_iter().map(rewrite_fun).collect(),
            ..decl_impl
        }),
    }
}

fn rewrite_file(file: File) -> File {
    File {
        content: file.content.into_iter().map(rewrite_decl).collect(),
        ..file
    }
}

pub fn rewrite(m : crate::frontend::Module<File>) -> crate::frontend::Module<File> {
    let content = rewrite_file(m.content);
    let submodules = m.submodules.into_iter().map(|(k, (b, m_inner))| (k, (b, rewrite(m_inner)))).collect();
    crate::frontend::Module::build(content, submodules)
}
