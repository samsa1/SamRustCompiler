use crate::ast::common::*;
use crate::ast::typed_rust::*;

fn is_ref(
    mutable: bool,
    mut top_expr: Expr,
    context: &mut Vec<Instr>,
    counter: &mut IdCounter,
    name1: &str,
    name2: &str,
) -> Expr {
    match *top_expr.content {
        ExprInner::Set(_, _)
//        | ExprInner::Constructor(_, _)
        | ExprInner::Tuple(_)
        | ExprInner::Print(_)
        | ExprInner::PrintPtr(_)
//        | ExprInner::Vec(_)
        | ExprInner::Bloc(_)
        | ExprInner::BuildStruct(_, _)
        | ExprInner::FunCall(_, _)
        | ExprInner::FunCallPath(_, _)
        | ExprInner::If(_, _, _)
        | ExprInner::Coercion(_, _, _)
        | ExprInner::BinOp(_, _, _)
        | ExprInner::UnaOp(_, _)
        | ExprInner::Return(_)
        | ExprInner::While(_, _)
        | ExprInner::Ref(_, _) => {
            top_expr = rewrite_expr(top_expr, context, counter, name1, name2);
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
        ExprInner::Proj(expr, proj) => Expr {
            content: Box::new(ExprInner::Proj(
                is_ref(mutable, expr, context, counter, name1, name2),
                proj,
            )),
            ..top_expr
        },
        ExprInner::VarPath(path) => Expr {
            content: Box::new(ExprInner::VarPath(path.rewrite_base(name1, name2))),
            ..top_expr
        },
        ExprInner::Bool(_) | ExprInner::Deref(_) | ExprInner::Int(_)
         | ExprInner::String(_) | ExprInner::Var(_) => top_expr,

    }
}

fn rewrite_expr(
    top_expr: Expr,
    context: &mut Vec<Instr>,
    counter: &mut IdCounter,
    name1: &str,
    name2: &str,
) -> Expr {
    match *top_expr.content {
        ExprInner::Set(expr1, expr2) => Expr {
            content: Box::new(ExprInner::Set(
                rewrite_expr(expr1, context, counter, name1, name2),
                rewrite_expr(expr2, context, counter, name1, name2),
            )),
            ..top_expr
        },

        //        ExprInner::Constructor(_, _) => todo!(),
        ExprInner::Tuple(exprs) => Expr {
            content: Box::new(ExprInner::Tuple(
                exprs
                    .into_iter()
                    .map(|e| is_ref(false, e, context, counter, name1, name2))
                    .collect(),
            )),
            ..top_expr
        },

        ExprInner::Print(str) => Expr {
            content: Box::new(ExprInner::Print(str)),
            ..top_expr
        },

        ExprInner::PrintPtr(expr) => {
            println!("print_ptr -> {:?}", expr);
            let expr = is_ref(false, expr, context, counter, name1, name2);
            println!("          -> {:?}", expr);
            Expr {
                content: Box::new(ExprInner::PrintPtr(expr)),
                ..top_expr
            }
        }

        ExprInner::Ref(mutable, expr) => Expr {
            content: Box::new(ExprInner::Ref(
                mutable,
                is_ref(mutable, expr, context, counter, name1, name2),
            )),
            ..top_expr
        },

        ExprInner::Int(_) | ExprInner::Bool(_) | ExprInner::Var(_) | ExprInner::String(_) => {
            top_expr
        }
        ExprInner::VarPath(path) => Expr {
            content: Box::new(ExprInner::VarPath(path.rewrite_base(name1, name2))),
            ..top_expr
        },

        ExprInner::BuildStruct(name, exprs) => Expr {
            content: Box::new(ExprInner::BuildStruct(
                name,
                exprs
                    .into_iter()
                    .map(|(n, e)| (n, is_ref(false, e, context, counter, name1, name2)))
                    .collect(),
            )),
            ..top_expr
        },

        ExprInner::If(expr, bloc1, bloc2) => Expr {
            content: Box::new(ExprInner::If(
                rewrite_expr(expr, context, counter, name1, name2),
                rewrite_bloc(bloc1, counter, name1, name2),
                rewrite_bloc(bloc2, counter, name1, name2),
            )),
            ..top_expr
        },

        ExprInner::Bloc(bloc) => Expr {
            content: Box::new(ExprInner::Bloc(rewrite_bloc(bloc, counter, name1, name2))),
            ..top_expr
        },

        ExprInner::FunCall(name, exprs) => Expr {
            content: Box::new(ExprInner::FunCall(
                name,
                exprs
                    .into_iter()
                    .map(|e| is_ref(false, e, context, counter, name1, name2))
                    .collect(),
            )),
            ..top_expr
        },

        ExprInner::FunCallPath(path, exprs) => Expr {
            content: Box::new(ExprInner::FunCallPath(
                path.rewrite_base(name1, name2),
                exprs
                    .into_iter()
                    .map(|e| is_ref(false, e, context, counter, name1, name2))
                    .collect(),
            )),
            ..top_expr
        },

        ExprInner::Deref(expr) => Expr {
            content: Box::new(ExprInner::Deref(rewrite_expr(
                expr, context, counter, name1, name2,
            ))),
            ..top_expr
        },

        ExprInner::Proj(expr, proj) => Expr {
            content: Box::new(ExprInner::Proj(
                is_ref(false, expr, context, counter, name1, name2),
                proj,
            )),
            ..top_expr
        },

        ExprInner::BinOp(binop, expr1, expr2) => Expr {
            content: Box::new(ExprInner::BinOp(
                binop,
                is_ref(false, expr1, context, counter, name1, name2),
                is_ref(false, expr2, context, counter, name1, name2),
            )),
            ..top_expr
        },

        ExprInner::UnaOp(unaop, expr) => Expr {
            content: Box::new(ExprInner::UnaOp(
                unaop,
                is_ref(false, expr, context, counter, name1, name2),
            )),
            ..top_expr
        },

        ExprInner::Coercion(expr, typ1, typ2) => Expr {
            content: Box::new(ExprInner::Coercion(
                is_ref(false, expr, context, counter, name1, name2),
                typ1,
                typ2,
            )),
            ..top_expr
        },

        ExprInner::Return(None) => top_expr,
        ExprInner::Return(Some(expr)) => Expr {
            content: Box::new(ExprInner::Return(Some(rewrite_expr(
                expr, context, counter, name1, name2,
            )))),
            ..top_expr
        },
        ExprInner::While(expr, bloc) => Expr {
            content: Box::new(ExprInner::While(
                expr,
                rewrite_bloc(bloc, counter, name1, name2),
            )),
            ..top_expr
        },
    }
}

fn rewrite_bloc(bloc: Bloc, counter: &mut IdCounter, name1: &str, name2: &str) -> Bloc {
    let mut vec_out = Vec::new();
    for instr in bloc.content.into_iter() {
        match instr {
            Instr::Expr(drop, expr) => {
                let expr = rewrite_expr(expr, &mut vec_out, counter, name1, name2);
                vec_out.push(Instr::Expr(drop, expr))
            }
            Instr::Binding(mutable, name, expr) => {
                let expr = rewrite_expr(expr, &mut vec_out, counter, name1, name2);
                vec_out.push(Instr::Binding(mutable, name, expr))
            }
        }
    }
    Bloc {
        content: vec_out,
        ..bloc
    }
}

fn rewrite_fun(mut fun_decl: DeclFun, name1: &str, name2: &str) -> DeclFun {
    DeclFun {
        name: fun_decl.name.rewrite_base(name1, name2),
        content: rewrite_bloc(fun_decl.content, &mut fun_decl.id_counter, name1, name2),
        ..fun_decl
    }
}

fn rewrite_struct(struct_decl: DeclStruct, name1: &str, name2: &str) -> DeclStruct {
    todo!()
}

fn rewrite_file(file: File, name1: &str, name2: &str) -> File {
    File {
        funs: file
            .funs
            .into_iter()
            .map(|fd| rewrite_fun(fd, name1, name2))
            .collect(),
        structs: file
            .structs
            .into_iter()
            .map(|sd| rewrite_struct(sd, name1, name2))
            .collect(),
        ..file
    }
}

pub fn rewrite(
    m: crate::frontend::Module<File>,
    name1: &str,
    name2: &str,
) -> crate::frontend::Module<File> {
    let content = rewrite_file(m.content, name1, name2);
    let submodules = m
        .submodules
        .into_iter()
        .map(|(k, (b, m_inner))| (k, (b, rewrite(m_inner, name1, name2))))
        .collect();
    crate::frontend::Module::build(content, submodules)
}
