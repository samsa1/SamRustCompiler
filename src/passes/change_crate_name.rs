use crate::ast::typed_rust::*;

fn rewrite_expr(top_expr: Expr, name1: &str, name2: &str) -> Expr {
    match *top_expr.content {
        ExprInner::Set(expr1, expr2) => Expr {
            content: Box::new(ExprInner::Set(
                rewrite_expr(expr1, name1, name2),
                rewrite_expr(expr2, name1, name2),
            )),
            ..top_expr
        },

        ExprInner::Tuple(exprs, pad) => Expr {
            content: Box::new(ExprInner::Tuple(
                exprs
                    .into_iter()
                    .map(|e| rewrite_expr(e, name1, name2))
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
            let expr = rewrite_expr(expr, name1, name2);
            println!("          -> {:?}", expr);
            Expr {
                content: Box::new(ExprInner::PrintPtr(expr)),
                ..top_expr
            }
        }

        ExprInner::Ref(mutable, expr) => Expr {
            content: Box::new(ExprInner::Ref(mutable, rewrite_expr(expr, name1, name2))),
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
                    .map(|(n, e)| (n, rewrite_expr(e, name1, name2)))
                    .collect(),
            )),
            ..top_expr
        },

        ExprInner::Constructor(name, exprs) => Expr {
            content: Box::new(ExprInner::Constructor(
                name,
                exprs
                    .into_iter()
                    .map(|e| rewrite_expr(e, name1, name2))
                    .collect(),
            )),
            ..top_expr
        },

        ExprInner::If(expr, bloc1, bloc2) => Expr {
            content: Box::new(ExprInner::If(
                rewrite_expr(expr, name1, name2),
                rewrite_bloc(bloc1, name1, name2),
                rewrite_bloc(bloc2, name1, name2),
            )),
            ..top_expr
        },

        ExprInner::Bloc(bloc) => Expr {
            content: Box::new(ExprInner::Bloc(rewrite_bloc(bloc, name1, name2))),
            ..top_expr
        },

        ExprInner::FunCall(name, exprs) => Expr {
            content: Box::new(ExprInner::FunCall(
                name,
                exprs
                    .into_iter()
                    .map(|e| rewrite_expr(e, name1, name2))
                    .collect(),
            )),
            ..top_expr
        },

        ExprInner::FunCallPath(specialisation, path, exprs) => Expr {
            content: Box::new(ExprInner::FunCallPath(
                specialisation
                    .into_iter()
                    .map(|typ| rewrite_type(typ, name1, name2))
                    .collect(),
                path.rewrite_base(name1, name2),
                exprs
                    .into_iter()
                    .map(|e| rewrite_expr(e, name1, name2))
                    .collect(),
            )),
            ..top_expr
        },

        ExprInner::Deref(expr) => Expr {
            content: Box::new(ExprInner::Deref(rewrite_expr(expr, name1, name2))),
            ..top_expr
        },

        ExprInner::Proj(expr, proj) => Expr {
            content: Box::new(ExprInner::Proj(rewrite_expr(expr, name1, name2), proj)),
            ..top_expr
        },

        ExprInner::BinOp(binop, expr1, expr2) => Expr {
            content: Box::new(ExprInner::BinOp(
                binop,
                rewrite_expr(expr1, name1, name2),
                rewrite_expr(expr2, name1, name2),
            )),
            ..top_expr
        },

        ExprInner::UnaOp(unaop, expr) => Expr {
            content: Box::new(ExprInner::UnaOp(unaop, rewrite_expr(expr, name1, name2))),
            ..top_expr
        },

        ExprInner::Coercion(expr, typ1, typ2) => Expr {
            content: Box::new(ExprInner::Coercion(
                rewrite_expr(expr, name1, name2),
                typ1,
                typ2,
            )),
            ..top_expr
        },

        ExprInner::Return(None) => top_expr,
        ExprInner::Return(Some(expr)) => Expr {
            content: Box::new(ExprInner::Return(Some(rewrite_expr(expr, name1, name2)))),
            ..top_expr
        },
        ExprInner::While(expr, bloc) => Expr {
            content: Box::new(ExprInner::While(expr, rewrite_bloc(bloc, name1, name2))),
            ..top_expr
        },

        ExprInner::PatternMatching(expr, patterns, fall) => Expr {
            content: Box::new(ExprInner::PatternMatching(
                rewrite_expr(expr, name1, name2),
                patterns
                    .into_iter()
                    .map(|patt| rewrite_patt(patt, name1, name2))
                    .collect(),
                fall.map(|(b, id, bloc)| (b, id, rewrite_bloc(bloc, name1, name2))),
            )),
            ..top_expr
        },
    }
}

fn rewrite_patt(patt: Pattern, name1: &str, name2: &str) -> Pattern {
    Pattern {
        constructor: patt.constructor.rewrite_base(name1, name2),
        constructor_id: patt.constructor_id,
        arguments: patt.arguments,
        guard: patt.guard.map(|expr| rewrite_expr(expr, name1, name2)),
        bloc: rewrite_bloc(patt.bloc, name1, name2),
    }
}

fn rewrite_bloc(bloc: Bloc, name1: &str, name2: &str) -> Bloc {
    let mut vec_out = Vec::new();
    for instr in bloc.content.into_iter() {
        match instr {
            Instr::Expr(drop, expr) => {
                let expr = rewrite_expr(expr, name1, name2);
                vec_out.push(Instr::Expr(drop, expr))
            }
            Instr::Binding(mutable, name, expr) => {
                let expr = rewrite_expr(expr, name1, name2);
                vec_out.push(Instr::Binding(mutable, name, expr))
            }
        }
    }
    Bloc {
        content: vec_out,
        ..bloc
    }
}

pub fn rewrite_type(typ: PostType, name1: &str, name2: &str) -> PostType {
    let content = match typ.content {
        PostTypeInner::BuiltIn(b) => PostTypeInner::BuiltIn(b),
        PostTypeInner::Struct(path, types) => PostTypeInner::Struct(
            path.rewrite_base(name1, name2),
            types
                .into_iter()
                .map(|typ| rewrite_type(typ, name1, name2))
                .collect(),
        ),
        PostTypeInner::Enum(path, types) => PostTypeInner::Enum(
            path.rewrite_base(name1, name2),
            types
                .into_iter()
                .map(|typ| rewrite_type(typ, name1, name2))
                .collect(),
        ),
        PostTypeInner::Box(typ) => PostTypeInner::Box(Box::new(rewrite_type(*typ, name1, name2))),
        PostTypeInner::Ref(b, typ) => PostTypeInner::Ref(b, typ),
        PostTypeInner::Tuple(types) => PostTypeInner::Tuple(
            types
                .into_iter()
                .map(|typ| rewrite_type(typ, name1, name2))
                .collect(),
        ),
        PostTypeInner::FreeType(t) => PostTypeInner::FreeType(t),
        PostTypeInner::Fun(free, types, typ) => PostTypeInner::Fun(
            free,
            types
                .into_iter()
                .map(|typ| rewrite_type(typ, name1, name2))
                .collect(),
            Box::new(rewrite_type(*typ, name1, name2)),
        ),
        PostTypeInner::Diverge => PostTypeInner::Diverge,
        PostTypeInner::String => PostTypeInner::String,
    };
    PostType { content }
}

fn rewrite_fun(fun_decl: DeclFun, name1: &str, name2: &str) -> DeclFun {
    DeclFun {
        name: fun_decl.name.rewrite_base(name1, name2),
        content: rewrite_bloc(fun_decl.content, name1, name2),
        ..fun_decl
    }
}

fn rewrite_struct(_: DeclStruct, _: &str, _: &str) -> DeclStruct {
    todo!()
}

fn rewrite_file(file: File, name1: &str, name2: &str) -> File {
    File {
        funs: file
            .funs
            .into_iter()
            .map(|fd| rewrite_fun(fd, name1, name2))
            .collect(),
        // structs: file
        //     .structs
        //     .into_iter()
        //     .map(|sd| rewrite_struct(sd, name1, name2))
        //     .collect(),
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
