use crate::ast::common::*;
use crate::ast::typed_rust::*;
use crate::typing::context::ModuleInterface;

fn build_proj(expr: Expr, id: usize, typed: PostType) -> Expr {
    Expr {
        content: Box::new(ExprInner::Proj(expr, Projector::Int(id))),
        loc: Location::default(),
        typed,
    }
}

fn rewrite_expr(expr: Expr, context: &ModuleInterface) -> Expr {
    let expr_inner = match *expr.content {
        ExprInner::BinOp(op, expr1, expr2) => ExprInner::BinOp(
            op,
            rewrite_expr(expr1, context),
            rewrite_expr(expr2, context),
        ),
        ExprInner::Bloc(bloc) => ExprInner::Bloc(rewrite_bloc(bloc, context)),
        ExprInner::Bool(b) => ExprInner::Bool(b),
        ExprInner::BuildStruct(path, exprs) => ExprInner::BuildStruct(
            path,
            exprs
                .into_iter()
                .map(|(id, expr)| (id, rewrite_expr(expr, context)))
                .collect(),
        ),
        ExprInner::Coercion(expr, typ1, typ2) => {
            ExprInner::Coercion(rewrite_expr(expr, context), typ1, typ2)
        }
        ExprInner::Constructor(mut path, exprs) => {
            let name = match path.pop() {
                Some(NamePath::Name(id)) => id,
                _ => panic!("ICE"),
            };
            let (_, enum_info) = context.get_enum(&path, true).unwrap();
            let (id, cons_size) = enum_info.get_cons_id(&name).unwrap();
            let mut exprs2 = vec![Expr {
                content: Box::new(ExprInner::Int(id)),
                loc: Location::default(),
                typed: PostType {
                    content: PostTypeInner::BuiltIn(crate::config::CONSTRUCTOR_PARAM),
                },
            }];
            for expr in exprs {
                exprs2.push(rewrite_expr(expr, context))
            }
            ExprInner::Tuple(
                exprs2,
                enum_info.get_size() - cons_size - crate::config::CONSTRUCTOR_SIZE.to_byte_size(),
            )
        }
        ExprInner::Deref(expr) => ExprInner::Deref(rewrite_expr(expr, context)),
        ExprInner::FunCall(name, exprs) => ExprInner::FunCall(
            name,
            exprs
                .into_iter()
                .map(|expr| rewrite_expr(expr, context))
                .collect(),
        ),
        ExprInner::FunCallPath(specialisation, path, exprs) => ExprInner::FunCallPath(
            specialisation,
            path,
            exprs
                .into_iter()
                .map(|expr| rewrite_expr(expr, context))
                .collect(),
        ),
        ExprInner::Int(i) => ExprInner::Int(i),
        ExprInner::If(expr, bloc1, bloc2) => ExprInner::If(
            rewrite_expr(expr, context),
            rewrite_bloc(bloc1, context),
            rewrite_bloc(bloc2, context),
        ),
        ExprInner::PatternMatching(id_expr, patterns, fall) => {
            match &*id_expr.content {
                ExprInner::Var(_) => (),
                _ => panic!("ICE, linear_program pass has not done it's job"),
            };
            let reference = match &id_expr.typed.content {
                PostTypeInner::Ref(b, _) => Some(*b),
                _ => None,
            };
            let loc = id_expr.loc;
            let mut last_expr = match fall {
                None => {
                    let zero = Expr {
                        content: Box::new(ExprInner::Int(0)),
                        typed: PostType {
                            content: PostTypeInner::BuiltIn(BuiltinType::Int(true, Sizes::S64)),
                        },
                        loc: Location::default(),
                    };
                    Expr {
                        content: Box::new(ExprInner::BinOp(
                            TypedBinop::Div(true, Sizes::S64),
                            zero.clone(),
                            zero,
                        )),
                        typed: PostType {
                            content: PostTypeInner::Diverge,
                        },
                        loc,
                    }
                }
                Some((mutable, id2, bloc)) => {
                    let typ = bloc.last_type.clone();
                    let bloc_expr = Expr {
                        typed: typ.clone(),
                        loc,
                        content: Box::new(ExprInner::Bloc(bloc)),
                    };
                    let bloc = Bloc {
                        content: vec![
                            Instr::Binding(mutable, id2, id_expr.clone()),
                            Instr::Expr(ComputedValue::Keep, bloc_expr),
                        ],
                        last_type: typ.clone(),
                    };
                    Expr {
                        content: Box::new(ExprInner::Bloc(bloc)),
                        loc,
                        typed: typ.clone(),
                    }
                }
            };
            for pattern in patterns.into_iter().rev() {
                let int_typ = PostType {
                    content: PostTypeInner::BuiltIn(crate::config::CONSTRUCTOR_PARAM),
                };
                let mut vec = vec![int_typ.clone()];
                for (_, _, typ) in &pattern.arguments {
                    vec.push(typ.clone())
                }
                let typed = PostType {
                    content: PostTypeInner::Tuple(vec),
                };
                let typed = match reference {
                    None => typed,
                    Some(b) => PostType {
                        content: PostTypeInner::Ref(b, Box::new(typed)),
                    },
                };
                let id_expr = Expr {
                    content: id_expr.content.clone(),
                    loc: id_expr.loc,
                    typed,
                };

                let check = match pattern.guard {
                    None => Expr {
                        content: Box::new(ExprInner::BinOp(
                            TypedBinop::Eq(crate::config::CONSTRUCTOR_SIZE),
                            build_proj(id_expr.clone(), 0, int_typ.clone()),
                            Expr {
                                content: Box::new(ExprInner::Int(pattern.constructor_id)),
                                loc,
                                typed: int_typ,
                            },
                        )),
                        loc,
                        typed: PostType::bool(),
                    },
                    Some(_) => panic!("Guard not implemented"),
                };
                let typed = pattern.bloc.last_type.clone();
                let mut content = Vec::new();
                for (i, (mutable, id, typ)) in pattern.arguments.into_iter().enumerate() {
                    let expr = build_proj(id_expr.clone(), i + 1, typ);
                    let expr = match reference {
                        None => expr,
                        Some(b) => Expr {
                            typed: PostType {
                                content: PostTypeInner::Ref(b, Box::new(expr.typed.clone())),
                            },
                            content: Box::new(ExprInner::Ref(b, expr)),
                            loc,
                        },
                    };

                    content.push(Instr::Binding(mutable, id, expr))
                }
                content.push(Instr::Expr(
                    ComputedValue::Keep,
                    Expr {
                        content: Box::new(ExprInner::Bloc(pattern.bloc)),
                        loc,
                        typed: typed.clone(),
                    },
                ));
                let bloc = Bloc {
                    content,
                    last_type: typed.clone(),
                };
                last_expr = Expr {
                    content: Box::new(ExprInner::If(
                        check,
                        bloc,
                        Bloc {
                            content: vec![Instr::Expr(ComputedValue::Keep, last_expr)],
                            last_type: typed.clone(),
                        },
                    )),
                    loc,
                    typed,
                }
            }
            return last_expr;
        }
        ExprInner::Proj(expr, proj) => ExprInner::Proj(rewrite_expr(expr, context), proj),
        ExprInner::Print(str) => ExprInner::Print(str),
        ExprInner::PrintPtr(expr) => ExprInner::PrintPtr(rewrite_expr(expr, context)),
        ExprInner::Ref(b, expr) => ExprInner::Ref(b, rewrite_expr(expr, context)),
        ExprInner::Return(opt) => ExprInner::Return(opt.map(|expr| rewrite_expr(expr, context))),
        ExprInner::Set(expr1, expr2) => {
            ExprInner::Set(rewrite_expr(expr1, context), rewrite_expr(expr2, context))
        }
        ExprInner::String(str) => ExprInner::String(str),
        ExprInner::Tuple(exprs, pad) => ExprInner::Tuple(
            exprs
                .into_iter()
                .map(|expr| rewrite_expr(expr, context))
                .collect(),
            pad,
        ),
        ExprInner::UnaOp(op, expr) => ExprInner::UnaOp(op, rewrite_expr(expr, context)),
        ExprInner::Var(v) => ExprInner::Var(v),
        ExprInner::VarPath(path) => ExprInner::VarPath(path),
        ExprInner::While(expr, bloc) => {
            ExprInner::While(rewrite_expr(expr, context), rewrite_bloc(bloc, context))
        }
        ExprInner::TraitFun(_, _, _, _) => panic!("Should be handled before hand"),
    };
    Expr {
        content: Box::new(expr_inner),
        ..expr
    }
}

fn rewrite_bloc(bloc: Bloc, context: &ModuleInterface) -> Bloc {
    let mut content = Vec::new();
    for instr in bloc.content {
        match instr {
            Instr::Expr(keep, expr) => content.push(Instr::Expr(keep, rewrite_expr(expr, context))),
            Instr::Binding(id, mutable, expr) => {
                content.push(Instr::Binding(id, mutable, rewrite_expr(expr, context)))
            }
        }
    }
    Bloc { content, ..bloc }
}

fn rewrite_fun(fun_decl: DeclFun, context: &ModuleInterface) -> DeclFun {
    DeclFun {
        content: rewrite_bloc(fun_decl.content, context),
        ..fun_decl
    }
}

fn rewrite_file(file: File, context: &ModuleInterface) -> File {
    File {
        funs: file
            .funs
            .into_iter()
            .map(|fun_decl| rewrite_fun(fun_decl, context))
            .collect(),
        ..file
    }
}

pub fn rewrite(
    m: crate::frontend::Module<File>,
    context: &ModuleInterface,
) -> crate::frontend::Module<File> {
    let content = rewrite_file(m.content, context);
    let submodules = m
        .submodules
        .into_iter()
        .map(|(k, (b, m_inner))| (k, (b, rewrite(m_inner, context))))
        .collect();
    crate::frontend::Module::build(content, submodules)
}
