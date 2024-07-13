use crate::ast::{common::*, typed_rust::*, operators::*};

fn rewrite_patt(patt: Pattern) -> Pattern {
    Pattern {
        bloc: rewrite_bloc(patt.bloc),
        guard: patt.guard.map(|expr| rewrite_expr(expr)),
        ..patt
    }
}

pub fn rewrite_expr(expr: Expr) -> Expr {
    let expr_inner = match *expr.content {
        ExprInner::BinOp(op, expr1, expr2) => {
            ExprInner::BinOp(op, rewrite_expr(expr1), rewrite_expr(expr2))
        }
        ExprInner::Bloc(bloc) => ExprInner::Bloc(rewrite_bloc(bloc)),
        ExprInner::Bool(b) => ExprInner::Bool(b),
        ExprInner::BuildStruct(path, exprs) => ExprInner::BuildStruct(
            path,
            exprs
                .into_iter()
                .map(|(id, expr)| (id, rewrite_expr(expr)))
                .collect(),
        ),
        ExprInner::Coercion(expr, typ1, typ2) => {
            ExprInner::Coercion(rewrite_expr(expr), typ1, typ2)
        }
        ExprInner::Constructor(path, exprs) => ExprInner::Constructor(
            path,
            exprs.into_iter().map(|expr| rewrite_expr(expr)).collect(),
        ),
        ExprInner::Deref(expr) => ExprInner::Deref(rewrite_expr(expr)),
        ExprInner::FunCall(name, exprs) => ExprInner::FunCall(
            name,
            exprs.into_iter().map(|expr| rewrite_expr(expr)).collect(),
        ),
        ExprInner::FunCallPath(specialisation, path, exprs) => ExprInner::FunCallPath(
            specialisation,
            path,
            exprs.into_iter().map(|expr| rewrite_expr(expr)).collect(),
        ),
        ExprInner::Int(i) => ExprInner::Int(i),
        ExprInner::If(expr, bloc1, bloc2) => {
            ExprInner::If(rewrite_expr(expr), rewrite_bloc(bloc1), rewrite_bloc(bloc2))
        }
        ExprInner::PatternMatching(expr, patterns, fall) => ExprInner::PatternMatching(
            rewrite_expr(expr),
            patterns
                .into_iter()
                .map(|patt| rewrite_patt(patt))
                .collect(),
            fall.map(|(b, id, bloc)| (b, id, rewrite_bloc(bloc))),
        ),
        ExprInner::Proj(expr, proj) => ExprInner::Proj(rewrite_expr(expr), proj),
        ExprInner::Print(str) => ExprInner::Print(str),
        ExprInner::PrintPtr(expr) => ExprInner::PrintPtr(rewrite_expr(expr)),
        ExprInner::Ref(b, expr) => ExprInner::Ref(b, rewrite_expr(expr)),
        ExprInner::Return(opt) => ExprInner::Return(opt.map(|expr| rewrite_expr(expr))),
        ExprInner::Set(expr1, expr2) => ExprInner::Set(rewrite_expr(expr1), rewrite_expr(expr2)),
        ExprInner::String(str) => ExprInner::String(str),
        ExprInner::Tuple(exprs, pad) => ExprInner::Tuple(
            exprs.into_iter().map(|expr| rewrite_expr(expr)).collect(),
            pad,
        ),
        ExprInner::UnaOp(op, expr) => ExprInner::UnaOp(op, rewrite_expr(expr)),
        ExprInner::Var(v) => ExprInner::Var(v),
        ExprInner::VarPath(path) => ExprInner::VarPath(path),
        ExprInner::While(expr, bloc) => ExprInner::While(rewrite_expr(expr), rewrite_bloc(bloc)),
        ExprInner::TraitFun(path, typ, fun_name, exprs) => {
            let mut exprs: Vec<_> = exprs.into_iter().map(|expr| rewrite_expr(expr)).collect();
            match &typ.content {
                PostTypeInner::BuiltIn(b) => {
                    if let Some(op) = path.is_op() {
                        match (op, exprs.len(), b) {
                            ("Add", 2, BuiltinType::Int(_, size)) => {
                                let expr2 = exprs.pop().unwrap();
                                let expr1 = exprs.pop().unwrap();
                                assert_eq!(fun_name, "add");
                                ExprInner::BinOp(TBinop::LArith(LArith::Add(*size)), expr1, expr2)
                            }
                            ("Mul", 2, BuiltinType::Int(signed, size)) => {
                                let expr2 = exprs.pop().unwrap();
                                let expr1 = exprs.pop().unwrap();
                                assert_eq!(fun_name, "mul");
                                ExprInner::BinOp(TBinop::HArith(HArith::new(HArithDesc::Mul, *signed, *size)), expr1, expr2)
                            }
                            ("Mod", 2, BuiltinType::Int(signed, size)) => {
                                let expr2 = exprs.pop().unwrap();
                                let expr1 = exprs.pop().unwrap();
                                assert_eq!(fun_name, "mod");
                                ExprInner::BinOp(TBinop::HArith(HArith::new(HArithDesc::Mod, *signed, *size)), expr1, expr2)
                            }
                            ("Sub", 2, BuiltinType::Int(_, size)) => {
                                let expr2 = exprs.pop().unwrap();
                                let expr1 = exprs.pop().unwrap();
                                assert_eq!(fun_name, "sub");
                                ExprInner::BinOp(TBinop::LArith(LArith::Sub(*size)), expr1, expr2)
                            }
                            ("Div", 2, BuiltinType::Int(signed, size)) => {
                                let expr2 = exprs.pop().unwrap();
                                let expr1 = exprs.pop().unwrap();
                                assert_eq!(fun_name, "div");
                                ExprInner::BinOp(TBinop::HArith(HArith::new(HArithDesc::Div, *signed, *size)), expr1, expr2)
                            }
                            ("Shl", 2, BuiltinType::Int(_, size)) => {
                                let expr2 = exprs.pop().unwrap();
                                let expr1 = exprs.pop().unwrap();
                                assert_eq!(fun_name, "shl");
                                ExprInner::BinOp(TBinop::Shl(*size), expr1, expr2)
                            }
                            ("Shr", 2, BuiltinType::Int(_, size)) => {
                                let expr2 = exprs.pop().unwrap();
                                let expr1 = exprs.pop().unwrap();
                                assert_eq!(fun_name, "shr");
                                ExprInner::BinOp(TBinop::Shr(*size), expr1, expr2)
                            }
                            ("PartialEq", 2, BuiltinType::Bool) => {
                                let expr2 = exprs.pop().unwrap();
                                let expr1 = exprs.pop().unwrap();
                                if fun_name == "eq" {
                                    ExprInner::BinOp(TBinop::Cmp(Cmp::eq(Sizes::S8)), expr1, expr2)
                                } else {
                                    assert_eq!(fun_name, "ne");
                                    ExprInner::BinOp(TBinop::Cmp(Cmp::neq(Sizes::S8)), expr1, expr2)
                                }
                            }
                            ("PartialEq", 2, BuiltinType::Int(_, size)) => {
                                let expr2 = exprs.pop().unwrap();
                                let expr1 = exprs.pop().unwrap();
                                if fun_name == "eq" {
                                    ExprInner::BinOp(TBinop::Cmp(Cmp::eq(*size)), expr1, expr2)
                                } else {
                                    assert_eq!(fun_name, "ne");
                                    ExprInner::BinOp(TBinop::Cmp(Cmp::neq(*size)), expr1, expr2)
                                }
                            }
                            ("PartialOrd", 2, BuiltinType::Int(signed, size)) => {
                                let expr2 = exprs.pop().unwrap();
                                let expr1 = exprs.pop().unwrap();
                                match fun_name.as_str() {
                                    "ge" => ExprInner::BinOp(
                                        TBinop::Cmp(Cmp::new(CmpDesc::GreaterEq, *signed, *size)),
                                        expr1,
                                        expr2,
                                    ),
                                    "gr" => ExprInner::BinOp(
                                        TBinop::Cmp(Cmp::new(CmpDesc::Greater, *signed, *size)),
                                        expr1,
                                        expr2,
                                    ),
                                    "le" => ExprInner::BinOp(
                                        TBinop::Cmp(Cmp::new(CmpDesc::LowerEq, *signed, *size)),
                                        expr1,
                                        expr2,
                                    ),
                                    "lo" => ExprInner::BinOp(
                                        TBinop::Cmp(Cmp::new(CmpDesc::Lower, *signed, *size)),
                                        expr1,
                                        expr2,
                                    ),
                                    _ => panic!("ICE"),
                                }
                            }
                            ("And", 2, BuiltinType::Bool) => {
                                let expr2 = exprs.pop().unwrap();
                                let expr1 = exprs.pop().unwrap();
                                ExprInner::BinOp(TBinop::Logic(Logic::LAnd), expr1, expr2)
                            }
                            ("Or", 2, BuiltinType::Bool) => {
                                let expr2 = exprs.pop().unwrap();
                                let expr1 = exprs.pop().unwrap();
                                ExprInner::BinOp(TBinop::Logic(Logic::LOr), expr1, expr2)
                            }
                            ("BitAnd", 2, BuiltinType::Int(_, size)) => {
                                let expr2 = exprs.pop().unwrap();
                                let expr1 = exprs.pop().unwrap();
                                assert_eq!(fun_name, "bit_and");
                                ExprInner::BinOp(TBinop::LArith(LArith::And(*size)), expr1, expr2)
                            }
                            ("BitOr", 2, BuiltinType::Int(_, size)) => {
                                let expr2 = exprs.pop().unwrap();
                                let expr1 = exprs.pop().unwrap();
                                assert_eq!(fun_name, "bit_or");
                                ExprInner::BinOp(TBinop::LArith(LArith::Or(*size)), expr1, expr2)
                            }
                            ("Neg", 1, BuiltinType::Int(_, size)) => {
                                let expr = exprs.pop().unwrap();
                                assert_eq!(fun_name, "neg");
                                ExprInner::UnaOp(TUnaop::Neg(*size), expr)
                            }
                            ("Not", 1, BuiltinType::Bool) => {
                                let expr = exprs.pop().unwrap();
                                assert_eq!(fun_name, "not");
                                ExprInner::UnaOp(TUnaop::Not(Sizes::S8), expr)
                            }
                            ("Add", 2, BuiltinType::Bool) => todo!(),
                            ("Mul", 2, BuiltinType::Bool) => todo!(),
                            ("Mod", 2, BuiltinType::Bool) => todo!(),
                            ("Sub", 2, BuiltinType::Bool) => todo!(),
                            ("Div", 2, BuiltinType::Bool) => todo!(),
                            ("Shl", 2, BuiltinType::Bool) => todo!(),
                            ("Shr", 2, BuiltinType::Bool) => todo!(),
                            ("PartialOrd", 2, BuiltinType::Bool) => todo!(),
                            ("And", 2, BuiltinType::Int(_, _)) => todo!(),
                            ("Or", 2, BuiltinType::Int(_, _)) => todo!(),
                            ("BitAnd", 2, BuiltinType::Bool) => todo!(),
                            ("Neg", 1, BuiltinType::Bool) => todo!(),
                            ("BitOr", 2, BuiltinType::Bool) => todo!(),
                            ("Not", 1, BuiltinType::Int(_, _)) => todo!(),
                            _ => panic!("ICE {} {}", op, exprs.len()),
                        }
                    } else {
                        panic!("Not handled {:?}", path)
                    }
                }
                _ => panic!("Not handled {:?} {:?}", path, typ),
            }
        }
    };
    Expr {
        content: Box::new(expr_inner),
        ..expr
    }
}

fn rewrite_bloc(bloc: Bloc) -> Bloc {
    let mut content = Vec::new();
    for instr in bloc.content {
        match instr {
            Instr::Expr(keep, expr) => content.push(Instr::Expr(keep, rewrite_expr(expr))),
            Instr::Binding(id, mutable, expr) => {
                content.push(Instr::Binding(id, mutable, rewrite_expr(expr)))
            }
        }
    }
    Bloc { content, ..bloc }
}

fn rewrite_fun(fun_decl: DeclFun) -> DeclFun {
    DeclFun {
        content: rewrite_bloc(fun_decl.content),
        ..fun_decl
    }
}

fn rewrite_file(file: File) -> File {
    File {
        funs: file
            .funs
            .into_iter()
            .map(|fun_decl| rewrite_fun(fun_decl))
            .collect(),
        ..file
    }
}

pub fn rewrite(code: crate::frontend::Module<File>) -> crate::frontend::Module<File> {
    let content = rewrite_file(code.content);
    let submodules = code
        .submodules
        .into_iter()
        .map(|(k, (b, m_inner))| (k, (b, rewrite(m_inner))))
        .collect();
    crate::frontend::Module::build(content, submodules)
}
