use crate::ast::common::*;
use crate::ast::typed_rust::*;
use crate::typing::context::ModuleInterface;

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
            ExprInner::Tuple(exprs2, enum_info.get_size() - cons_size)
        }
        ExprInner::Deref(expr) => ExprInner::Deref(rewrite_expr(expr, context)),
        ExprInner::FunCall(name, exprs) => ExprInner::FunCall(
            name,
            exprs
                .into_iter()
                .map(|expr| rewrite_expr(expr, context))
                .collect(),
        ),
        ExprInner::FunCallPath(path, exprs) => ExprInner::FunCallPath(
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
        ExprInner::PatternMatching(_, _, _) => todo!(),
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

fn rewrite_fun(mut fun_decl: DeclFun, context: &ModuleInterface) -> DeclFun {
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
