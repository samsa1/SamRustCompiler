use crate::ast::common::*;
use crate::ast::rust::*;
use crate::typing::errors::TypeError;
use std::collections::HashMap;

struct GiveUniqueId {
    id: usize,
    names: Vec<HashMap<String, String>>,
}

impl GiveUniqueId {
    fn new() -> Self {
        Self {
            id: 0,
            names: vec![],
        }
    }

    fn add_layer(&mut self) {
        self.names.push(HashMap::new())
    }

    fn pop_layer(&mut self) {
        self.names.pop();
    }

    fn name_already_in_layer(&self, name: &Ident) -> bool {
        self.names.last().unwrap().get(name.get_content()).is_some()
    }

    fn add_name(&mut self, name: Ident) -> Ident {
        let id = self.id;
        let new_name = Ident::new(&format!("{}@{}", name.get_content(), id), name.get_loc());
        self.names
            .last_mut()
            .unwrap()
            .insert(name.content(), new_name.get_content().to_string());
        self.id = id + 1;
        new_name
    }

    fn get_name(&self, name: &str) -> Option<&str> {
        for id in self.names.iter().rev() {
            match id.get(name) {
                None => (),
                Some(v) => return Some(v),
            }
        }
        None
    }
}

fn rewrite_expr(top_expr: Expr, counter: &mut GiveUniqueId) -> Expr {
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

        ExprInner::MacroCall(name, mut exprs)
            if name.get_content() == "print_ptr" && exprs.len() == 1 =>
        {
            eprintln!("Didn't unfold a print macro");
            Expr {
                content: Box::new(ExprInner::MacroCall(
                    name,
                    vec![rewrite_expr(exprs.pop().unwrap(), counter)],
                )),
                loc: top_expr.loc,
                typed: top_expr.typed,
            }
        }

        ExprInner::MacroCall(_name, _exprs) => {
            todo!()
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

        ExprInner::Var(v) => Expr {
            content: Box::new(ExprInner::Var(match counter.get_name(v.get_content()) {
                None => v,
                Some(v2) => Ident::new(v2, v.get_loc()),
            })),
            ..top_expr
        },

        ExprInner::Int(_, _) | ExprInner::Bool(_) | ExprInner::String(_) => top_expr,

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

        ExprInner::FunCall(args, name, exprs) => {
            let name = match counter.get_name(name.get_content()) {
                None => name,
                Some(new_name) => Ident::new(new_name, name.get_loc()),
            };
            Expr {
                content: Box::new(ExprInner::FunCall(
                    args,
                    name,
                    exprs
                        .into_iter()
                        .map(|e| rewrite_expr(e, counter))
                        .collect(),
                )),
                ..top_expr
            }
        }

        ExprInner::Deref(expr) => Expr {
            content: Box::new(ExprInner::Deref(rewrite_expr(expr, counter))),
            ..top_expr
        },

        ExprInner::Proj(expr, proj) => Expr {
            content: Box::new(ExprInner::Proj(rewrite_expr(expr, counter), proj)),
            ..top_expr
        },
        ExprInner::Coercion(expr, typ) => Expr {
            content: Box::new(ExprInner::Coercion(rewrite_expr(expr, counter), typ)),
            ..top_expr
        },
    }
}

fn rewrite_bloc(bloc: Bloc, counter: &mut GiveUniqueId) -> Bloc {
    counter.add_layer();
    let mut vec_out = Vec::new();
    for instr in bloc.content.into_iter() {
        let content = match instr.content {
            InstrInner::Expr(drop, expr) => {
                let expr = rewrite_expr(expr, counter);
                InstrInner::Expr(drop, expr)
            }
            InstrInner::Return(None) => InstrInner::Return(None),
            InstrInner::Return(Some(expr)) => {
                let expr = rewrite_expr(expr, counter);
                InstrInner::Return(Some(expr))
            }
            InstrInner::While(expr, bloc) => {
                let expr = rewrite_expr(expr, counter);
                let bloc = rewrite_bloc(bloc, counter);
                InstrInner::While(expr, bloc)
            }
            InstrInner::Binding(mutable, name, expr) => {
                let expr = rewrite_expr(expr, counter);
                let name = counter.add_name(name);
                InstrInner::Binding(mutable, name, expr)
            }
        };
        vec_out.push(Instr { content, ..instr })
    }
    counter.pop_layer();
    Bloc {
        content: vec_out,
        ..bloc
    }
}

fn rewrite_fun(fun_decl: DeclFun) -> Result<DeclFun, Vec<TypeError>> {
    let mut giver = GiveUniqueId::new();
    giver.add_layer();
    let mut args = Vec::new();
    for (arg_name, b, t) in fun_decl.args {
        if giver.name_already_in_layer(&arg_name) {
            return Err(vec![TypeError::same_arg_name(
                fun_decl.name,
                arg_name.content(),
            )]);
        } else {
            args.push((giver.add_name(arg_name), b, t))
        }
    }
    Ok(DeclFun {
        content: rewrite_bloc(fun_decl.content, &mut giver),
        args,
        ..fun_decl
    })
}

fn rewrite_decl(decl: Decl) -> Result<Decl, Vec<TypeError>> {
    match decl {
        Decl::Struct(_) => Ok(decl),
        Decl::Impl(decl_impl) => {
            let mut content = Vec::new();
            for decl_fun in decl_impl.content {
                content.push(rewrite_fun(decl_fun)?)
            }
            Ok(Decl::Impl(DeclImpl {
                content,
                ..decl_impl
            }))
        }
        Decl::Fun(decl_fun) => Ok(Decl::Fun(rewrite_fun(decl_fun)?)),
    }
}

pub fn rewrite_file(file: File) -> File {
    let mut content = vec![];
    for decl in file.content {
        match rewrite_decl(decl) {
            Ok(decl) => content.push(decl),
            Err(errs) => {
                for err in errs.into_iter() {
                    err.report_error(&file.err_reporter);
                }
                std::process::exit(1);
            }
        }
    }
    File { content, ..file }
}
