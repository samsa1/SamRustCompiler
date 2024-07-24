use super::pre_asm::{self as pasm, LoR};
use crate::ast::{
    common::{self, Sizes},
    low_level_repr::{self as llr, Pos, UnaOp, Value},
    operators::{LArith, Logic, TBinop},
};
use pasm::{Context, Reg};

// fn rewrite_bloc(ctxt : &mut Context)

fn compute_ref(ctxt: &mut Context, e: llr::Expr) -> Reg {
    if e.typed.is_ref() {
        rewrite_expr(ctxt, e)
    } else {
        rewrite_pointer(ctxt, e)
    }
}

fn rewrite_pointer(ctxt: &mut Context, e: llr::Expr) -> Reg {
    match *e.content {
        llr::ExprInner::BinOp(_, _, _)
        | llr::ExprInner::Bloc(_)
        | llr::ExprInner::BuildStruct(_, _)
        | llr::ExprInner::Coercion(_, _, _)
        | llr::ExprInner::FunCall(_, _)
        | llr::ExprInner::FunCallVar(_, _)
        | llr::ExprInner::If(_, _, _)
        | llr::ExprInner::Print(_)
        | llr::ExprInner::Ref(_)
        | llr::ExprInner::Set(_, _, _)
        | llr::ExprInner::Tuple(_, _)
        | llr::ExprInner::Return(_)
        | llr::ExprInner::Value(_)
        | llr::ExprInner::While(_, _)
        | llr::ExprInner::UnaOp(_, _) => panic!("ICE"),
        llr::ExprInner::Constant(_) => todo!(),
        llr::ExprInner::Deref(e) => rewrite_expr(ctxt, e),
        llr::ExprInner::FunVar(_) => todo!(),
        llr::ExprInner::Proj(e, offset) => {
            let r1 = compute_ref(ctxt, e);
            let r2 = ctxt.new_reg(8);
            let offset = Value::UInt(offset as u64, Sizes::SUsize);
            ctxt.current_bloc
                .larith(LArith::Add(Sizes::SUsize), r1, LoR::R(offset), r2);
            r2
        }
        llr::ExprInner::VarId(id) => {
            let rout = ctxt.new_reg(8);
            ctxt.current_bloc.leaq(LoR::R(ctxt.get_var(&id)), rout);
            rout
        }
    }
}

fn rewrite_binop(ctxt: &mut Context, op: TBinop, r1: Reg, r2: Reg, size: usize) -> Reg {
    match op {
        TBinop::Logic(_) => panic!("ICE"),
        TBinop::Shift(sop) => {
            let r3 = ctxt.new_reg(size);
            ctxt.current_bloc.shift(sop, r1, LoR::L(r2), r3);
            r3
        }
        TBinop::LArith(aop) => {
            let r3 = ctxt.new_reg(size);
            ctxt.current_bloc.larith(aop, r1, LoR::L(r2), r3);
            r3
        }
        TBinop::Cmp(cmp) => {
            let r3 = ctxt.new_reg(size);
            ctxt.current_bloc.cmp(cmp, r1, r2, r3);
            r3
        }
        TBinop::HArith(hop) => {
            let r3 = ctxt.new_reg(size);
            ctxt.current_bloc.harith(hop, r1, r2, r3);
            r3
        }
    }
}

fn rewrite_expr(ctxt: &mut Context, e: llr::Expr) -> Reg {
    match *e.content {
        llr::ExprInner::BinOp(op, e1, e2) => match op {
            TBinop::Logic(Logic::LAnd) => {
                let rout = ctxt.new_reg(1);

                let r1 = rewrite_expr(ctxt, e1);
                let bid_else = ctxt.cond(r1);

                let r2 = rewrite_expr(ctxt, e2);
                let bid_then = ctxt.cond_next(r2, bid_else);
                ctxt.current_bloc.set(Value::Bool(false), rout);
                let join_id = ctxt.finish_bloc(bid_then);
                ctxt.current_bloc.set(Value::Bool(true), rout);
                ctxt.join(join_id);
                rout
            }
            TBinop::Logic(Logic::LOr) => {
                let rout = ctxt.new_reg(1);

                let r1 = rewrite_expr(ctxt, e1);
                let bid_then = ctxt.cond_rev(r1);

                let r2 = rewrite_expr(ctxt, e2);
                ctxt.jcc(r2, bid_then.clone());
                ctxt.current_bloc.set(Value::Bool(false), rout);
                let join_id = ctxt.finish_bloc(bid_then);
                ctxt.current_bloc.set(Value::Bool(true), rout);
                ctxt.join(join_id);
                rout
            }
            op => {
                let r1 = rewrite_expr(ctxt, e1);
                let r2 = rewrite_expr(ctxt, e2);
                rewrite_binop(ctxt, op, r1, r2, e.size)
            }
        },
        llr::ExprInner::Bloc(b) => rewrite_bloc(ctxt, b),
        llr::ExprInner::BuildStruct(size, exprs) => {
            let mut regs = Vec::new();
            for (offset, expr) in exprs {
                regs.push((offset, rewrite_expr(ctxt, expr)))
            }
            let out = ctxt.new_reg(size);
            ctxt.current_bloc.merge_reg(regs, out);
            out
        }
        llr::ExprInner::Coercion(e, t1, t2) => {
            let r1 = rewrite_expr(ctxt, e);
            let r2 = ctxt.new_reg(t2.to_byte_size());
            ctxt.current_bloc.coerce(r1, t1, r2, t2);
            r2
        }
        llr::ExprInner::Constant(_p) => {
            todo!()
            // let r = ctxt.new_reg(common::Sizes::SUsize.to_byte_size());
            // ctxt.current_bloc.setp(p, r);
            // r
        }
        llr::ExprInner::Deref(expr) => {
            let out = ctxt.new_reg(e.size);
            let r = rewrite_expr(ctxt, expr);
            ctxt.current_bloc.deref(r, out);
            out
        }
        llr::ExprInner::FunCall(p, args) => {
            let rout = ctxt.new_reg(e.size);
            let args = args.into_iter().map(|e| rewrite_expr(ctxt, e)).collect();
            ctxt.current_bloc.call(p, args, rout);
            rout
        }
        llr::ExprInner::FunCallVar(id, args) => {
            let r = ctxt.get_var(&id);
            let rout = ctxt.new_reg(e.size);
            let args = args.into_iter().map(|e| rewrite_expr(ctxt, e)).collect();
            ctxt.current_bloc.call_reg(r, args, rout);
            rout
        }
        llr::ExprInner::FunVar(p) => {
            let r = ctxt.new_reg(common::Sizes::SUsize.to_byte_size());
            ctxt.current_bloc.leaq(LoR::L(p), r);
            r
        }
        llr::ExprInner::If(econd, bthen, belse) => {
            let rcond = rewrite_expr(ctxt, econd);
            let else_bid = ctxt.cond(rcond);
            let rthen = rewrite_bloc(ctxt, bthen);
            let rout = ctxt.new_reg(e.size);
            ctxt.current_bloc.mov(rthen, rout);
            let join_id = ctxt.finish_bloc(else_bid);
            let relse = rewrite_bloc(ctxt, belse);
            ctxt.current_bloc.mov(relse, rout);
            ctxt.join(join_id);
            rout
        }
        llr::ExprInner::Print(p) => {
            let rout = ctxt.new_reg(0);
            ctxt.current_bloc.print(p);
            rout
        }
        llr::ExprInner::Proj(expr, offset) => {
            let out = ctxt.new_reg(e.size);
            let r = compute_ref(ctxt, expr);
            ctxt.current_bloc.extract(r, offset, out);
            out
        }
        llr::ExprInner::Ref(e) => rewrite_pointer(ctxt, e),
        llr::ExprInner::Return(Some(expr)) => {
            let r = rewrite_expr(ctxt, expr);
            ctxt.current_bloc.ret(r);
            ctxt.new_reg(e.size)
        }
        llr::ExprInner::Return(None) => {
            let r = ctxt.new_reg(0);
            ctxt.current_bloc.ret(r);
            ctxt.new_reg(e.size)
        }
        llr::ExprInner::Set(_, addr, expr) => {
            eprintln!("Set {:?}", addr);
            let raddr = rewrite_expr(ctxt, addr);
            let rvalue = rewrite_expr(ctxt, expr);
            ctxt.current_bloc.update(rvalue, raddr);
            ctxt.new_reg(0)
        }
        llr::ExprInner::Tuple(size, exprs) => {
            let mut regs = Vec::new();
            let mut offset = 0;
            for expr in exprs {
                let expr_size = expr.size;
                regs.push((offset, rewrite_expr(ctxt, expr)));
                offset += expr_size;
            }
            let out = ctxt.new_reg(size);
            ctxt.current_bloc.merge_reg(regs, out);
            out
        }
        llr::ExprInner::UnaOp(UnaOp::Unary(uno), expr) => {
            let rout = ctxt.new_reg(e.size);
            let r1 = rewrite_expr(ctxt, expr);
            ctxt.current_bloc.mov(r1, rout);
            ctxt.current_bloc.unary(uno, rout);
            rout
        }
        llr::ExprInner::UnaOp(UnaOp::Binary(op, v, pos), e1) => {
            let re = rewrite_expr(ctxt, e1);
            let rv = ctxt.new_reg(v.size().to_byte_size());
            ctxt.current_bloc.set(v, rv);
            match pos {
                Pos::Left => rewrite_binop(ctxt, op, rv, re, e.size),
                Pos::Right => rewrite_binop(ctxt, op, re, rv, e.size),
            }
        }
        llr::ExprInner::Value(v) => {
            let r = ctxt.new_reg(v.size().to_byte_size());
            ctxt.current_bloc.set(v, r);
            r
        }
        llr::ExprInner::VarId(id) => ctxt.get_var(&id),
        llr::ExprInner::While(e, b) => {
            let enter = ctxt.gen_bid();
            let r = rewrite_expr(ctxt, e);
            let bid_else = ctxt.cond(r);
            rewrite_bloc(ctxt, b);
            ctxt.next_bloc(enter, bid_else);
            ctxt.new_reg(0)
        }
    }
}

fn rewrite_bloc(ctxt: &mut Context, b: llr::Bloc) -> Reg {
    let mut last_reg = ctxt.new_reg(0);
    for instr in b.content {
        match instr {
            llr::Instr::Expr(_v, e) => last_reg = rewrite_expr(ctxt, e),
            llr::Instr::Binding(id, e) => {
                let size = e.size;
                let reg = rewrite_expr(ctxt, e);
                let id = ctxt.new_var(id, size);
                ctxt.current_bloc.mov(reg, id);
                last_reg = ctxt.new_reg(0)
            }
        }
    }
    last_reg
}

fn rewrite_decl_fun(fun: llr::DeclFun) -> pasm::DeclFun {
    let mut ctxt = Context::new();
    let args = fun
        .args
        .into_iter()
        .map(|(id, size)| ctxt.new_var(id, size))
        .collect();
    let out_reg = rewrite_bloc(&mut ctxt, fun.content);
    let content = ctxt.extract_program(out_reg);
    pasm::DeclFun {
        name: fun.name,
        args,
        output: fun.output,
        content,
    }
}

pub fn compile(file: llr::File) -> pasm::File {
    pasm::File {
        funs: file.funs.into_iter().map(rewrite_decl_fun).collect(),
    }
}
