use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::hash::Hash;

use crate::ast::common::BuiltinType;
use crate::ast::low_level_repr::Value;
use crate::ast::operators::{Cmp, HArith, LArith, Shift, TUnaop};
use crate::ast::{
    common::{self, PathUL},
    low_level_repr::UnaOp,
};

#[derive(Debug)]
pub enum LoR<L, R> {
    L(L),
    R(R),
}

#[derive(Debug)]
pub struct File {
    pub funs: Vec<DeclFun>,
}

#[derive(Debug)]
pub struct DeclFun {
    pub name: PathUL<()>,
    pub args: Vec<Reg>,
    pub output: usize,
    pub content: FunBody,
}

#[derive(Debug)]
pub struct FunBody {
    pub enter: BID,
    pub blocs: HashMap<BID, Bloc<Reg>>,
    pub out_reg: Reg,
}

#[derive(Debug)]
pub struct Bloc<R> {
    pub id: BID,
    iid_gen: IIDGen,
    pub enter_iid: IID,
    pub out_iid: IID,
    pub content: HashMap<IID, Instruction<R>>,
    pub cond: Option<(R, BID)>,
    pub next: Option<BID>,
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct IID {
    id: usize,
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct BID {
    id: usize,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash)]
pub struct Reg {
    id: usize,
    pub size: usize,
}

#[derive(Debug)]
pub struct Instruction<R> {
    id: IID,
    pub instr: InstructionInner<R>,
    pub next: IID,
}

#[derive(Debug)]
struct Graph {
    arrows: HashMap<BID, HashSet<BID>>,
}

impl Graph {
    fn new() -> Self {
        Self {
            arrows: HashMap::new(),
        }
    }

    fn add(&mut self, i: BID, j: BID) {
        match self.arrows.get_mut(&i) {
            Some(set) => {
                set.insert(j);
            }
            None => {
                self.arrows.insert(i, HashSet::from([j]));
            }
        }
    }
}

#[derive(Debug)]
struct BlocGraph {
    in_arrows: Graph,
    out_arrows: Graph,
}

impl BlocGraph {
    fn new() -> Self {
        Self {
            in_arrows: Graph::new(),
            out_arrows: Graph::new(),
        }
    }

    fn add(&mut self, i: BID, o: BID) {
        self.in_arrows.add(i.clone(), o.clone());
        self.out_arrows.add(o, i);
    }

    fn add_instr<R>(&mut self, iid: BID, instr: &Bloc<R>) {
        match &instr.next {
            Some(next) => self.add(iid.clone(), next.clone()),
            _ => (),
        }
        match &instr.cond {
            Some((_, next)) => self.add(iid, next.clone()),
            _ => (),
        }
    }
}

#[derive(Debug)]
pub struct Context {
    reg_counter: usize,
    bloc_counter: usize,
    pub current_bloc: Bloc<Reg>,
    blocs: HashMap<BID, Bloc<Reg>>,
    arrows: BlocGraph,
    out: HashSet<BID>,
    variables: HashMap<usize, Reg>,
}

#[derive(Debug)]
pub struct IIDGen {
    counter: usize,
}

impl IIDGen {
    fn new() -> Self {
        Self { counter: 0 }
    }

    pub fn gen(&mut self) -> IID {
        let id = self.counter;
        self.counter += 1;
        IID { id }
    }
}

impl Reg {
    fn new(id: usize, size: usize) -> Self {
        Self { id, size }
    }
}

impl Context {
    pub fn new() -> Self {
        Self {
            reg_counter: 0,
            bloc_counter: 1,
            current_bloc: Bloc::new(BID { id: 0 }),
            blocs: HashMap::new(),
            arrows: BlocGraph::new(),
            out: HashSet::new(),
            variables: HashMap::new(),
        }
    }

    pub fn new_reg(&mut self, size: usize) -> Reg {
        let id = self.reg_counter;
        self.reg_counter += 1;
        Reg::new(id, size)
    }

    fn new_bloc(&mut self) -> BID {
        let id = self.bloc_counter;
        self.bloc_counter += 1;
        BID { id }
    }

    // pub fn next(&mut self, iid : BID, bloc : Bloc<Reg>) {
    //     self.arrows.add_instr(iid.clone(), &bloc);
    //     assert!(self.blocs.insert(iid, bloc).is_none());
    // }

    pub fn new_var(&mut self, id: usize, size: usize) -> Reg {
        let r = self.new_reg(size);
        assert!(self.variables.insert(id, r).is_none());
        r
    }

    pub fn get_var(&self, id: &usize) -> Reg {
        *self.variables.get(id).unwrap()
    }

    // Opens the bloc if r is true and returns bloc id for false
    pub fn cond(&mut self, r: Reg) -> BID {
        let bid_then = self.new_bloc();
        let bid_else = self.new_bloc();
        self.current_bloc.cond = Some((r, bid_then.clone()));
        self.next_bloc(bid_else.clone(), bid_then);
        bid_else
    }

    // Opens the bloc if r is false and returns bloc id for true
    pub fn cond_rev(&mut self, r: Reg) -> BID {
        let bid_then = self.new_bloc();
        let bid_else = self.new_bloc();
        self.current_bloc.cond = Some((r, bid_then.clone()));
        self.next_bloc(bid_then.clone(), bid_else);
        bid_then
    }

    pub fn jcc(&mut self, r: Reg, to: BID) {
        self.current_bloc.cond = Some((r, to));
        let target = self.new_bloc();
        self.next_bloc(target.clone(), target)
    }

    pub fn extract_program(mut self, r: Reg) -> FunBody {
        self.blocs
            .insert(self.current_bloc.id.clone(), self.current_bloc);
        FunBody {
            enter: BID { id: 0 },
            blocs: self.blocs,
            out_reg: r,
        }
    }

    pub fn gen_bid(&mut self) -> BID {
        let bid = self.new_bloc();
        self.next_bloc(bid.clone(), bid.clone());
        bid
    }

    // Closes bloc by targeting closed_target and opens bloc next_open
    pub fn next_bloc(&mut self, closed_target: BID, next_open: BID) {
        self.current_bloc.next = Some(closed_target);
        let bloc = std::mem::replace(&mut self.current_bloc, Bloc::new(next_open));
        assert!(self.blocs.insert(bloc.id.clone(), bloc).is_none());
    }

    // Finishes current bloc and starts a new one at new_bid
    // Returns the targeted BID of the finished bloc
    pub fn finish_bloc(&mut self, next_open: BID) -> BID {
        let closed_target = self.new_bloc();
        self.next_bloc(closed_target.clone(), next_open);
        closed_target
    }

    pub fn cond_next(&mut self, rcond: Reg, bid_else: BID) -> BID {
        let bid_then = self.new_bloc();
        self.current_bloc.cond = Some((rcond, bid_then.clone()));
        self.join(bid_else);
        bid_then
    }

    pub fn join(&mut self, join_id: BID) {
        self.next_bloc(join_id.clone(), join_id)
    }
}

#[derive(Debug)]
pub enum InstructionInner<R> {
    Mov(R, R),
    Leaq(LoR<PathUL<()>, R>, R),
    Set(Value, R),
    BuildStruct(Vec<(usize, R)>, R),
    Cmp(Cmp, R, R, R), // Comparers r1 and r2 and stores result in r3
    LArith(LArith, R, LoR<R, Value>, R), // r3 = r2 op r1
    HArith(HArith, R, R, R), // r3 = r2 op r1
    Shift(Shift, R, LoR<R, usize>, R), // r3 = r2 op r1
    Unaop(TUnaop, R),
    Deref(R, R),
    Coercion(R, R, common::BuiltinType, common::BuiltinType),
    CallP(LoR<PathUL<()>, R>, Vec<R>, R),
    Print(PathUL<()>),
    Return(R),
    Extract(R, usize, R),
    Update(R, R), // Move content of R1 to address stored in R2
}

impl<R> Bloc<R> {
    pub fn new(id: BID) -> Self {
        let mut iid_gen = IIDGen::new();
        let out_iid = iid_gen.gen();
        Self {
            id,
            enter_iid: out_iid.clone(),
            iid_gen,
            out_iid,
            content: HashMap::new(),
            cond: None,
            next: None,
        }
    }

    fn add_instr(&mut self, instr: InstructionInner<R>) {
        let id = self.out_iid;
        self.out_iid = self.iid_gen.gen();
        let instr = Instruction {
            id,
            instr,
            next: self.out_iid,
        };
        assert!(self.content.insert(id, instr).is_none())
    }

    pub fn mov(&mut self, from: R, to: R) {
        self.add_instr(InstructionInner::Mov(from, to))
    }

    pub fn set(&mut self, v: Value, r: R) {
        self.add_instr(InstructionInner::Set(v, r))
    }

    pub fn leaq(&mut self, p: LoR<PathUL<()>, R>, r: R) {
        self.add_instr(InstructionInner::Leaq(p, r))
    }

    pub fn larith(&mut self, aop: LArith, r1: R, r2: LoR<R, Value>, r3: R) {
        self.add_instr(InstructionInner::LArith(aop, r1, r2, r3))
    }

    pub fn harith(&mut self, aop: HArith, r1: R, r2: R, r3: R) {
        self.add_instr(InstructionInner::HArith(aop, r1, r2, r3))
    }

    pub fn shift(&mut self, sop: Shift, r1: R, r2: LoR<R, usize>, r3: R) {
        self.add_instr(InstructionInner::Shift(sop, r1, r2, r3))
    }

    pub fn cmp(&mut self, cmp: Cmp, r1: R, r2: R, r3: R) {
        self.add_instr(InstructionInner::Cmp(cmp, r1, r2, r3))
    }

    pub fn coerce(&mut self, r1: R, t1: BuiltinType, r2: R, t2: BuiltinType) {
        self.add_instr(InstructionInner::Coercion(r1, r2, t1, t2))
    }

    pub fn call(&mut self, p: PathUL<()>, args: Vec<R>, out: R) {
        self.add_instr(InstructionInner::CallP(LoR::L(p), args, out))
    }

    pub fn call_reg(&mut self, r: R, args: Vec<R>, out: R) {
        self.add_instr(InstructionInner::CallP(LoR::R(r), args, out))
    }

    pub fn print(&mut self, p: PathUL<()>) {
        self.add_instr(InstructionInner::Print(p))
    }

    pub fn ret(&mut self, r: R) {
        self.add_instr(InstructionInner::Return(r))
    }

    pub fn merge_reg(&mut self, regs: Vec<(usize, R)>, out: R) {
        self.add_instr(InstructionInner::BuildStruct(regs, out))
    }

    pub fn deref(&mut self, r: R, out: R) {
        self.add_instr(InstructionInner::Deref(r, out))
    }

    pub fn extract(&mut self, r: R, offset: usize, out: R) {
        self.add_instr(InstructionInner::Extract(r, offset, out))
    }

    pub fn unary(&mut self, uno: TUnaop, r: R) {
        self.add_instr(InstructionInner::Unaop(uno, r))
    }

    pub fn update(&mut self, r1: R, r2: R) {
        self.add_instr(InstructionInner::Update(r1, r2))
    }
}

impl BID {
    pub fn id(&self) -> usize {
        self.id
    }
}
