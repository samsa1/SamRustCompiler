pub mod file;
pub mod instr;
pub mod reg;
pub mod data;

use std::io::prelude::*;
use std::ops::Add;

macro_rules! regb {
    ($reg_name:ident) => {
        crate::backend::asm::reg::Operand::Reg(crate::backend::asm::reg::RegB::$reg_name)
    };
}

macro_rules! regw {
    ($reg_name:ident) => {
        crate::backend::asm::reg::Operand::Reg(crate::backend::asm::reg::RegW::$reg_name)
    };
}

macro_rules! regl {
    ($reg_name:ident) => {
        crate::backend::asm::reg::Operand::Reg(crate::backend::asm::reg::RegL::$reg_name)
    };
}

macro_rules! regq {
    ($reg_name:ident) => {
        crate::backend::asm::reg::Operand::Reg(crate::backend::asm::reg::RegQ::$reg_name)
    };
}

macro_rules! addr {
    ($reg:expr) => {
        crate::backend::asm::reg::Operand::Addr(0, $reg, None, 0)
    };
    ($offset:expr, $reg:expr) => {
        crate::backend::asm::reg::Operand::Addr($offset, $reg, None, 0)
    };
    ($offset:expr, $reg:expr, $reg2:expr) => {
        crate::backend::asm::reg::Operand::Addr($offset, $reg, Some($reg2), 1)
    };
    ($offset:expr, $reg:expr, $reg2:expr, $scale:expr) => {
        crate::backend::asm::reg::Operand::Addr($offset, $reg, Some($reg2), scale)
    };
}

pub fn pushq(op: reg::Operand<reg::RegQ>) -> Asm {
    Asm::Instr(Box::new(instr::InstrOp::new(instr::OpInstrName::Push, op)))
}
pub fn popq(op: reg::Operand<reg::RegQ>) -> Asm {
    Asm::Instr(Box::new(instr::InstrOp::new(instr::OpInstrName::Pop, op)))
}

pub fn deplq(l: reg::Label, op: reg::Operand<reg::RegQ>) -> Asm {
    leaq(reg::Operand::LabRelAddr(l), op)
}

pub fn leaq(reg1: reg::Operand<reg::RegQ>, reg2: reg::Operand<reg::RegQ>) -> Asm {
    Asm::Instr(Box::new(instr::InstrOpOp::new(
        instr::OpOpInstrName::Lea,
        reg1,
        reg2,
    )))
}

macro_rules! build_instr_op_op {
    ($op:ident, $nameb:ident, $namew:ident, $namel:ident, $nameq:ident) => {
        pub fn $nameb(reg1: reg::Operand<reg::RegB>, reg2: reg::Operand<reg::RegB>) -> Asm {
            Asm::Instr(Box::new(instr::InstrOpOp::new(
                instr::OpOpInstrName::$op,
                reg1,
                reg2,
            )))
        }

        pub fn $namew(reg1: reg::Operand<reg::RegW>, reg2: reg::Operand<reg::RegW>) -> Asm {
            Asm::Instr(Box::new(instr::InstrOpOp::new(
                instr::OpOpInstrName::$op,
                reg1,
                reg2,
            )))
        }

        pub fn $namel(reg1: reg::Operand<reg::RegL>, reg2: reg::Operand<reg::RegL>) -> Asm {
            Asm::Instr(Box::new(instr::InstrOpOp::new(
                instr::OpOpInstrName::$op,
                reg1,
                reg2,
            )))
        }

        pub fn $nameq(reg1: reg::Operand<reg::RegQ>, reg2: reg::Operand<reg::RegQ>) -> Asm {
            Asm::Instr(Box::new(instr::InstrOpOp::new(
                instr::OpOpInstrName::$op,
                reg1,
                reg2,
            )))
        }
    };
}

build_instr_op_op!(Move, movb, movw, movl, movq);
build_instr_op_op!(Add, addb, addw, addl, addq);
build_instr_op_op!(Sub, subb, subw, subl, subq);
build_instr_op_op!(IMul, imulb, imluw, imull, imulq);

build_instr_op_op!(And, andb, andw, andl, andq);
build_instr_op_op!(Or, orb, orw, orl, orq);
build_instr_op_op!(Xor, xorb, xorw, xorl, xorq);

build_instr_op_op!(Cmp, cmpb, cmpw, cmpl, cmpq);
build_instr_op_op!(Test, testb, testw, testl, testq);

macro_rules! build_instr_op {
    ($op:ident, $nameb:ident, $namew:ident, $namel:ident, $nameq:ident) => {
        pub fn $nameb(reg: reg::Operand<reg::RegB>) -> Asm {
            Asm::Instr(Box::new(instr::InstrOp::new(instr::OpInstrName::$op, reg)))
        }

        pub fn $namew(reg: reg::Operand<reg::RegW>) -> Asm {
            Asm::Instr(Box::new(instr::InstrOp::new(instr::OpInstrName::$op, reg)))
        }

        pub fn $namel(reg: reg::Operand<reg::RegL>) -> Asm {
            Asm::Instr(Box::new(instr::InstrOp::new(instr::OpInstrName::$op, reg)))
        }

        pub fn $nameq(reg: reg::Operand<reg::RegQ>) -> Asm {
            Asm::Instr(Box::new(instr::InstrOp::new(instr::OpInstrName::$op, reg)))
        }
    };
}

build_instr_op!(Neg, negb, negw, negl, negq);
build_instr_op!(Not, notb, notw, notl, notq);
build_instr_op!(Inc, incb, incw, incl, incq);
build_instr_op!(Dec, decb, decw, decl, decq);
build_instr_op!(SignedDiv, idivb, idivw, idivl, idivq);
build_instr_op!(UnsignedDiv, divb, divw, divl, divq);

pub fn immq(imm: i64) -> reg::Operand<reg::RegQ> {
    reg::Operand::Imm(imm)
}
pub fn imml(imm: i32) -> reg::Operand<reg::RegL> {
    reg::Operand::Imm(imm as i64)
}
pub fn immw(imm: i16) -> reg::Operand<reg::RegW> {
    reg::Operand::Imm(imm as i64)
}
pub fn immb(imm: i8) -> reg::Operand<reg::RegB> {
    reg::Operand::Imm(imm as i64)
}

pub fn call(label: reg::Label) -> Asm {
    Asm::Instr(Box::new(instr::Goto::Call(label)))
}

pub fn ret() -> Asm {
    Asm::Instr(Box::new(instr::InstrNoArg::Ret))
}

pub fn cltd() -> Asm {
    Asm::Instr(Box::new(instr::InstrNoArg::Cltd))
}
pub fn cqto() -> Asm {
    Asm::Instr(Box::new(instr::InstrNoArg::Cqto))
}

pub fn set(cond : instr::Cond, reg : reg::Operand<reg::RegB>) -> Asm {
    Asm::Instr(Box::new(instr::Goto::Set(cond, reg)))
}


pub fn jmp(label: reg::Label) -> Asm {
    Asm::Instr(Box::new(instr::Goto::Jump(label)))
}

pub fn jz(label: reg::Label) -> Asm {
    Asm::Instr(Box::new(instr::Goto::CondJump(instr::Cond::JZ, label)))
}

pub fn jnz(label: reg::Label) -> Asm {
    Asm::Instr(Box::new(instr::Goto::CondJump(instr::Cond::JNZ, label)))
}

pub fn jae(label: reg::Label) -> Asm {
    Asm::Instr(Box::new(instr::Goto::CondJump(instr::Cond::JAE, label)))
}

pub fn label(l: reg::Label) -> Asm {
    Asm::Label(l)
}

// cmovb is not valid

pub fn cmovw(
    cond: instr::Cond,
    reg1: reg::Operand<reg::RegW>,
    reg2: reg::Operand<reg::RegW>,
) -> Asm {
    Asm::Instr(Box::new(instr::CondMove::new(cond, reg1, reg2)))
}
pub fn cmovl(
    cond: instr::Cond,
    reg1: reg::Operand<reg::RegL>,
    reg2: reg::Operand<reg::RegL>,
) -> Asm {
    Asm::Instr(Box::new(instr::CondMove::new(cond, reg1, reg2)))
}
pub fn cmovq(
    cond: instr::Cond,
    reg1: reg::Operand<reg::RegQ>,
    reg2: reg::Operand<reg::RegQ>,
) -> Asm {
    Asm::Instr(Box::new(instr::CondMove::new(cond, reg1, reg2)))
}

pub fn dbyte(name : String, i : i8) -> data::Data {
    data::Data::new(Some(name), data::DataEL::Byte(vec![i]))
}
pub fn dword(name : String, i : i16) -> data::Data {
    data::Data::new(Some(name), data::DataEL::Word(vec![i]))
}
pub fn dlong(name : String, i : i32) -> data::Data {
    data::Data::new(Some(name), data::DataEL::Long(vec![i]))
}
pub fn dquad(name : String, i : i64) -> data::Data {
    data::Data::new(Some(name), data::DataEL::Quad(vec![i]))
}

pub fn string(name : String, data : String) -> data::Data {
    data::Data::new(Some(name), data::DataEL::String(data))
}

pub fn address(name : String, addr : reg::Label) -> data::Data {
    data::Data::new(Some(name), data::DataEL::Address(vec![addr]))
}

pub fn space(i : usize) -> data::Data {
    data::Data::new(None, data::DataEL::Space(i))
}



pub fn nop() -> Asm {
    Asm::Concat(Vec::new())
}

pub enum Asm {
    Concat(Vec<Asm>),
    Instr(Box<dyn instr::Instr>),
    Label(reg::Label),
}

impl Add for Asm {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self::Concat(vec![self, other])
    }
}

impl Asm {
    pub fn write_in(self, file: &mut std::fs::File) -> std::io::Result<()> {
        match self {
            Self::Concat(vec) => {
                for asm in vec {
                    asm.write_in(file)?
                }
                Ok(())
            }
            Self::Label(label) => {
                label.write_in(file)?;
                file.write_all(b":\n")
            }
            Self::Instr(instr) => instr.write_in(file),
        }
    }
}

/*
type 'size operand = formatter -> unit -> unit

let mangle_none fmt (l: label) = fprintf fmt "%s" l
let mangle_leading_underscore fmt (l: label) = fprintf fmt "_%s" l
let mangle = if estMac then mangle_leading_underscore else mangle_none

let reg r = fun fmt () -> fprintf fmt "%s" r
let (!%) = reg
let imm i = fun fmt () -> fprintf fmt "$%i" i
let imm32 i = fun fmt () -> fprintf fmt "$%ld" i
let imm64 i = fun fmt () -> fprintf fmt "$%s" (Int64.to_string i)
let immF f = fun fmt () -> fprintf fmt "$%f" f
let immD f = fun fmt () -> fprintf fmt "$%f" f
let ind ?(ofs=0) ?index ?(scale=1) r = fun fmt () -> match index with
  | None -> fprintf fmt "%d(%s)" ofs r
  | Some r1 -> fprintf fmt "%d(%s,%s,%d)" ofs r r1 scale
let abslab (l: label) = fun fmt () -> fprintf fmt "$%a" mangle l
let rellab (l: label) = fun fmt () -> fprintf fmt "%a(%%rip)" mangle l
let lab = if estMac then rellab else abslab
let ilab (l: label) = fun fmt () -> fprintf fmt "(%a)" mangle l
let univerlab = if estMac then lab else ilab

type 'a asm =
  | Nop
  | S of string
  | Cat of 'a asm * 'a asm

let nop = Nop
let inline s = S s
let (++) x y = Cat (x, y)

type text = [`text ] asm
type data = [`data ] asm

let buf = Buffer.create 17
let fmt = formatter_of_buffer buf
let ins x =
  Buffer.add_char buf '\t';
  kfprintf (fun fmt ->
    fprintf fmt "\n";
    pp_print_flush fmt ();
    let s = Buffer.contents buf in
    Buffer.clear buf;
    S s
  ) fmt x

let pr_list fmt pr = function
  | []      -> ()
  | [i]     -> pr fmt i
  | i :: ll -> pr fmt i; List.iter (fun i -> fprintf fmt ", %a" pr i) ll

let pr_ilist fmt l =
  pr_list fmt (fun fmt i -> fprintf fmt "%i" i) l

let pr_alist fmt l =
  pr_list fmt (fun fmt (a : label) -> fprintf fmt "%s" a) l

let movb a b = ins "movb %a, %a" a () b ()
let movw a b = ins "movw %a, %a" a () b ()
let movl a b = ins "movl %a, %a" a () b ()
let movq a b = ins "movq %a, %a" a () b ()

let movabsq a b = ins "movabsq %a, %s" a () b

let movsbw a b = ins "movsbw %a, %s" a () b
let movsbl a b = ins "movsbl %a, %s" a () b
let movsbq a b = ins "movsbq %a, %s" a () b
let movswl a b = ins "movswl %a, %s" a () b
let movswq a b = ins "movswq %a, %s" a () b
let movslq a b = ins "movslq %a, %s" a () b

let movzbw a b = ins "movzbw %a, %s" a () b
let movzbl a b = ins "movzbl %a, %s" a () b
let movzbq a b = ins "movzbq %a, %s" a () b
let movzwl a b = ins "movzwl %a, %s" a () b
let movzwq a b = ins "movzwq %a, %s" a () b

let movsd a b = ins "movsd %a, %s" a () b
let movss a b = ins "movss %a, %s" a () b

let cmovs a b = ins "cmovs %a, %s" a () b

let cmove a b = ins "cmove %s, %a" a b ()

let cvtss2sd a b = ins "cvtss2sd %a, %s" a () b
let cvtsd2ss a b = ins "cvtsd2ss %a, %s" a () b

  (** conversion from integer to float/double *)
let cvtsi2sd a b = ins "cvtsi2sd %a, %s" a () b
let cvtsi2ss a b = ins "cvtsi2ss %a, %s" a () b
let cvtsi2sdq a b = ins "cvtsi2sdq %a, %s" a () b
let cvtsi2ssq a b = ins "cvtsi2sdq %a, %s" a () b

  (** conversion from float/integer to integer *)
let cvttss2si a b = ins "cvttss2si %a, %s" a () b
let cvttsd2si a b = ins "cvttsd2si %a, %s" a () b
let cvttss2siq a b = ins "cvttss2siq %a, %s" a () b
let cvttsd2siq a b = ins "cvttsd2siq %a, %s" a () b

let addss a b = ins "addss %a, %a" a () b ()
let addsd a b = ins "addsd %a, %a" a () b () (* Addition double précision *)

let subss a b = ins "subss %a, %a" a () b ()
let subsd a b = ins "subsd %a, %a" a () b () (* Soustraction double précision *)

let imulw a b = ins "imulw %a, %a" a () b ()
let imull a b = ins "imull %a, %a" a () b ()
let imulq a b = ins "imulq %a, %a" a () b ()
let mulss a b = ins "mulss %a, %a" a () b ()
let mulsd a b = ins "mulsd %a, %a" a () b () (* multiplication double précision *)

let idivq a = ins "idivq %a" a ()
let divss a b = ins "divss %a, %a" a () b ()
let divsd a b = ins "divsd %a, %a" a () b ()
let cqto = S "\tcqto\n"

let sqrtss a b = ins "sqrtsd %a, %a" a () b ()
let sqrtsd a b = ins "sqrtsd %a, %a" a () b ()


let sarb a b = ins "sarb %a, %a" a () b ()
let sarw a b = ins "sarw %a, %a" a () b ()
let sarl a b = ins "sarl %a, %a" a () b ()
let sarq a b = ins "sarq %a, %a" a () b ()

let jmp (z: label) = ins "jmp %a" mangle z
let jmp_star o = ins "jmp *%a" o ()

let leave = ins "leave"
let ret = ins "ret"

let ucomiss a b = ins "ucomiss %a, %a" a () b ()
let ucomisd a b = ins "ucomisd %a, %a" a () b ()

let cmpltsd a b = ins "cmpltsd %a, %a" a () b ()
let cmplesd a b = ins "cmplesd %a, %a" a () b ()
let cmpnltsd a b = ins "cmpnltsd %a, %a" a () b ()
let cmpnlesd a b = ins "cmpnlesd %a, %a" a () b ()


let sete  a = ins "sete %a" a ()
let setne a = ins "setne %a" a ()
let sets  a = ins "sets %a" a ()
let setns a = ins "setns %a" a ()
let setg  a = ins "setg %a" a ()
let setge a = ins "setge %a" a ()
let setl  a = ins "setl %a" a ()
let setle a = ins "setle %a" a ()
let seta  a = ins "seta %a" a ()
let setae a = ins "setae %a" a ()
let setb  a = ins "setb %a" a ()
let setbe a = ins "setbe %a" a ()

let label (s : label) = S (asprintf "%a:\n" mangle s)
let globl (s: label) = S (asprintf "\t.globl\t%a\n" mangle s)

let comment s = S ("#" ^ s ^ "\n")

let align n = ins ".align %i" n

let dbyte l = ins ".byte %a" pr_ilist l
let dint  l = ins ".int %a" pr_ilist l
let dword l = ins ".word %a" pr_ilist l
let dquad l = ins ".quad %a" pr_ilist l
let string s = ins ".string %S" s
let double d = ins ".double %f" d

let address l = ins ".quad %a" pr_alist l
let space n = ins ".space %d" n

let pushq a = ins "pushq %a" a ()
let popq r = ins "popq %s" r

let rdtsc () = ins "rdtsc"

let tpause a = ins "tpause %a" a ()

let hlt () = ins "hlt"

let syscall () = ins "syscall"

type program = {
  text : [ `text ] asm;
  data : [ `data ] asm;
}

let rec pr_asm fmt = function
  | Nop          -> ()
  | S s          -> fprintf fmt "%s" s
  | Cat (a1, a2) -> pr_asm fmt a1; pr_asm fmt a2

let print_program fmt p =
  fprintf fmt "\t.text\n";
  pr_asm fmt p.text;
  fprintf fmt "\t.data\n";
  pr_asm fmt p.data;
  pp_print_flush fmt ()

let print_in_file ~file p =
  let c = open_out file in
  let fmt = formatter_of_out_channel c in
  print_program fmt p;
  close_out c

  */
