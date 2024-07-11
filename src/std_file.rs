use std::collections::HashMap;
use crate::ast::common;
use crate::ast::rust::*;
use crate::frontend::Module;

pub fn stdlib() -> Option<Module<File>> {Some(

{let file = File {
	name : "std/mod.rs".to_string(),
	content : vec![Decl::Fun(DeclFun {
	public : false,
	name : common::Ident::new("main", common::Location::default()),
self_arg : None,
	args : vec![],
	output : PreType { content : PreTypeInner::Tuple(vec![])},
	generics : vec![],
	content : Bloc { loc : common::Location::default(),
	content : vec![Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Drop, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::FunCall(vec![], common::Ident::new("testing", common::Location::default()), vec![]))
, typed : None})},]},
	id_counter : common::IdCounter::new(),
}),],
	dep : vec![Open::Mod(true, common::Ident::new("allocator", common::Location::default()), None),Open::Mod(true, common::Ident::new("vec", common::Location::default()), None),Open::Use(common::Path::new(vec![common::NamePath::Name(common::Ident::new("allocator", common::Location::default())),common::NamePath::Name(common::Ident::new("testing", common::Location::default())),], common::Location::default()),None),],
	err_reporter : common::ErrorReporter::empty(),
}
;
let mut submodules = HashMap::new();submodules.insert("vec".to_string(), (true, 
{let file = File {
	name : "std/vec/mod.rs".to_string(),
	content : vec![],
	dep : vec![Open::Mod(true, common::Ident::new("Vec", common::Location::default()), None),],
	err_reporter : common::ErrorReporter::empty(),
}
;
let mut submodules = HashMap::new();submodules.insert("Vec".to_string(), (true, 
{let file = File {
	name : "std/vec/Vec.rs".to_string(),
	content : vec![Decl::Fun(DeclFun {
	public : true,
	name : common::Ident::new("new", common::Location::default()),
self_arg : None,
	args : vec![],
	output : PreType { content : PreTypeInner::IdentParametrized(common::Ident::new("Vec", common::Location::default()), vec![PreType { content : PreTypeInner::Ident(common::Ident::new("T", common::Location::default()))},])},
	generics : vec![(common::Ident::new("T", common::Location::default()), vec![]),],
	content : Bloc { loc : common::Location::default(),
	content : vec![Instr { loc : common::Location::default(),
	content : InstrInner::Binding(true, common::Ident::new("vec", common::Location::default()), None, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::MacroCall(common::Ident::new("vec", common::Location::default()), vec![]))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Drop, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::FunCallPath(vec![], common::Path::new(vec![common::NamePath::Name(common::Ident::new("crate", common::Location::default())),common::NamePath::Name(common::Ident::new("allocator", common::Location::default())),common::NamePath::Name(common::Ident::new("init", common::Location::default())),], common::Location::default()), vec![Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Ref(true, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("vec", common::Location::default())))
, typed : None}))
, typed : None},]))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Drop, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::FunCallPath(vec![], common::Path::new(vec![common::NamePath::Name(common::Ident::new("crate", common::Location::default())),common::NamePath::Name(common::Ident::new("allocator", common::Location::default())),common::NamePath::Name(common::Ident::new("realloc", common::Location::default())),], common::Location::default()), vec![Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Ref(true, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("vec", common::Location::default())))
, typed : None}))
, typed : None},Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(0, None))
, typed : None},Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(0, None))
, typed : None},]))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Keep, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::MacroCall(common::Ident::new("vec", common::Location::default()), vec![]))
, typed : None})},]},
	id_counter : common::IdCounter::new(),
}),Decl::Fun(DeclFun {
	public : true,
	name : common::Ident::new("push", common::Location::default()),
self_arg : None,
	args : vec![(common::Ident::new("vec", common::Location::default()), false, PreType { content : PreTypeInner::Ref(true, Box::new(PreType { content : PreTypeInner::IdentParametrized(common::Ident::new("Vec", common::Location::default()), vec![PreType { content : PreTypeInner::Ident(common::Ident::new("T", common::Location::default()))},])}))}),(common::Ident::new("t", common::Location::default()), false, PreType { content : PreTypeInner::Ident(common::Ident::new("T", common::Location::default()))}),],
	output : PreType { content : PreTypeInner::Tuple(vec![])},
	generics : vec![(common::Ident::new("T", common::Location::default()), vec![]),],
	content : Bloc { loc : common::Location::default(),
	content : vec![]},
	id_counter : common::IdCounter::new(),
}),Decl::Fun(DeclFun {
	public : true,
	name : common::Ident::new("len", common::Location::default()),
self_arg : None,
	args : vec![(common::Ident::new("vec", common::Location::default()), false, PreType { content : PreTypeInner::Ref(false, Box::new(PreType { content : PreTypeInner::IdentParametrized(common::Ident::new("Vec", common::Location::default()), vec![PreType { content : PreTypeInner::Ident(common::Ident::new("T", common::Location::default()))},])}))}),],
	output : PreType { content : PreTypeInner::Ident(common::Ident::new("usize", common::Location::default()))},
	generics : vec![(common::Ident::new("T", common::Location::default()), vec![]),],
	content : Bloc { loc : common::Location::default(),
	content : vec![Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Keep, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(0, None))
, typed : None})},]},
	id_counter : common::IdCounter::new(),
}),Decl::Fun(DeclFun {
	public : true,
	name : common::Ident::new("get", common::Location::default()),
self_arg : None,
	args : vec![(common::Ident::new("vec", common::Location::default()), false, PreType { content : PreTypeInner::Ref(false, Box::new(PreType { content : PreTypeInner::IdentParametrized(common::Ident::new("Vec", common::Location::default()), vec![PreType { content : PreTypeInner::Ident(common::Ident::new("T", common::Location::default()))},])}))}),(common::Ident::new("id", common::Location::default()), false, PreType { content : PreTypeInner::Ident(common::Ident::new("usize", common::Location::default()))}),],
	output : PreType { content : PreTypeInner::Ref(false, Box::new(PreType { content : PreTypeInner::Ident(common::Ident::new("T", common::Location::default()))}))},
	generics : vec![(common::Ident::new("T", common::Location::default()), vec![]),],
	content : Bloc { loc : common::Location::default(),
	content : vec![Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Keep, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Ref(false, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Index(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("vec", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("id", common::Location::default())))
, typed : None}))
, typed : None}))
, typed : None})},]},
	id_counter : common::IdCounter::new(),
}),],
	dep : vec![],
	err_reporter : common::ErrorReporter::empty(),
}
;
Module::build(file, HashMap::new())
}));
Module::build(file, submodules)
}));
submodules.insert("allocator".to_string(), (true, 
{let file = File {
	name : "std/allocator/mod.rs".to_string(),
	content : vec![Decl::Const(DeclConst {
	public : false,
	name : common::Ident::new("NULL_PTR", common::Location::default()),
	typ : PreType { content : PreTypeInner::Ident(common::Ident::new("usize", common::Location::default()))},
	expr : Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(18446744073709551615, Some((false, common::Sizes::SUsize))))
, typed : None},}),Decl::Const(DeclConst {
	public : false,
	name : common::Ident::new("U32_MAX", common::Location::default()),
	typ : PreType { content : PreTypeInner::Ident(common::Ident::new("usize", common::Location::default()))},
	expr : Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Shl, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(1, None))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(32, None))
, typed : None}))
, typed : None},}),Decl::Const(DeclConst {
	public : false,
	name : common::Ident::new("FREE_BIT", common::Location::default()),
	typ : PreType { content : PreTypeInner::Ident(common::Ident::new("u32", common::Location::default()))},
	expr : Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(1, None))
, typed : None},}),Decl::Fun(DeclFun {
	public : false,
	name : common::Ident::new("const_zero", common::Location::default()),
self_arg : None,
	args : vec![],
	output : PreType { content : PreTypeInner::Ident(common::Ident::new("usize", common::Location::default()))},
	generics : vec![],
	content : Bloc { loc : common::Location::default(),
	content : vec![Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Keep, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(0, None))
, typed : None})},]},
	id_counter : common::IdCounter::new(),
}),Decl::Fun(DeclFun {
	public : true,
	name : common::Ident::new("init", common::Location::default()),
self_arg : None,
	args : vec![(common::Ident::new("vec", common::Location::default()), false, PreType { content : PreTypeInner::Ref(true, Box::new(PreType { content : PreTypeInner::IdentParametrized(common::Ident::new("Vec", common::Location::default()), vec![PreType { content : PreTypeInner::Ident(common::Ident::new("u32", common::Location::default()))},])}))}),],
	output : PreType { content : PreTypeInner::Tuple(vec![])},
	generics : vec![],
	content : Bloc { loc : common::Location::default(),
	content : vec![Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Drop, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Set, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Index(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("vec", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(0, None))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::BitOr, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Coercion(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Parenthesis(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Sub, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Method(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("vec", common::Location::default())))
, typed : None}, common::Ident::new("len", common::Location::default()), vec![]))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(4, None))
, typed : None}))
, typed : None}))
, typed : None}, Some(PreType { content : PreTypeInner::Ident(common::Ident::new("u32", common::Location::default()))})))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("FREE_BIT", common::Location::default())))
, typed : None}))
, typed : None}))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Drop, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Set, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Index(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("vec", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(1, None))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(0, None))
, typed : None}))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Binding(false, common::Ident::new("len", common::Location::default()), None, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Method(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("vec", common::Location::default())))
, typed : None}, common::Ident::new("len", common::Location::default()), vec![]))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Drop, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Set, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Index(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("vec", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Sub, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("len", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(2, None))
, typed : None}))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("FREE_BIT", common::Location::default())))
, typed : None}))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Drop, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Set, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Index(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("vec", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Sub, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("len", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(1, None))
, typed : None}))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(0, None))
, typed : None}))
, typed : None})},]},
	id_counter : common::IdCounter::new(),
}),Decl::Fun(DeclFun {
	public : true,
	name : common::Ident::new("malloc", common::Location::default()),
self_arg : None,
	args : vec![(common::Ident::new("vec", common::Location::default()), false, PreType { content : PreTypeInner::Ref(true, Box::new(PreType { content : PreTypeInner::IdentParametrized(common::Ident::new("Vec", common::Location::default()), vec![PreType { content : PreTypeInner::Ident(common::Ident::new("u32", common::Location::default()))},])}))}),(common::Ident::new("size", common::Location::default()), false, PreType { content : PreTypeInner::Ident(common::Ident::new("usize", common::Location::default()))}),],
	output : PreType { content : PreTypeInner::Ident(common::Ident::new("usize", common::Location::default()))},
	generics : vec![],
	content : Bloc { loc : common::Location::default(),
	content : vec![Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Keep, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::If(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::GreaterEq, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("size", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("U32_MAX", common::Location::default())))
, typed : None}))
, typed : None}, Bloc { loc : common::Location::default(),
	content : vec![Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Drop, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Return(Some(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("NULL_PTR", common::Location::default())))
, typed : None})))
, typed : None})},]}, Bloc { loc : common::Location::default(),
	content : vec![]}))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Binding(true, common::Ident::new("size", common::Location::default()), None, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Coercion(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("size", common::Location::default())))
, typed : None}, Some(PreType { content : PreTypeInner::Ident(common::Ident::new("u32", common::Location::default()))})))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Drop, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Set, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("size", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Shl, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Shr, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Parenthesis(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Add, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("size", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(7, None))
, typed : None}))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(3, None))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(1, None))
, typed : None}))
, typed : None}))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Binding(true, common::Ident::new("pos", common::Location::default()), None, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(0, None))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Binding(true, common::Ident::new("previous", common::Location::default()), None, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(0, None))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Drop, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::While(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Bool(true))
, typed : None}, Bloc { loc : common::Location::default(),
	content : vec![Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Keep, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::If(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Eq, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::BitAnd, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Index(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("vec", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("pos", common::Location::default())))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("FREE_BIT", common::Location::default())))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(1, None))
, typed : None}))
, typed : None}, Bloc { loc : common::Location::default(),
	content : vec![Instr { loc : common::Location::default(),
	content : InstrInner::Binding(false, common::Ident::new("available", common::Location::default()), None, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Sub, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Index(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("vec", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("pos", common::Location::default())))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(1, None))
, typed : None}))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Keep, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::If(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::GreaterEq, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("available", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Add, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("size", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(2, None))
, typed : None}))
, typed : None}))
, typed : None}, Bloc { loc : common::Location::default(),
	content : vec![Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Keep, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::If(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Greater, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("available", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Add, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("size", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(4, None))
, typed : None}))
, typed : None}))
, typed : None}, Bloc { loc : common::Location::default(),
	content : vec![Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Drop, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Set, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Index(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("vec", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Add, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Add, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("pos", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Parenthesis(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Coercion(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("size", common::Location::default())))
, typed : None}, Some(PreType { content : PreTypeInner::Ident(common::Ident::new("usize", common::Location::default()))})))
, typed : None}))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(2, None))
, typed : None}))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::BitOr, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Parenthesis(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Sub, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Sub, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("available", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("size", common::Location::default())))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(2, None))
, typed : None}))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("FREE_BIT", common::Location::default())))
, typed : None}))
, typed : None}))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Drop, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Set, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Index(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("vec", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Add, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Add, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("pos", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Parenthesis(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Coercion(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("size", common::Location::default())))
, typed : None}, Some(PreType { content : PreTypeInner::Ident(common::Ident::new("usize", common::Location::default()))})))
, typed : None}))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(3, None))
, typed : None}))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Coercion(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("pos", common::Location::default())))
, typed : None}, Some(PreType { content : PreTypeInner::Ident(common::Ident::new("u32", common::Location::default()))})))
, typed : None}))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Drop, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Set, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Index(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("vec", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Add, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Add, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("pos", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Parenthesis(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Coercion(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("available", common::Location::default())))
, typed : None}, Some(PreType { content : PreTypeInner::Ident(common::Ident::new("usize", common::Location::default()))})))
, typed : None}))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(3, None))
, typed : None}))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Add, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Add, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Coercion(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("pos", common::Location::default())))
, typed : None}, Some(PreType { content : PreTypeInner::Ident(common::Ident::new("u32", common::Location::default()))})))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("size", common::Location::default())))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(2, None))
, typed : None}))
, typed : None}))
, typed : None})},]}, Bloc { loc : common::Location::default(),
	content : vec![Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Keep, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Set, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("size", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("available", common::Location::default())))
, typed : None}))
, typed : None})},]}))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Drop, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Set, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Index(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("vec", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("pos", common::Location::default())))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Add, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("size", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(2, None))
, typed : None}))
, typed : None}))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Drop, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Set, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Index(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("vec", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Add, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("pos", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(1, None))
, typed : None}))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("previous", common::Location::default())))
, typed : None}))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Drop, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Return(Some(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Shl, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Parenthesis(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Add, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("pos", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(2, None))
, typed : None}))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(2, None))
, typed : None}))
, typed : None})))
, typed : None})},]}, Bloc { loc : common::Location::default(),
	content : vec![]}))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Drop, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Set, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("previous", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Coercion(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("pos", common::Location::default())))
, typed : None}, Some(PreType { content : PreTypeInner::Ident(common::Ident::new("u32", common::Location::default()))})))
, typed : None}))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Drop, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Set, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("pos", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Add, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Add, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("pos", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Coercion(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Index(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("vec", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("pos", common::Location::default())))
, typed : None}))
, typed : None}, Some(PreType { content : PreTypeInner::Ident(common::Ident::new("usize", common::Location::default()))})))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(1, None))
, typed : None}))
, typed : None}))
, typed : None})},]}, Bloc { loc : common::Location::default(),
	content : vec![Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Keep, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::If(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Eq, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Index(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("vec", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("pos", common::Location::default())))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(0, None))
, typed : None}))
, typed : None}, Bloc { loc : common::Location::default(),
	content : vec![Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Drop, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::MacroCall(common::Ident::new("print", common::Location::default()), vec![Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::String("an ommited string".to_string()))
, typed : None},]))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Drop, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Div, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(1, None))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::FunCall(vec![], common::Ident::new("const_zero", common::Location::default()), vec![]))
, typed : None}))
, typed : None})},]}, Bloc { loc : common::Location::default(),
	content : vec![]}))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Drop, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Set, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("previous", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Coercion(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("pos", common::Location::default())))
, typed : None}, Some(PreType { content : PreTypeInner::Ident(common::Ident::new("u32", common::Location::default()))})))
, typed : None}))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Drop, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Set, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("pos", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Add, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("pos", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Coercion(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Index(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("vec", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("pos", common::Location::default())))
, typed : None}))
, typed : None}, Some(PreType { content : PreTypeInner::Ident(common::Ident::new("usize", common::Location::default()))})))
, typed : None}))
, typed : None}))
, typed : None})},]}))
, typed : None})},]}))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Keep, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(0, None))
, typed : None})},]},
	id_counter : common::IdCounter::new(),
}),Decl::Fun(DeclFun {
	public : true,
	name : common::Ident::new("realloc", common::Location::default()),
self_arg : None,
	args : vec![(common::Ident::new("vec", common::Location::default()), false, PreType { content : PreTypeInner::Ref(true, Box::new(PreType { content : PreTypeInner::IdentParametrized(common::Ident::new("Vec", common::Location::default()), vec![PreType { content : PreTypeInner::Ident(common::Ident::new("u32", common::Location::default()))},])}))}),(common::Ident::new("pos", common::Location::default()), false, PreType { content : PreTypeInner::Ident(common::Ident::new("usize", common::Location::default()))}),(common::Ident::new("size", common::Location::default()), false, PreType { content : PreTypeInner::Ident(common::Ident::new("usize", common::Location::default()))}),],
	output : PreType { content : PreTypeInner::Ident(common::Ident::new("usize", common::Location::default()))},
	generics : vec![],
	content : Bloc { loc : common::Location::default(),
	content : vec![Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Keep, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::If(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Eq, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("pos", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("NULL_PTR", common::Location::default())))
, typed : None}))
, typed : None}, Bloc { loc : common::Location::default(),
	content : vec![Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Keep, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::FunCall(vec![], common::Ident::new("malloc", common::Location::default()), vec![Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("vec", common::Location::default())))
, typed : None},Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("size", common::Location::default())))
, typed : None},]))
, typed : None})},]}, Bloc { loc : common::Location::default(),
	content : vec![Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Keep, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::If(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Eq, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("size", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(0, None))
, typed : None}))
, typed : None}, Bloc { loc : common::Location::default(),
	content : vec![Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Drop, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::FunCall(vec![], common::Ident::new("free", common::Location::default()), vec![Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("vec", common::Location::default())))
, typed : None},Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("pos", common::Location::default())))
, typed : None},]))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Keep, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("NULL_PTR", common::Location::default())))
, typed : None})},]}, Bloc { loc : common::Location::default(),
	content : vec![Instr { loc : common::Location::default(),
	content : InstrInner::Binding(false, common::Ident::new("real_pos", common::Location::default()), None, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Sub, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Parenthesis(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Shr, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("pos", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(2, None))
, typed : None}))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(2, None))
, typed : None}))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Binding(false, common::Ident::new("next_pos", common::Location::default()), None, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Add, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("real_pos", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Coercion(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Index(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("vec", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("real_pos", common::Location::default())))
, typed : None}))
, typed : None}, Some(PreType { content : PreTypeInner::Ident(common::Ident::new("usize", common::Location::default()))})))
, typed : None}))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Binding(false, common::Ident::new("available", common::Location::default()), None, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::If(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Eq, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::BitAnd, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Index(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("vec", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("next_pos", common::Location::default())))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("FREE_BIT", common::Location::default())))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(1, None))
, typed : None}))
, typed : None}, Bloc { loc : common::Location::default(),
	content : vec![Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Keep, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Sub, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Add, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Index(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("vec", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("real_pos", common::Location::default())))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Index(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("vec", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("next_pos", common::Location::default())))
, typed : None}))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(1, None))
, typed : None}))
, typed : None})},]}, Bloc { loc : common::Location::default(),
	content : vec![Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Keep, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Sub, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Index(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("vec", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("real_pos", common::Location::default())))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(2, None))
, typed : None}))
, typed : None})},]}))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Binding(false, common::Ident::new("new_size", common::Location::default()), None, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Shl, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Shr, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Parenthesis(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Add, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("size", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(7, None))
, typed : None}))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(3, None))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(1, None))
, typed : None}))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Keep, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::If(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::And, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::GreaterEq, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Coercion(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("available", common::Location::default())))
, typed : None}, Some(PreType { content : PreTypeInner::Ident(common::Ident::new("usize", common::Location::default()))})))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("size", common::Location::default())))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Bool(false))
, typed : None}))
, typed : None}, Bloc { loc : common::Location::default(),
	content : vec![Instr { loc : common::Location::default(),
	content : InstrInner::Binding(false, common::Ident::new("last_pos", common::Location::default()), None, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Add, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("next_pos", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Coercion(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Parenthesis(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Add, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Parenthesis(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::BitOr, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Index(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("vec", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("next_pos", common::Location::default())))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("FREE_BIT", common::Location::default())))
, typed : None}))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("FREE_BIT", common::Location::default())))
, typed : None}))
, typed : None}))
, typed : None}, Some(PreType { content : PreTypeInner::Ident(common::Ident::new("usize", common::Location::default()))})))
, typed : None}))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Drop, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::MacroCall(common::Ident::new("print", common::Location::default()), vec![Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::String("an ommited string".to_string()))
, typed : None},]))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Keep, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Div, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(1, None))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::FunCall(vec![], common::Ident::new("const_zero", common::Location::default()), vec![]))
, typed : None}))
, typed : None})},]}, Bloc { loc : common::Location::default(),
	content : vec![Instr { loc : common::Location::default(),
	content : InstrInner::Binding(false, common::Ident::new("new_pos", common::Location::default()), None, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Shr, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::FunCall(vec![], common::Ident::new("malloc", common::Location::default()), vec![Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("vec", common::Location::default())))
, typed : None},Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("size", common::Location::default())))
, typed : None},]))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(2, None))
, typed : None}))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Binding(true, common::Ident::new("offset", common::Location::default()), None, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(0, None))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Keep, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::While(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Lower, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("offset", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Parenthesis(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Sub, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Coercion(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Index(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("vec", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("real_pos", common::Location::default())))
, typed : None}))
, typed : None}, Some(PreType { content : PreTypeInner::Ident(common::Ident::new("usize", common::Location::default()))})))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(2, None))
, typed : None}))
, typed : None}))
, typed : None}))
, typed : None}, Bloc { loc : common::Location::default(),
	content : vec![Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Drop, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Set, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Index(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("vec", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Add, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("new_pos", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("offset", common::Location::default())))
, typed : None}))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Index(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("vec", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Add, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Parenthesis(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Shr, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("pos", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(2, None))
, typed : None}))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("offset", common::Location::default())))
, typed : None}))
, typed : None}))
, typed : None}))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Drop, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Set, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("offset", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Add, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("offset", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(1, None))
, typed : None}))
, typed : None}))
, typed : None})},]}))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Drop, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::FunCall(vec![], common::Ident::new("free", common::Location::default()), vec![Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("vec", common::Location::default())))
, typed : None},Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("pos", common::Location::default())))
, typed : None},]))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Keep, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Shl, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("new_pos", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(2, None))
, typed : None}))
, typed : None})},]}))
, typed : None})},]}))
, typed : None})},]}))
, typed : None})},]},
	id_counter : common::IdCounter::new(),
}),Decl::Fun(DeclFun {
	public : true,
	name : common::Ident::new("free", common::Location::default()),
self_arg : None,
	args : vec![(common::Ident::new("vec", common::Location::default()), false, PreType { content : PreTypeInner::Ref(true, Box::new(PreType { content : PreTypeInner::IdentParametrized(common::Ident::new("Vec", common::Location::default()), vec![PreType { content : PreTypeInner::Ident(common::Ident::new("u32", common::Location::default()))},])}))}),(common::Ident::new("pos", common::Location::default()), false, PreType { content : PreTypeInner::Ident(common::Ident::new("usize", common::Location::default()))}),],
	output : PreType { content : PreTypeInner::Tuple(vec![])},
	generics : vec![],
	content : Bloc { loc : common::Location::default(),
	content : vec![Instr { loc : common::Location::default(),
	content : InstrInner::Binding(true, common::Ident::new("real_pos", common::Location::default()), None, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Sub, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Parenthesis(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Shr, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("pos", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(2, None))
, typed : None}))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(2, None))
, typed : None}))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Keep, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::If(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Eq, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::BitAnd, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Coercion(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Index(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("vec", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("real_pos", common::Location::default())))
, typed : None}))
, typed : None}, Some(PreType { content : PreTypeInner::Ident(common::Ident::new("u32", common::Location::default()))})))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("FREE_BIT", common::Location::default())))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(1, None))
, typed : None}))
, typed : None}, Bloc { loc : common::Location::default(),
	content : vec![Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Drop, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::MacroCall(common::Ident::new("print", common::Location::default()), vec![Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::String("an ommited string".to_string()))
, typed : None},]))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Drop, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Div, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(1, None))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::FunCall(vec![], common::Ident::new("const_zero", common::Location::default()), vec![]))
, typed : None}))
, typed : None})},]}, Bloc { loc : common::Location::default(),
	content : vec![Instr { loc : common::Location::default(),
	content : InstrInner::Binding(true, common::Ident::new("last_pos", common::Location::default()), None, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Add, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("real_pos", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Coercion(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Index(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("vec", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("real_pos", common::Location::default())))
, typed : None}))
, typed : None}, Some(PreType { content : PreTypeInner::Ident(common::Ident::new("usize", common::Location::default()))})))
, typed : None}))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Drop, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::If(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Eq, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::BitAnd, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Index(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("vec", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("last_pos", common::Location::default())))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("FREE_BIT", common::Location::default())))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(1, None))
, typed : None}))
, typed : None}, Bloc { loc : common::Location::default(),
	content : vec![Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Drop, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Set, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("last_pos", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Add, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Add, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("last_pos", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Coercion(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Index(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("vec", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("last_pos", common::Location::default())))
, typed : None}))
, typed : None}, Some(PreType { content : PreTypeInner::Ident(common::Ident::new("usize", common::Location::default()))})))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(1, None))
, typed : None}))
, typed : None}))
, typed : None})},]}, Bloc { loc : common::Location::default(),
	content : vec![]}))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Binding(false, common::Ident::new("previous_pos", common::Location::default()), None, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Coercion(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Index(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("vec", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Add, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("real_pos", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(1, None))
, typed : None}))
, typed : None}))
, typed : None}, Some(PreType { content : PreTypeInner::Ident(common::Ident::new("usize", common::Location::default()))})))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Keep, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::If(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Eq, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::BitAnd, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Index(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("vec", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("previous_pos", common::Location::default())))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("FREE_BIT", common::Location::default())))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(1, None))
, typed : None}))
, typed : None}, Bloc { loc : common::Location::default(),
	content : vec![Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Keep, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Set, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("real_pos", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("previous_pos", common::Location::default())))
, typed : None}))
, typed : None})},]}, Bloc { loc : common::Location::default(),
	content : vec![]}))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Drop, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Set, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Index(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("vec", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Add, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("last_pos", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(1, None))
, typed : None}))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Coercion(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("real_pos", common::Location::default())))
, typed : None}, Some(PreType { content : PreTypeInner::Ident(common::Ident::new("u32", common::Location::default()))})))
, typed : None}))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Drop, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Set, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Index(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("vec", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("real_pos", common::Location::default())))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::BitOr, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Coercion(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Parenthesis(Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Sub, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::BinaryOp(common::BinOperator::Sub, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("last_pos", common::Location::default())))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("real_pos", common::Location::default())))
, typed : None}))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(2, None))
, typed : None}))
, typed : None}))
, typed : None}, Some(PreType { content : PreTypeInner::Ident(common::Ident::new("u32", common::Location::default()))})))
, typed : None}, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("FREE_BIT", common::Location::default())))
, typed : None}))
, typed : None}))
, typed : None})},]}))
, typed : None})},]},
	id_counter : common::IdCounter::new(),
}),Decl::Fun(DeclFun {
	public : true,
	name : common::Ident::new("testing", common::Location::default()),
self_arg : None,
	args : vec![],
	output : PreType { content : PreTypeInner::Tuple(vec![])},
	generics : vec![],
	content : Bloc { loc : common::Location::default(),
	content : vec![Instr { loc : common::Location::default(),
	content : InstrInner::Binding(true, common::Ident::new("array", common::Location::default()), None, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::MacroCall(common::Ident::new("vec", common::Location::default()), vec![Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(42, None))
, typed : None},Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(42, None))
, typed : None},Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(42, None))
, typed : None},Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(42, None))
, typed : None},Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(42, None))
, typed : None},Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(42, None))
, typed : None},Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(42, None))
, typed : None},Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(42, None))
, typed : None},Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(42, None))
, typed : None},Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(42, None))
, typed : None},Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(42, None))
, typed : None},Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(42, None))
, typed : None},Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(42, None))
, typed : None},Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(42, None))
, typed : None},Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(42, None))
, typed : None},Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(42, None))
, typed : None},Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(42, None))
, typed : None},Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(42, None))
, typed : None},Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(42, None))
, typed : None},Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(42, None))
, typed : None},]))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Drop, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::FunCall(vec![], common::Ident::new("init", common::Location::default()), vec![Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Ref(true, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("array", common::Location::default())))
, typed : None}))
, typed : None},]))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Drop, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::MacroCall(common::Ident::new("print", common::Location::default()), vec![Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::String("an ommited string".to_string()))
, typed : None},]))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Binding(false, common::Ident::new("id1", common::Location::default()), None, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::FunCall(vec![], common::Ident::new("malloc", common::Location::default()), vec![Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Ref(true, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("array", common::Location::default())))
, typed : None}))
, typed : None},Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(2, None))
, typed : None},]))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Drop, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::MacroCall(common::Ident::new("print", common::Location::default()), vec![Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::String("an ommited string".to_string()))
, typed : None},]))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Binding(false, common::Ident::new("id2", common::Location::default()), None, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::FunCall(vec![], common::Ident::new("malloc", common::Location::default()), vec![Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Ref(true, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("array", common::Location::default())))
, typed : None}))
, typed : None},Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(1, None))
, typed : None},]))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Drop, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::MacroCall(common::Ident::new("print", common::Location::default()), vec![Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::String("an ommited string".to_string()))
, typed : None},]))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Binding(false, common::Ident::new("id3", common::Location::default()), None, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::FunCall(vec![], common::Ident::new("malloc", common::Location::default()), vec![Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Ref(true, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("array", common::Location::default())))
, typed : None}))
, typed : None},Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Int(3, None))
, typed : None},]))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Drop, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::MacroCall(common::Ident::new("print", common::Location::default()), vec![Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::String("an ommited string".to_string()))
, typed : None},]))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Drop, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::FunCall(vec![], common::Ident::new("free", common::Location::default()), vec![Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Ref(true, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("array", common::Location::default())))
, typed : None}))
, typed : None},Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("id2", common::Location::default())))
, typed : None},]))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Drop, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::MacroCall(common::Ident::new("print", common::Location::default()), vec![Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::String("an ommited string".to_string()))
, typed : None},]))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Drop, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::FunCall(vec![], common::Ident::new("free", common::Location::default()), vec![Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Ref(true, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("array", common::Location::default())))
, typed : None}))
, typed : None},Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::Var(common::Ident::new("id3", common::Location::default())))
, typed : None},]))
, typed : None})},Instr { loc : common::Location::default(),
	content : InstrInner::Expr(common::ComputedValue::Drop, Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::MacroCall(common::Ident::new("print", common::Location::default()), vec![Expr { loc : common::Location::default(),
	content : Box::new(ExprInner::String("an ommited string".to_string()))
, typed : None},]))
, typed : None})},]},
	id_counter : common::IdCounter::new(),
}),],
	dep : vec![],
	err_reporter : common::ErrorReporter::empty(),
}
;
Module::build(file, HashMap::new())
}));
Module::build(file, submodules)
})}
