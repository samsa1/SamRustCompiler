use crate::ast::common::*;
use crate::ast::low_level_repr as llr;
use crate::ast::operators::*;
use crate::ast::typed_rust as tr;
use crate::frontend::Module;
use crate::typing::context::ModuleInterface;
use std::collections::HashMap;

pub struct VecInfo {
    pub new: HashMap<usize, usize>,
    pub push: HashMap<usize, usize>,
    pub len: HashMap<usize, usize>,
    pub get: HashMap<usize, usize>,
}

#[derive(Debug, Clone)]
struct StructInfo {
    size: usize,
    binding: HashMap<String, (usize, usize)>,
}

impl StructInfo {
    fn new(size: usize) -> Self {
        Self {
            size,
            binding: HashMap::new(),
        }
    }

    fn new_full(binding: HashMap<String, (usize, usize)>, size: usize) -> Self {
        Self { size, binding }
    }

    fn get_size(&self) -> usize {
        self.size
    }

    fn get_pos(&self, id: &str) -> usize {
        match self.binding.get(id) {
            Some((pos, _size)) => *pos,
            None => panic!("ICE"),
        }
    }

    fn get_proj_size(&self, id: &str) -> usize {
        match self.binding.get(id) {
            Some((_pos, size)) => *size,
            None => panic!("ICE"),
        }
    }
}

#[derive(Debug, Clone)]
struct EnumInfo {
    size: usize,
}

impl EnumInfo {
    fn new(size: usize) -> Self {
        Self { size }
    }

    fn get_size(&self) -> usize {
        self.size
    }
}

struct DataStruct {
    max_id: usize,
    vars: Vec<HashMap<String, usize>>,
    structs: HashMap<PathUL<()>, StructInfo>,
    enums: HashMap<PathUL<()>, EnumInfo>,
    strings_count: usize,
    string_extend: String,
    strings_stored: HashMap<String, String>,
    pointer_size: usize,
}

fn build_structs_1(
    modint: &ModuleInterface,
    structs: &mut HashMap<PathUL<()>, StructInfo>,
    enums: &mut HashMap<PathUL<()>, EnumInfo>,
    path: &mut PathUL<()>,
) {
    for (name, struct_info) in &modint.structs {
        path.push(NamePath::Name(name.to_string()));
        assert!(structs
            .insert(path.clone(), StructInfo::new(struct_info.get_size()))
            .is_none());
        path.pop();
    }
    for (name, enum_info) in &modint.enums {
        path.push(NamePath::Name(name.to_string()));
        assert!(enums
            .insert(path.clone(), EnumInfo::new(enum_info.get_size()))
            .is_none());
        path.pop();
    }

    for (name, (_, modint)) in &modint.submodules {
        path.push(NamePath::Name(name.to_string()));
        build_structs_1(modint, structs, enums, path);
        path.pop();
    }
}

fn build_structs_2(
    modint: ModuleInterface,
    data_struct: &DataStruct,
    structs: &mut HashMap<PathUL<()>, StructInfo>,
    path: &mut PathUL<()>,
) {
    for (name, struct_info) in modint.structs {
        let mut offset = 0;
        let mut binding = HashMap::new();
        let size = struct_info.get_size();
        for (argname, (_, typ)) in struct_info.args() {
            let size = data_struct.compute_size(&typ);
            binding.insert(argname, (offset, size));
            offset += size;
        }
        assert_eq!(offset, size);
        path.push(NamePath::Name(name.clone()));
        assert!(structs
            .insert(path.clone(), StructInfo::new_full(binding, offset))
            .is_none());
        path.pop();
    }

    for (name, (_, modint)) in modint.submodules {
        path.push(NamePath::Name(name));
        build_structs_2(modint, data_struct, structs, path);
        path.pop();
    }
}

impl DataStruct {
    fn new(modint: ModuleInterface, string_extend: String) -> Self {
        let mut structs = HashMap::new();
        let mut enums = HashMap::new();
        build_structs_1(
            &modint,
            &mut structs,
            &mut enums,
            &mut PathUL::new(Vec::new()),
        );
        let mut data_struct = Self {
            max_id: 0,
            vars: vec![HashMap::new()],
            structs,
            enums,
            strings_count: 0,
            string_extend,
            strings_stored: HashMap::new(),
            pointer_size: 8,
        };
        let mut structs = HashMap::new();

        build_structs_2(
            modint,
            &data_struct,
            &mut structs,
            &mut PathUL::new(Vec::new()),
        );
        data_struct.structs = structs;
        data_struct
    }

    fn export_strings(self) -> HashMap<String, String> {
        self.strings_stored
            .into_iter()
            .map(|(k, s)| (k, format!("user_string....{s}")))
            .collect()
    }

    fn reset(&mut self) {
        self.max_id = 0;
        self.vars = vec![HashMap::new()]
    }

    fn add_layer(&mut self) {
        self.vars.push(HashMap::new())
    }

    fn pop_layer(&mut self) {
        assert!(self.vars.pop().is_some());
    }

    fn insert(&mut self, name: String) -> usize {
        let id = self.max_id;
        self.max_id += 1;
        match self.vars.last_mut() {
            Some(layer) => assert!(layer.insert(name, id).is_none()),
            None => panic!("ICE"),
        };
        id
    }

    fn get_var(&self, str: &str) -> Option<usize> {
        for layer in self.vars.iter().rev() {
            match layer.get(str) {
                Some(id) => return Some(*id),
                None => (),
            }
        }
        None
    }

    fn insert_string(&mut self, str: String) -> String {
        match self.strings_stored.get(&str) {
            Some(str) => return str.to_string(),
            None => (),
        };
        let str2 = format!("rust_string_{}_{}", self.string_extend, self.strings_count);
        self.strings_count += 1;
        self.strings_stored.insert(str, str2.clone());
        str2
    }

    fn get_struct_info(&self, name: &PathUL<()>) -> &StructInfo {
        self.structs.get(name).unwrap()
    }

    fn compute_size(&self, typ: &tr::PostType) -> usize {
        match &typ.content {
            tr::PostTypeInner::Box(_)
            | tr::PostTypeInner::Fun(_, _, _)
            | tr::PostTypeInner::Ref(_, _)
            | tr::PostTypeInner::String => self.pointer_size,
            tr::PostTypeInner::BuiltIn(BuiltinType::Bool) => 1,
            tr::PostTypeInner::BuiltIn(BuiltinType::Int(_, s)) => s.to_byte_size(),
            tr::PostTypeInner::Diverge => 0,
            tr::PostTypeInner::FreeType(_) => todo!(),
            tr::PostTypeInner::Struct(name, _) if name.is_vec() => self.pointer_size,
            tr::PostTypeInner::Struct(name, _) => self.structs.get(name).unwrap().get_size(),
            tr::PostTypeInner::Enum(name, _) => self.enums.get(name).unwrap().get_size(),
            tr::PostTypeInner::Tuple(exprs) => {
                let mut total = 0;
                for expr in exprs {
                    total += self.compute_size(expr);
                }
                total
            }
        }
    }

    fn get_pointer_size(&self) -> usize {
        self.pointer_size
    }

    fn compute_offset(&self, typ: &tr::PostType, id: usize) -> (usize, usize) {
        match &typ.content {
            tr::PostTypeInner::Tuple(exprs) => {
                let mut offset = 0;
                for sub_typ in exprs.iter().take(id) {
                    offset += self.compute_size(sub_typ)
                }
                (self.compute_size(&exprs[id]), offset)
            }

            tr::PostTypeInner::Ref(_, typ) => match &typ.content {
                tr::PostTypeInner::Tuple(exprs) => {
                    let mut offset = 0;
                    for sub_typ in exprs.iter().take(id) {
                        offset += self.compute_size(sub_typ)
                    }
                    (self.compute_size(&exprs[id]), offset)
                }
                _ => panic!("ICE"),
            },

            _ => panic!("ICE"),
        }
    }
}

fn eval(op: TBinop, v1: &llr::Value, v2: &llr::Value) -> Option<llr::Value> {
    use crate::ast::operators::LArith::*;
    use crate::ast::operators::Logic::*;
    use crate::ast::operators::Shift::*;
    use llr::Value::*;
    match (op, v1, v2) {
        (TBinop::LArith(Add(s)), SInt(i1, _), SInt(i2, _)) => Some(SInt(i1 + i2, s)),
        (TBinop::LArith(Add(s)), UInt(i1, _), UInt(i2, _)) => Some(UInt(i1 + i2, s)),
        (TBinop::LArith(Sub(s)), SInt(i1, _), SInt(i2, _)) => Some(SInt(*i1 - *i2, s)),
        (TBinop::LArith(Sub(s)), UInt(i1, _), UInt(i2, _)) => Some(UInt(*i1 - *i2, s)),
        (
            TBinop::HArith(HArith {
                dm: HArithDesc::Mul,
                signed: false,
                size,
            }),
            UInt(i1, _),
            UInt(i2, _),
        ) => Some(UInt((*i1 * *i2) as u64, size)),
        (
            TBinop::HArith(HArith {
                dm: HArithDesc::Mul,
                signed: true,
                size,
            }),
            SInt(i1, _),
            SInt(i2, _),
        ) => Some(SInt(*i1 * *i2, size)),
        (
            TBinop::HArith(HArith {
                dm: HArithDesc::Div,
                signed: true,
                size,
            }),
            SInt(i1, _),
            SInt(i2, _),
        ) => {
            if v2.imm() == 0 {
                None
            } else {
                Some(SInt(*i1 / *i2, size))
            }
        }
        (
            TBinop::HArith(HArith {
                dm: HArithDesc::Div,
                signed: false,
                size,
            }),
            UInt(i1, _),
            UInt(i2, _),
        ) => {
            if *i2 == 0 {
                panic!("ICE")
            } else {
                Some(UInt(*i1 / *i2, size))
            }
        }
        (
            TBinop::HArith(HArith {
                dm: HArithDesc::Mod,
                signed: true,
                size,
            }),
            SInt(i1, _),
            SInt(i2, _),
        ) => {
            if v2.imm() == 0 {
                panic!("ICE")
            } else {
                Some(SInt(*i1 % *i2, size))
            }
        }
        (
            TBinop::HArith(HArith {
                dm: HArithDesc::Mod,
                signed: false,
                size,
            }),
            UInt(i1, _),
            UInt(i2, _),
        ) => {
            if *i2 == 0 {
                panic!("ICE")
            } else {
                Some(UInt(*i1 % *i2, size))
            }
        }
        (TBinop::Logic(LAnd), Bool(b1), Bool(b2)) => Some(Bool(*b1 && *b2)),
        (TBinop::Logic(LOr), Bool(b1), Bool(b2)) => Some(Bool(*b1 || *b2)),
        (TBinop::LArith(And(s)), SInt(i1, _), SInt(i2, _)) => Some(SInt(*i1 & *i2, s)),
        (TBinop::LArith(And(s)), UInt(i1, _), UInt(i2, _)) => Some(UInt(*i1 & *i2, s)),
        (TBinop::LArith(Or(s)), SInt(i1, _), SInt(i2, _)) => Some(SInt(*i1 | *i2, s)),
        (TBinop::LArith(Or(s)), UInt(i1, _), UInt(i2, _)) => Some(UInt(*i1 | *i2, s)),
        (TBinop::Shift(Shl(s)), SInt(i1, _), SInt(i2, _)) => Some(SInt(*i1 << *i2, s)),
        (TBinop::Shift(Shl(s)), UInt(i1, _), UInt(i2, _)) => Some(UInt(*i1 << *i2, s)),
        (TBinop::Shift(Shr(s)), SInt(i1, _), SInt(i2, _)) => Some(SInt(*i1 >> *i2, s)),
        (TBinop::Shift(Shr(s)), UInt(i1, _), UInt(i2, _)) => Some(UInt(*i1 >> *i2, s)),
        (
            TBinop::Cmp(Cmp {
                cmp: CmpDesc::Eq, ..
            }),
            Bool(b1),
            Bool(b2),
        ) => Some(Bool(*b1 == *b2)),
        (
            TBinop::Cmp(Cmp {
                cmp: CmpDesc::Eq, ..
            }),
            SInt(i1, _),
            SInt(i2, _),
        ) => Some(Bool(*i1 == *i2)),
        (
            TBinop::Cmp(Cmp {
                cmp: CmpDesc::Eq, ..
            }),
            UInt(i1, _),
            UInt(i2, _),
        ) => Some(Bool(*i1 == *i2)),
        (
            TBinop::Cmp(Cmp {
                cmp: CmpDesc::Neq, ..
            }),
            Bool(b1),
            Bool(b2),
        ) => Some(Bool(*b1 != *b2)),
        (
            TBinop::Cmp(Cmp {
                cmp: CmpDesc::Neq, ..
            }),
            SInt(i1, _),
            SInt(i2, _),
        ) => Some(Bool(*i1 != *i2)),
        (
            TBinop::Cmp(Cmp {
                cmp: CmpDesc::Neq, ..
            }),
            UInt(i1, _),
            UInt(i2, _),
        ) => Some(Bool(*i1 != *i2)),
        (
            TBinop::Cmp(Cmp {
                cmp: CmpDesc::Lower,
                ..
            }),
            SInt(i1, _),
            SInt(i2, _),
        ) => Some(Bool(*i1 < *i2)),
        (
            TBinop::Cmp(Cmp {
                cmp: CmpDesc::Lower,
                ..
            }),
            UInt(i1, _),
            UInt(i2, _),
        ) => Some(Bool(*i1 < *i2)),
        (
            TBinop::Cmp(Cmp {
                cmp: CmpDesc::LowerEq,
                ..
            }),
            SInt(i1, _),
            SInt(i2, _),
        ) => Some(Bool(*i1 <= *i2)),
        (
            TBinop::Cmp(Cmp {
                cmp: CmpDesc::LowerEq,
                ..
            }),
            UInt(i1, _),
            UInt(i2, _),
        ) => Some(Bool(*i1 <= *i2)),
        (
            TBinop::Cmp(Cmp {
                cmp: CmpDesc::Greater,
                ..
            }),
            SInt(i1, _),
            SInt(i2, _),
        ) => Some(Bool(*i1 > *i2)),
        (
            TBinop::Cmp(Cmp {
                cmp: CmpDesc::Greater,
                ..
            }),
            UInt(i1, _),
            UInt(i2, _),
        ) => Some(Bool(*i1 > *i2)),
        (
            TBinop::Cmp(Cmp {
                cmp: CmpDesc::GreaterEq,
                ..
            }),
            SInt(i1, _),
            SInt(i2, _),
        ) => Some(Bool(*i1 >= *i2)),
        (
            TBinop::Cmp(Cmp {
                cmp: CmpDesc::GreaterEq,
                ..
            }),
            UInt(i1, _),
            UInt(i2, _),
        ) => Some(Bool(*i1 >= *i2)),
        (_, _, _) => panic!("ICE eval({:?}, {:?}, {:?})", op, v1, v2),
    }
}

fn rewrite_expr(top_expr: tr::Expr, names_info: &mut DataStruct) -> llr::Expr {
    match *top_expr.content {
        tr::ExprInner::PatternMatching(_, _, _) | tr::ExprInner::TraitFun(_, _, _, _) => {
            panic!("Case should be handled by previous passes")
        }

        tr::ExprInner::Bloc(bloc) => llr::Expr {
            content: Box::new(llr::ExprInner::Bloc(rewrite_bloc(bloc, names_info))),
            loc: top_expr.loc,
            size: names_info.compute_size(&top_expr.typed),
            typed: top_expr.typed,
        },
        tr::ExprInner::Bool(b) => llr::Expr {
            content: Box::new(llr::ExprInner::Value(llr::Value::Bool(b))),
            loc: top_expr.loc,
            typed: top_expr.typed,
            size: 1,
        },
        tr::ExprInner::BuildStruct(name, args) => {
            let struct_info = names_info.get_struct_info(&name).clone();
            let args2 = args
                .into_iter()
                .map(|(id, expr)| {
                    (
                        struct_info.get_pos(id.get_content()),
                        rewrite_expr(expr, names_info),
                    )
                })
                .collect();
            llr::Expr {
                content: Box::new(llr::ExprInner::BuildStruct(struct_info.get_size(), args2)),
                loc: top_expr.loc,
                typed: top_expr.typed,
                size: struct_info.get_size(),
            }
        }
        tr::ExprInner::Constructor(_, _) => todo!(),
        tr::ExprInner::Deref(expr) => llr::Expr {
            content: Box::new(llr::ExprInner::Deref(rewrite_expr(expr, names_info))),
            loc: top_expr.loc,
            size: names_info.compute_size(&top_expr.typed),
            typed: top_expr.typed,
        },

        tr::ExprInner::FunCall(id, args) => {
            let args: Vec<llr::Expr> = args
                .into_iter()
                .map(|e| rewrite_expr(e, names_info))
                .collect();
            let expr_inner = match names_info.get_var(id.get_content()) {
                Some(id) => llr::ExprInner::FunCallVar(id, args),
                None => panic!("ICE {:?}", id),
            };
            llr::Expr {
                content: Box::new(expr_inner),
                loc: top_expr.loc,
                size: names_info.compute_size(&top_expr.typed),
                typed: top_expr.typed,
            }
        }

        tr::ExprInner::FunCallPath(spec, id, args) => {
            assert!(spec.is_empty());
            let args: Vec<llr::Expr> = args
                .into_iter()
                .map(|e| rewrite_expr(e, names_info))
                .collect();
            let path = id.get_content();
            let args2_bis = if path.len() != 4 {
                args
            } else {
                match (&path[0], &path[1], &path[2], &path[3]) {
                    (
                        NamePath::Name(n1),
                        NamePath::Name(n2),
                        NamePath::Name(n3),
                        NamePath::Name(n4),
                    ) => {
                        if n1 == "std" && n2 == "vec" && n3 == "Vec" && false {
                            if n4 == "push" && args.len() == 2 {
                                args.into_iter().rev().collect()
                            } else if n4 == "new" && args.is_empty() {
                                let size = match &top_expr.typed.content {
                                    tr::PostTypeInner::Struct(name, arg1)
                                        if name.is_vec() && arg1.len() == 1 =>
                                    {
                                        names_info.compute_size(&arg1[0])
                                    }
                                    _ => panic!("ICE"),
                                };
                                vec![llr::Expr::new_usize(size as u64)]
                            } else {
                                args
                            }
                        } else {
                            args
                        }
                    }
                    _ => panic!("ICE Specialisation are not handled here"),
                }
            };
            let expr_inner = llr::ExprInner::FunCall(id, args2_bis);
            llr::Expr {
                content: Box::new(expr_inner),
                loc: top_expr.loc,
                size: names_info.compute_size(&top_expr.typed),
                typed: top_expr.typed,
            }
        }

        tr::ExprInner::If(expr, bloc1, bloc2) => llr::Expr {
            content: Box::new(llr::ExprInner::If(
                rewrite_expr(expr, names_info),
                rewrite_bloc(bloc1, names_info),
                rewrite_bloc(bloc2, names_info),
            )),
            loc: top_expr.loc,
            size: names_info.compute_size(&top_expr.typed),
            typed: top_expr.typed,
        },
        tr::ExprInner::Int(i) => {
            let (signed, size) = top_expr.typed.get_int_size().unwrap();
            let v = if signed {
                llr::Value::SInt(i as i64, size)
            } else {
                llr::Value::UInt(i, size)
            };
            llr::Expr {
                content: Box::new(llr::ExprInner::Value(v)),
                loc: top_expr.loc,
                size: size.to_byte_size(),
                typed: top_expr.typed,
            }
        }
        tr::ExprInner::Print(str) => {
            let label = names_info.insert_string(str);
            llr::Expr {
                content: Box::new(llr::ExprInner::Print(PathUL::from_vec(vec!["", &label]))),
                loc: top_expr.loc,
                typed: top_expr.typed,
                size: 0,
            }
        }

        tr::ExprInner::PrintPtr(expr) => {
            println!("print_ptr => {:?}", expr);
            llr::Expr {
                content: Box::new(llr::ExprInner::FunCall(
                    PathUL::from_vec(vec!["", "print_ptr"]),
                    vec![rewrite_expr(expr, names_info)],
                )),
                loc: top_expr.loc,
                typed: top_expr.typed,
                size: 0,
            }
        }

        tr::ExprInner::Proj(expr, Projector::Int(id)) => {
            let (size, id) = names_info.compute_offset(&expr.typed, id);
            llr::Expr {
                content: Box::new(llr::ExprInner::Proj(rewrite_expr(expr, names_info), id)),
                loc: top_expr.loc,
                typed: top_expr.typed,
                size,
            }
        }

        tr::ExprInner::Proj(expr, Projector::Name(id)) => {
            let struct_info = match &expr.typed.get_struct() {
                Some((struct_name, l)) => {
                    assert!(l.is_empty());
                    names_info.get_struct_info(struct_name)
                }
                _ => panic!("ICE"),
            };
            let size = struct_info.get_proj_size(id.get_content());
            let id = struct_info.get_pos(id.get_content());
            llr::Expr {
                content: Box::new(llr::ExprInner::Proj(rewrite_expr(expr, names_info), id)),
                loc: top_expr.loc,
                typed: top_expr.typed,
                size,
            }
        }

        tr::ExprInner::Ref(_, expr) => llr::Expr {
            content: Box::new(llr::ExprInner::Ref(rewrite_expr(expr, names_info))),
            loc: top_expr.loc,
            typed: top_expr.typed,
            size: names_info.get_pointer_size(),
        },
        tr::ExprInner::Set(expr1, expr2) => llr::Expr {
            content: Box::new(llr::ExprInner::Set(
                names_info.compute_size(&expr2.typed),
                rewrite_expr(expr1, names_info),
                rewrite_expr(expr2, names_info),
            )),
            loc: top_expr.loc,
            typed: top_expr.typed,
            size: 0,
        },
        tr::ExprInner::String(str) => {
            let label = names_info.insert_string(str);
            llr::Expr {
                content: Box::new(llr::ExprInner::Constant(PathUL::from_vec(vec!["", &label]))),
                loc: top_expr.loc,
                typed: top_expr.typed,
                size: names_info.get_pointer_size(),
            }
            .to_ref(names_info.get_pointer_size())
        }
        tr::ExprInner::Tuple(exprs, _) => llr::Expr {
            content: Box::new(llr::ExprInner::Tuple(
                names_info.compute_size(&top_expr.typed),
                exprs
                    .into_iter()
                    .map(|e| rewrite_expr(e, names_info))
                    .collect(),
            )),
            loc: top_expr.loc,
            size: names_info.compute_size(&top_expr.typed),
            typed: top_expr.typed,
        },

        tr::ExprInner::Var(var_name) => match names_info.get_var(var_name.get_content()) {
            Some(id) => llr::Expr {
                content: Box::new(llr::ExprInner::VarId(id)),
                loc: top_expr.loc,
                size: names_info.compute_size(&top_expr.typed),
                typed: top_expr.typed,
            },
            None => panic!("ICE"),
        },

        tr::ExprInner::VarPath(var_name) => llr::Expr {
            content: Box::new(llr::ExprInner::FunVar(var_name)),
            loc: top_expr.loc,
            size: names_info.compute_size(&top_expr.typed),
            typed: top_expr.typed,
        },

        tr::ExprInner::BinOp(op, expr1, expr2) => {
            let e1 = rewrite_expr(expr1, names_info);
            let e2 = rewrite_expr(expr2, names_info);
            let content = match (&*e1.content, &*e2.content) {
                (llr::ExprInner::Value(v1), llr::ExprInner::Value(v2)) => match eval(op, v1, v2) {
                    Some(v) if v.valid() => llr::ExprInner::Value(v),
                    _ => llr::ExprInner::BinOp(op, e1, e2),
                },
                (llr::ExprInner::Value(v1), _) if op.can_unary() && v1.valid() => {
                    llr::ExprInner::UnaOp(llr::UnaOp::Binary(op, *v1, llr::Pos::Left), e2)
                }
                (_, llr::ExprInner::Value(v2)) if op.can_unary() && v2.valid() => {
                    llr::ExprInner::UnaOp(llr::UnaOp::Binary(op, *v2, llr::Pos::Right), e1)
                }
                (_, _) => llr::ExprInner::BinOp(op, e1, e2),
            };
            llr::Expr {
                content: Box::new(content),
                loc: top_expr.loc,
                size: names_info.compute_size(&top_expr.typed),
                typed: top_expr.typed,
            }
        }

        tr::ExprInner::UnaOp(op, expr) => llr::Expr {
            content: Box::new(llr::ExprInner::UnaOp(
                llr::UnaOp::Unary(op),
                rewrite_expr(expr, names_info),
            )),
            loc: top_expr.loc,
            size: names_info.compute_size(&top_expr.typed),
            typed: top_expr.typed,
        },

        tr::ExprInner::Coercion(expr, typ1, typ2) => llr::Expr {
            content: Box::new(llr::ExprInner::Coercion(
                rewrite_expr(expr, names_info),
                typ1,
                typ2,
            )),
            loc: top_expr.loc,
            size: typ2.to_byte_size(),
            typed: top_expr.typed,
        },

        tr::ExprInner::Return(opt) => llr::Expr {
            content: Box::new(llr::ExprInner::Return(
                opt.map(|e| rewrite_expr(e, names_info)),
            )),
            loc: top_expr.loc,
            size: names_info.compute_size(&top_expr.typed),
            typed: top_expr.typed,
        },

        tr::ExprInner::While(cond, bloc) => llr::Expr {
            content: Box::new(llr::ExprInner::While(
                rewrite_expr(cond, names_info),
                rewrite_bloc(bloc, names_info),
            )),
            loc: top_expr.loc,
            size: names_info.compute_size(&top_expr.typed),
            typed: top_expr.typed,
        },
    }
}

fn rewrite_bloc(bloc: tr::Bloc, names_info: &mut DataStruct) -> llr::Bloc {
    names_info.add_layer();
    let mut content = Vec::new();
    for instr in bloc.content.into_iter() {
        match instr {
            tr::Instr::Binding(_, id, expr) => {
                let expr = rewrite_expr(expr, names_info);
                let id = names_info.insert(id.content());
                content.push(llr::Instr::Binding(id, expr))
            }
            tr::Instr::Expr(drop, expr) => {
                let expr = rewrite_expr(expr, names_info);
                content.push(llr::Instr::Expr(drop, expr))
            }
        }
    }
    names_info.pop_layer();
    llr::Bloc {
        content,
        last_type: bloc.last_type,
    }
}

fn rewrite_decl_fun(decl_fun: tr::DeclFun, names_info: &mut DataStruct) -> llr::DeclFun {
    let args = decl_fun
        .args
        .into_iter()
        .map(|(name, _b, typ)| {
            (
                names_info.insert(name.content()),
                names_info.compute_size(&typ),
            )
        })
        .collect();
    llr::DeclFun {
        name: decl_fun.name,
        output: names_info.compute_size(&decl_fun.output),
        content: rewrite_bloc(decl_fun.content, names_info),
        args,
    }
}

fn rewrite_file(file: tr::File, data_struct: &mut DataStruct) -> llr::File {
    let mut funs = Vec::new();
    for decl_fun in file.funs {
        data_struct.reset();
        let decl_fun = rewrite_decl_fun(decl_fun, data_struct);
        funs.push(decl_fun)
    }

    llr::File { funs }
}

fn rewrite_rec(module: Module<tr::File>, data_struct: &mut DataStruct) -> Module<llr::File> {
    let content = rewrite_file(module.content, data_struct);
    let mut submodules = HashMap::new();
    for (name, (b, module)) in module.submodules.into_iter() {
        submodules.insert(name, (b, rewrite_rec(module, data_struct)));
    }
    Module::build(content, submodules)
}

pub fn rewrite(
    module: Module<tr::File>,
    modint: ModuleInterface,
    string_extend: String,
) -> (Module<llr::File>, HashMap<String, String>, VecInfo) {
    let vec_new = modint
        .get_fun(&crate::ast::common::Path::from_vec(vec![
            "std", "vec", "Vec", "new",
        ]))
        .unwrap()
        .1
        .clone();
    let vec_push = modint
        .get_fun(&crate::ast::common::Path::from_vec(vec![
            "std", "vec", "Vec", "push",
        ]))
        .unwrap()
        .1
        .clone();
    let vec_get = modint
        .get_fun(&crate::ast::common::Path::from_vec(vec![
            "std", "vec", "Vec", "get",
        ]))
        .unwrap()
        .1
        .clone();
    let vec_len = modint
        .get_fun(&crate::ast::common::Path::from_vec(vec![
            "std", "vec", "Vec", "len",
        ]))
        .unwrap()
        .1
        .clone();
    let mut data_struct = DataStruct::new(modint, string_extend);
    let mut new = HashMap::new();
    for (vec, i) in vec_new.get_ids() {
        assert_eq!(vec.len(), 1);
        let size = data_struct.compute_size(&vec[0]);
        new.insert(*i, size);
    }
    let mut push = HashMap::new();
    for (vec, i) in vec_push.get_ids() {
        assert_eq!(vec.len(), 1);
        let size = data_struct.compute_size(&vec[0]);
        push.insert(*i, size);
    }
    let mut get = HashMap::new();
    for (vec, i) in vec_get.get_ids() {
        assert_eq!(vec.len(), 1);
        let size = data_struct.compute_size(&vec[0]);
        get.insert(*i, size);
    }
    let mut len = HashMap::new();
    for (vec, i) in vec_len.get_ids() {
        assert_eq!(vec.len(), 1);
        let size = data_struct.compute_size(&vec[0]);
        len.insert(*i, size);
    }

    let out = rewrite_rec(module, &mut data_struct);
    (
        out,
        data_struct.export_strings(),
        VecInfo {
            new,
            push,
            get,
            len,
        },
    )
}
