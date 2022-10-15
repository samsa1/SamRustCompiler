use crate::ast::common::*;
use crate::ast::low_level_repr as llr;
use crate::ast::typed_rust as tr;
use std::collections::HashMap;

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

struct DataStruct {
    max_id: usize,
    vars: Vec<HashMap<String, usize>>,
    structs: HashMap<String, StructInfo>,
    strings_count: usize,
    strings_stored: HashMap<String, String>,
    pointer_size: usize,
}

impl DataStruct {
    fn new(structs_raw: Vec<tr::DeclStruct>) -> Self {
        let mut structs = HashMap::new();
        for struct_raw in &structs_raw {
            //            println!("First add of {:?}", struct_raw.name.get_content());
            structs.insert(
                struct_raw.name.get_content().to_string(),
                StructInfo::new(struct_raw.size),
            );
        }
        let mut data_struct = Self {
            max_id: 0,
            vars: vec![HashMap::new()],
            structs,
            strings_count: 0,
            strings_stored: HashMap::new(),
            pointer_size: 8,
        };
        let mut structs2 = HashMap::new();
        for struct_raw in structs_raw {
            //            println!("Adding {:?}", struct_raw.name.get_content());
            let mut offset = 0;
            let mut binding = HashMap::new();
            //            println!("{:?}", struct_raw);
            for (argname, typ) in struct_raw.args {
                let size = data_struct.compute_size(&typ);
                binding.insert(argname, (offset, size));
                //                println!("Computing size of {:?}", typ.content);
                offset += size;
            }
            assert_eq!(offset, struct_raw.size);
            structs2.insert(
                struct_raw.name.content(),
                StructInfo::new_full(binding, offset),
            );
        }

        data_struct.structs = structs2;
        data_struct
    }

    fn export_strings(self) -> HashMap<String, String> {
        self.strings_stored
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
        let str2 = format!("rust_string{}", self.strings_count);
        self.strings_count += 1;
        self.strings_stored.insert(str, str2.clone());
        str2
    }

    fn get_struct_info(&self, id: &str) -> &StructInfo {
        self.structs.get(id).unwrap()
    }

    fn compute_size(&self, typ: &tr::PostType) -> usize {
        match &typ.content {
            tr::PostTypeInner::Box(_)
            | tr::PostTypeInner::Fun(_, _, _)
            | tr::PostTypeInner::Ref(_, _)
            | tr::PostTypeInner::String => self.pointer_size,
            tr::PostTypeInner::BuiltIn(BuiltinType::Bool) => 1,
            tr::PostTypeInner::BuiltIn(BuiltinType::Int(_, s)) => s.to_byte_size(),
            tr::PostTypeInner::Diverge => todo!(),
            //            tr::PostTypeInner::Enum(_) => todo!(),
            tr::PostTypeInner::FreeType(_) => todo!(),
            tr::PostTypeInner::Struct(name, _) if name == "Vec" => self.pointer_size,
            tr::PostTypeInner::Struct(name, _) => self.structs.get(name).unwrap().get_size(),
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

            _ => panic!("ICE"),
        }
    }
}

fn rewrite_expr(top_expr: tr::Expr, names_info: &mut DataStruct) -> llr::Expr {
    match *top_expr.content {
        tr::ExprInner::Bloc(bloc) => llr::Expr {
            content: Box::new(llr::ExprInner::Bloc(rewrite_bloc(bloc, names_info))),
            loc: top_expr.loc,
            size: names_info.compute_size(&top_expr.typed),
            typed: top_expr.typed,
        },
        tr::ExprInner::Bool(b) => llr::Expr {
            content: Box::new(llr::ExprInner::Bool(b)),
            loc: top_expr.loc,
            typed: top_expr.typed,
            size: 1,
        },
        tr::ExprInner::BuildStruct(name, args) => {
            let struct_info = names_info.get_struct_info(name.get_content()).clone();
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
        tr::ExprInner::Deref(expr) => llr::Expr {
            content: Box::new(llr::ExprInner::Deref(rewrite_expr(expr, names_info))),
            loc: top_expr.loc,
            size: names_info.compute_size(&top_expr.typed),
            typed: top_expr.typed,
        },

        tr::ExprInner::FunCall(id, args) => {
            let args2 = args
                .into_iter()
                .map(|e| rewrite_expr(e, names_info))
                .collect();
            let expr_inner = match names_info.get_var(id.get_content()) {
                Some(id) => llr::ExprInner::FunCallVar(id, args2),
                None => llr::ExprInner::FunCall(id.content(), args2),
            };
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
        tr::ExprInner::Int(i) => llr::Expr {
            content: Box::new(llr::ExprInner::Int(
                i,
                top_expr.typed.get_int_size().unwrap(),
            )),
            loc: top_expr.loc,
            size: top_expr.typed.get_int_size().unwrap().to_byte_size(),
            typed: top_expr.typed,
        },
        tr::ExprInner::Print(str) => {
            let label = names_info.insert_string(str);
            llr::Expr {
                content: Box::new(llr::ExprInner::Print(label)),
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
                names_info.compute_size(&expr1.typed),
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
                content: Box::new(llr::ExprInner::Constant(label)),
                loc: top_expr.loc,
                typed: top_expr.typed,
                size: names_info.get_pointer_size(),
            }
            .to_ref(names_info.get_pointer_size())
        }
        tr::ExprInner::Tuple(exprs) => llr::Expr {
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
            None => llr::Expr {
                content: Box::new(llr::ExprInner::Constant(var_name.content())),
                loc: top_expr.loc,
                size: names_info.compute_size(&top_expr.typed),
                typed: top_expr.typed,
            },
        },

        tr::ExprInner::BinOp(op, expr1, expr2) => llr::Expr {
            content: Box::new(llr::ExprInner::BinOp(
                op,
                rewrite_expr(expr1, names_info),
                rewrite_expr(expr2, names_info),
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
            tr::Instr::Return(opt_expr) => {
                let opt_expr = opt_expr.map(|e| rewrite_expr(e, names_info));
                content.push(llr::Instr::Return(opt_expr));
                names_info.pop_layer();
                return llr::Bloc {
                    content,
                    last_type: bloc.last_type,
                };
            }
            tr::Instr::While(expr, bloc) => {
                let expr = rewrite_expr(expr, names_info);
                let bloc = rewrite_bloc(bloc, names_info);
                content.push(llr::Instr::While(expr, bloc))
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
        .map(|(name, _b, typ)| (names_info.insert(name.content()), typ))
        .collect();
    llr::DeclFun {
        name: decl_fun.name,
        output: decl_fun.output,
        content: rewrite_bloc(decl_fun.content, names_info),
        args,
    }
}

pub fn rewrite_file(file: tr::File) -> llr::File {
    println!("Building DataStruct");
    let mut data_struct = DataStruct::new(file.structs);
    println!("DataStruct defined");
    let mut funs = Vec::new();
    for decl_fun in file.funs {
        let decl_fun = rewrite_decl_fun(decl_fun, &mut data_struct);
        data_struct.reset();
        funs.push(decl_fun)
    }

    llr::File {
        name: file.name,
        funs,
        strings: data_struct.export_strings(),
    }
}
