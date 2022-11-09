use super::common::*;
use super::rust::*;
use std::io::prelude::*;

pub trait CanWrite {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()>;
}

impl CanWrite for bool {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        if *self {
            file.write_all(b"true")
        } else {
            file.write_all(b"false")
        }
    }
}

impl<T: CanWrite> CanWrite for Option<T> {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        match self {
            None => file.write_all(b"None"),
            Some(t) => {
                file.write_all(b"Some(")?;
                t.write_in(file)?;
                file.write_all(b")")
            }
        }
    }
}

impl<T: CanWrite> CanWrite for Vec<T> {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        file.write_all(b"vec![")?;
        for el in self {
            el.write_in(file)?;
            file.write_all(b",")?;
        }
        file.write_all(b"]")
    }
}

impl<T: CanWrite> CanWrite for Box<T> {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        file.write_all(b"Box::new(")?;
        (**self).write_in(file)?;
        file.write_all(b")")
    }
}

impl<T1: CanWrite, T2: CanWrite> CanWrite for (T1, T2) {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        file.write_all(b"(")?;
        self.0.write_in(file)?;
        file.write_all(b", ")?;
        self.1.write_in(file)?;
        file.write_all(b")")
    }
}

impl<T1: CanWrite, T2: CanWrite, T3: CanWrite> CanWrite for (T1, T2, T3) {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        file.write_all(b"(")?;
        self.0.write_in(file)?;
        file.write_all(b", ")?;
        self.1.write_in(file)?;
        file.write_all(b", ")?;
        self.2.write_in(file)?;
        file.write_all(b")")
    }
}

impl CanWrite for IdCounter {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        file.write_all(b"common::IdCounter::new()")
    }
}

impl CanWrite for Ident {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        file.write_all(b"common::Ident::new(\"")?;
        file.write_all(self.get_content().as_bytes())?;
        file.write_all(b"\", common::Location::default())")
    }
}

impl<T: CanWrite> CanWrite for NamePath<T> {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        match self {
            Self::Name(id) => {
                file.write_all(b"common::NamePath::<PreType>::Name(")?;
                id.write_in(file)?;
                file.write_all(b")")
            }
            Self::Specialisation(_) => todo!(),
        }
    }
}

impl<T: CanWrite> CanWrite for Path<T> {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        file.write_all(b"common::Path::<PreType>::new(")?;
        self.get_content().write_in(file)?;
        file.write_all(b", common::Location::default())")
    }
}

impl CanWrite for ComputedValue {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        match self {
            Self::Drop => file.write_all(b"common::ComputedValue::Drop"),
            Self::Keep => file.write_all(b"common::ComputedValue::Keep"),
        }
    }
}

impl CanWrite for PreTypeInner {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        match self {
            Self::Fun(id1, id2) => {
                file.write_all(b"PreTypeInner::Fun(")?;
                id1.write_in(file)?;
                file.write_all(b", ")?;
                id2.write_in(file)?;
                file.write_all(b")")
            }
            Self::Ident(id) => {
                file.write_all(b"PreTypeInner::Ident(")?;
                id.write_in(file)?;
                file.write_all(b")")

            }
            Self::IdentParametrized(id1, id2) => {
                file.write_all(b"PreTypeInner::IdentParametrized(")?;
                id1.write_in(file)?;
                file.write_all(b", ")?;
                id2.write_in(file)?;
                file.write_all(b")")
            }
            Self::Ref(id1, id2) => {
                file.write_all(b"PreTypeInner::Ref(")?;
                id1.write_in(file)?;
                file.write_all(b", ")?;
                id2.write_in(file)?;
                file.write_all(b")")
            }
            Self::Tuple(id,) => {
                file.write_all(b"PreTypeInner::Tuple(")?;
                id.write_in(file)?;
                file.write_all(b")")
            }
        }
    }
}

impl CanWrite for PreType {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        file.write_all(b"PreType { content : ")?;
        self.content.write_in(file)?;
        file.write_all(b"}")
    }
}

impl CanWrite for BinOperator {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        match self {
            Self::Add => file.write_all(b"common::BinOperator::Add"),
            Self::Sub => file.write_all(b"common::BinOperator::Sub"),
            Self::Mul => file.write_all(b"common::BinOperator::Mul"),
            Self::Div => file.write_all(b"common::BinOperator::Div"),
            Self::Mod => file.write_all(b"common::BinOperator::Mod"),

            Self::And => file.write_all(b"common::BinOperator::And"),
            Self::Or  => file.write_all(b"common::BinOperator::Or"),

            Self::Shl => file.write_all(b"common::BinOperator::Shl"),
            Self::Shr => file.write_all(b"common::BinOperator::Shr"),
            Self::BitAnd => file.write_all(b"common::BinOperator::BitAnd"),
            Self::BitOr => file.write_all(b"common::BinOperator::BitOr"),

            Self::Eq => file.write_all(b"common::BinOperator::Eq"),
            Self::Ne => file.write_all(b"common::BinOperator::Ne"),


            Self::Greater => file.write_all(b"common::BinOperator::Greater"),
            Self::GreaterEq => file.write_all(b"common::BinOperator::GreaterEq"),
            Self::Lower => file.write_all(b"common::BinOperator::Lower"),
            Self::LowerEq => file.write_all(b"common::BinOperator::LowerEq"),

            Self::Set => file.write_all(b"common::BinOperator::Set"),
        }
    }
}

impl CanWrite for UnaOperator {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        match self {
            Self::Neg => file.write_all(b"common::UnaOperator::Neg"),
            Self::Not => file.write_all(b"common::UnaOperator::Not"),
        }
    }
}

impl CanWrite for u64 {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        file.write_all(format!("{}", self).as_bytes())
    }
}

impl CanWrite for usize {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        file.write_all(format!("{}", self).as_bytes())
    }
}

impl CanWrite for Sizes {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        match self {
            Self::S8  => file.write_all(b"common::Sizes::S8"),
            Self::S16 => file.write_all(b"common::Sizes::S16"),
            Self::S32 => file.write_all(b"common::Sizes::S32"),
            Self::S64 => file.write_all(b"common::Sizes::S64"),
            Self::SUsize => file.write_all(b"common::Sizes::SUsize"),
        }
    }
}

impl CanWrite for Projector {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        match self {
            Self::Name(_) => todo!(),
            Self::Int(_) => todo!(),
        }
    }
}

impl<T: CanWrite> CanWrite for ExprInner<T> {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        match self {
            Self::Array(v) => {
                file.write_all(b"ExprInner::Array(")?;
                v.write_in(file)?;
                file.write_all(b")")
            }
            Self::BinaryOp(bin, e1, e2) => {
                file.write_all(b"ExprInner::BinaryOp(")?;
                bin.write_in(file)?;
                file.write_all(b", ")?;
                e1.write_in(file)?;
                file.write_all(b", ")?;
                e2.write_in(file)?;
                file.write_all(b")")
            }
            Self::Bloc(b) => {
                file.write_all(b"ExprInner::Bloc(")?;
                b.write_in(file)?;
                file.write_all(b")")
            }
            Self::Bool(b) => {
                file.write_all(b"ExprInner::Bool(")?;
                b.write_in(file)?;
                file.write_all(b")")
            }
            Self::BuildStruct(b, args) => {
                file.write_all(b"ExprInner::BuildStruct(")?;
                b.write_in(file)?;
                file.write_all(b", ")?;
                args.write_in(file)?;
                file.write_all(b")")
            }
            Self::Coercion(a1, a2) => {
                file.write_all(b"ExprInner::Coercion(")?;
                a1.write_in(file)?;
                file.write_all(b", ")?;
                a2.write_in(file)?;
                file.write_all(b")")
            },
            Self::Deref(e) => {
                file.write_all(b"ExprInner::Deref(")?;
                e.write_in(file)?;
                file.write_all(b")")
            },
            Self::FunCall(a1, a2, a3) => {
                file.write_all(b"ExprInner::FunCall(")?;
                a1.write_in(file)?;
                file.write_all(b", ")?;
                a2.write_in(file)?;
                file.write_all(b", ")?;
                a3.write_in(file)?;
                file.write_all(b")")
            },
            Self::If(a1, a2, a3) => {
                file.write_all(b"ExprInner::If(")?;
                a1.write_in(file)?;
                file.write_all(b", ")?;
                a2.write_in(file)?;
                file.write_all(b", ")?;
                a3.write_in(file)?;
                file.write_all(b")")
            },
            Self::Index(a1, a2) => {
                file.write_all(b"ExprInner::Index(")?;
                a1.write_in(file)?;
                file.write_all(b", ")?;
                a2.write_in(file)?;
                file.write_all(b")")
            },
            Self::Int(a1, a2) => {
                file.write_all(b"ExprInner::Int(")?;
                a1.write_in(file)?;
                file.write_all(b", ")?;
                a2.write_in(file)?;
                file.write_all(b")")
            },
            Self::MacroCall(a1, a2) => {
                file.write_all(b"ExprInner::MacroCall(")?;
                a1.write_in(file)?;
                file.write_all(b", ")?;
                a2.write_in(file)?;
                file.write_all(b")")
            },
            Self::Method(a1, a2, a3) => {
                file.write_all(b"ExprInner::Method(")?;
                a1.write_in(file)?;
                file.write_all(b", ")?;
                a2.write_in(file)?;
                file.write_all(b", ")?;
                a3.write_in(file)?;
                file.write_all(b")")
            },
            Self::Parenthesis(e) => {
                file.write_all(b"ExprInner::Parenthesis(")?;
                e.write_in(file)?;
                file.write_all(b")")
            },
            Self::Proj(a1, a2) => {
                file.write_all(b"ExprInner::Proj(")?;
                a1.write_in(file)?;
                file.write_all(b", ")?;
                a2.write_in(file)?;
                file.write_all(b")")
            },
            Self::Ref(a1, a2) => {
                file.write_all(b"ExprInner::Ref(")?;
                a1.write_in(file)?;
                file.write_all(b", ")?;
                a2.write_in(file)?;
                file.write_all(b")")
            },
            Self::Return(opt) => {
                file.write_all(b"ExprInner::Return(")?;
                opt.write_in(file)?;
                file.write_all(b")")
            },
            Self::String(s) => {
                file.write_all(b"ExprInner::String(\"")?;
                file.write_all(b"an ommited string")?;
                file.write_all(b"\".to_string())")
            },
            Self::Tuple(v) => {
                file.write_all(b"ExprInner::Tuple(")?;
                v.write_in(file)?;
                file.write_all(b")")
            },
            Self::UnaryOp(a1, a2) => {
                file.write_all(b"ExprInner::UnaryOp(")?;
                a1.write_in(file)?;
                file.write_all(b", ")?;
                a2.write_in(file)?;
                file.write_all(b")")
            },
            Self::Var(v) => {
                file.write_all(b"ExprInner::Var(")?;
                v.write_in(file)?;
                file.write_all(b")")
            },
            Self::While(a1, a2) => {
                file.write_all(b"ExprInner::While(")?;
                a1.write_in(file)?;
                file.write_all(b", ")?;
                a2.write_in(file)?;
                file.write_all(b")")
            },
        }
    }
}

impl<T: CanWrite> CanWrite for Expr<T> {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        file.write_all(b"Expr { loc : common::Location::default(),\n\tcontent : ")?;
        self.content.write_in(file)?;
        file.write_all(b"\n, typed : ")?;
        self.typed.write_in(file)?;
        file.write_all(b"}")
    }
}

impl<T: CanWrite> CanWrite for InstrInner<T> {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        match self {
            Self::Expr(comp_val, expr) => {
                file.write_all(b"InstrInner::Expr(")?;
                comp_val.write_in(file)?;
                file.write_all(b", ")?;
                expr.write_in(file)?;
                file.write_all(b")")
            }
            Self::Binding(public, id, typ, expr) => {
                file.write_all(b"InstrInner::Binding(")?;
                public.write_in(file)?;
                file.write_all(b", ")?;
                id.write_in(file)?;
                file.write_all(b", ")?;
                typ.write_in(file)?;
                file.write_all(b", ")?;
                expr.write_in(file)?;
                file.write_all(b")")
            }
        }
    }
}

impl<T: CanWrite> CanWrite for Instr<T> {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        file.write_all(b"Instr { loc : common::Location::default(),\n\tcontent : ")?;
        self.content.write_in(file)?;
        file.write_all(b"}")
    }
}

impl<T: CanWrite> CanWrite for Bloc<T> {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        file.write_all(b"Bloc { loc : common::Location::default(),\n\tcontent : ")?;
        self.content.write_in(file)?;
        file.write_all(b"}")
    }
}

impl<T: CanWrite> CanWrite for DeclConst<T> {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        file.write_all(b"DeclConst {\n\tpublic : ")?;
        self.public.write_in(file)?;
        file.write_all(b",\n\tname : ")?;
        self.name.write_in(file)?;
        file.write_all(b",\n\ttyp : ")?;
        self.typ.write_in(file)?;
        file.write_all(b",\n\texpr : ")?;
        self.expr.write_in(file)?;
        file.write_all(b",}")
    }
}

impl CanWrite for DeclImpl {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        todo!()
    }
}

impl CanWrite for DeclFun {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        file.write_all(b"DeclFun {\n\tpublic : ")?;
        self.public.write_in(file)?;
        file.write_all(b",\n\tname : ")?;
        self.name.write_in(file)?;
        file.write_all(format!(",\nself_arg : {:?},\n", self.self_arg).as_bytes())?;
        file.write_all(b"\targs : ")?;
        self.args.write_in(file)?;
        file.write_all(b",\n\toutput : ")?;
        self.output.write_in(file)?;
        file.write_all(b",\n\tcontent : ")?;
        self.content.write_in(file)?;
        file.write_all(b",\n\tid_counter : ")?;
        self.id_counter.write_in(file)?;
        file.write_all(b",\n}")
    }
}

impl CanWrite for DeclStruct {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        todo!()
    }
}

impl<DF: CanWrite> CanWrite for Decl<DF> {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        match self {
            Self::Fun(df) => {
                file.write_all(b"Decl::Fun(")?;
                df.write_in(file)?;
                file.write_all(b")")
            }
            Self::Struct(ds) => {
                file.write_all(b"Decl::Struct(")?;
                ds.write_in(file)?;
                file.write_all(b")")
            }
            Self::Impl(di) => {
                file.write_all(b"Decl::Impl(")?;
                di.write_in(file)?;
                file.write_all(b")")
            }
            Self::Const(dc) => {
                file.write_all(b"Decl::Const(")?;
                dc.write_in(file)?;
                file.write_all(b")")
            }
        }
    }
}

impl CanWrite for Open {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        match self {
            Self::Mod(b, id, opt) => {
                file.write_all(b"Open::Mod(")?;
                b.write_in(file)?;
                file.write_all(b", ")?;
                id.write_in(file)?;
                file.write_all(b", ")?;
                opt.write_in(file)?;
                file.write_all(b")")
            }
            Self::Use(path, opt) => {
                file.write_all(b"Open::Use(")?;
                path.write_in(file)?;
                file.write_all(b",")?;
                opt.write_in(file)?;
                file.write_all(b")")
            }
        }
    }
}

impl CanWrite for File {
    fn write_in(&self, file: &mut std::fs::File) -> std::io::Result<()> {
        file.write_all(b"File {\n\tname : \"")?;
        file.write_all(self.name.as_bytes())?;
        file.write_all(b"\".to_string(),\n\tcontent : ")?;
        self.content.write_in(file)?;
        file.write_all(b",\n\tdep : ")?;
        self.dep.write_in(file)?;
        file.write_all(b",\n\terr_reporter : common::ErrorReporter::empty(),\n}\n")
    }
}
