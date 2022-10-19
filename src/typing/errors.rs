use super::context::Trait;
use crate::ast::common::{ErrorReporter, Ident, Location, Sizes};
use crate::ast::rust::Types;
use crate::ast::typed_rust::{PostType, PostTypeInner};

#[derive(Debug)]
enum TypeErrorInfo {
    ExpectedStruct(Types),
    ExpectedTuple(Types),
    Unknown,
    NotCompatible(Types, Types),
    TryUnref(Types),
    UndeclaredVariable(String),
    CannotAffectValue,
    UndeclaredStruct(String),
    WrongNbArgs(usize, usize),
    ExpectedFun(PostTypeInner),
    StructDoesNotHasField(String, String),
    MissingField(String, String),
    CannotBorrowAsMutable,
    SameArgName(String, String),
    ExpectedSigned,
    ExpectedUnsigned,
    IncompatibleSizes(Sizes, Sizes),
    DoesNotImpTrait(PostType, Trait),
}

#[derive(Debug)]
pub struct TypeError {
    loc: Location,
    info: TypeErrorInfo,
}

impl TypeError {
    fn new(loc: Location, info: TypeErrorInfo) -> Self {
        Self { loc, info }
    }

    pub fn expected_struct(typ: Types, loc: Location) -> Self {
        Self {
            loc,
            info: TypeErrorInfo::ExpectedStruct(typ),
        }
    }

    pub fn expected_tuple(typ: Types, loc: Location) -> Self {
        Self {
            loc,
            info: TypeErrorInfo::ExpectedTuple(typ),
        }
    }

    pub fn unknown_error(loc: Location) -> Self {
        todo!();
        Self {
            loc,
            info: TypeErrorInfo::Unknown,
        }
    }

    pub fn not_compatible(loc: Location, typ1: Types, typ2: Types) -> Self {
        Self {
            loc,
            info: TypeErrorInfo::NotCompatible(typ1, typ2),
        }
    }

    pub fn cannot_unref(loc: Location, typ: Types) -> Self {
        Self {
            loc,
            info: TypeErrorInfo::TryUnref(typ),
        }
    }

    pub fn cannot_affect(loc: Location) -> Self {
        Self {
            loc,
            info: TypeErrorInfo::CannotAffectValue,
        }
    }

    pub fn unknown_var(id: Ident) -> Self {
        Self {
            loc: id.get_loc(),
            info: TypeErrorInfo::UndeclaredVariable(id.content()),
        }
    }

    pub fn unknown_struct(id: Ident) -> Self {
        Self {
            loc: id.get_loc(),
            info: TypeErrorInfo::UndeclaredStruct(id.content()),
        }
    }

    pub fn wrong_nb_args(loc: Location, got: usize, expected: usize) -> Self {
        Self {
            loc,
            info: TypeErrorInfo::WrongNbArgs(got, expected),
        }
    }

    pub fn expected_fun(loc: Location, typ: PostTypeInner) -> Self {
        Self {
            loc,
            info: TypeErrorInfo::ExpectedFun(typ),
        }
    }

    pub fn struct_no_field(loc: Location, struct_name: String, field_name: String) -> Self {
        Self {
            loc,
            info: TypeErrorInfo::StructDoesNotHasField(struct_name, field_name),
        }
    }

    pub fn missing_field(loc: Location, struct_name: String, field_name: String) -> Self {
        Self {
            loc,
            info: TypeErrorInfo::MissingField(struct_name, field_name),
        }
    }

    pub fn cannot_borrow_as_mut(loc: Location) -> Self {
        Self {
            loc,
            info: TypeErrorInfo::CannotBorrowAsMutable,
        }
    }

    pub fn same_arg_name(fun_name: Ident, arg_name: String) -> Self {
        Self {
            loc: fun_name.get_loc(),
            info: TypeErrorInfo::SameArgName(fun_name.content(), arg_name),
        }
    }

    pub fn incompatible_sizes(s1: Sizes, s2: Sizes, loc: Location) -> Self {
        Self {
            loc,
            info: TypeErrorInfo::IncompatibleSizes(s1, s2),
        }
    }

    pub fn expected_unsigned(loc: Location) -> Self {
        Self {
            loc,
            info: TypeErrorInfo::ExpectedUnsigned,
        }
    }

    pub fn expected_signed(loc: Location) -> Self {
        Self {
            loc,
            info: TypeErrorInfo::ExpectedSigned,
        }
    }

    pub fn does_not_impl_trait(loc: Location, typ: &PostType, trait_name: Trait) -> Self {
        Self {
            loc,
            info: TypeErrorInfo::DoesNotImpTrait(typ.clone(), trait_name),
        }
    }

    pub fn report_error(&self, err_reporter: &ErrorReporter) {
        if self.loc.start() == usize::MAX {
            println!("Unknown line")
        } else {
            let fst_line_id = err_reporter.get_fst_line_id(self.loc);
            let lst_line_id = err_reporter.get_last_line_id(self.loc);
            let line_str = err_reporter.get_line(fst_line_id).unwrap();
            let char_id_fst = err_reporter.get_line_start_char(fst_line_id).unwrap();
            if fst_line_id == lst_line_id {
                println!(
                    "File \"{}\", line {}, characters {}-{}:",
                    err_reporter.get_file_name(),
                    fst_line_id + 1,
                    self.loc.start() - char_id_fst + 1,
                    self.loc.start() - char_id_fst + 1
                );
                print!("{line_str}");
            } else {
                let char_id_lst = err_reporter.get_line_start_char(lst_line_id).unwrap();
                todo!()
            }
            println!("{:?}", self)
        }
    }
}
