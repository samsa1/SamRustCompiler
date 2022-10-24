use super::context::Trait;
use crate::ast::common::{ErrorReporter, Ident, Location, Sizes};
use crate::ast::rust::Types;
use crate::ast::typed_rust::{PostType, PostTypeInner};

const RED: &str = "\x1b[1;31m";
const NC: &str = "\x1b[0m";
const BLUE: &str = "\x1b[1;34m";

#[derive(Debug)]
enum TypeErrorInfo {
    ExpectedStruct(Types),
    ExpectedTuple(Types),
    ExpectedTuple2(PostType),
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
    OutOfBoundTuple(usize, usize),
    WrongMutability(bool, bool),
    SelfRefConst(String),
}

impl TypeErrorInfo {
    fn get_id(&self) -> usize {
        match self {
            Self::ExpectedStruct(_) => 1,
            Self::ExpectedTuple(_) => 2,
            Self::ExpectedTuple2(_) => 3,
            Self::NotCompatible(_, _) => 4,
            Self::TryUnref(_) => 5,
            Self::UndeclaredVariable(_) => 6,
            Self::CannotAffectValue => 7,
            Self::UndeclaredStruct(_) => 8,
            Self::WrongNbArgs(_, _) => 9,
            Self::ExpectedFun(_) => 10,
            Self::StructDoesNotHasField(_, _) => 11,
            Self::MissingField(_, _) => 12,
            Self::CannotBorrowAsMutable => 13,
            Self::SameArgName(_, _) => 14,
            Self::ExpectedSigned => 15,
            Self::ExpectedUnsigned => 16,
            Self::IncompatibleSizes(_, _) => 17,
            Self::DoesNotImpTrait(_, _) => 18,
            Self::OutOfBoundTuple(_, _) => 19,
            Self::WrongMutability(_, _) => 20,
            Self::SelfRefConst(_) => 21,
        }
    }

    fn get_error_name(&self) -> &'static str {
        match self {
            Self::ExpectedStruct(_) => "expected struct",
            Self::ExpectedTuple(_) => "expected tuple",
            Self::ExpectedTuple2(_) => "expected tuple",
            Self::NotCompatible(_, _) => "incompatible types",
            Self::TryUnref(_) => "invalid dereferencing",
            Self::UndeclaredVariable(_) => "undeclared variable",
            Self::CannotAffectValue => "invalid affectation",
            Self::UndeclaredStruct(_) => "unkown struct",
            Self::WrongNbArgs(_, _) => "invalid number of arguments",
            Self::ExpectedFun(_) => "expected function",
            Self::StructDoesNotHasField(_, _) => "invalid field",
            Self::MissingField(_, _) => "missing field",
            Self::CannotBorrowAsMutable => "cannot borrow as mut",
            Self::SameArgName(_, _) => "identifier bound multiple times",
            Self::ExpectedSigned => "expected signed integer",
            Self::ExpectedUnsigned => "expected unsigned integer",
            Self::IncompatibleSizes(_, _) => "integer of different sizes",
            Self::DoesNotImpTrait(_, _) => "type does not implement trait",
            Self::OutOfBoundTuple(_, _) => "index out of bound",
            Self::WrongMutability(_, _) => "mutability incompatibility",
            Self::SelfRefConst(_) => "cycle detected in const evaluating",
        }
    }

    fn get_message(&self) -> String {
        match self {
            Self::ExpectedStruct(typ) => format!("{} is not a struct", typ),
            Self::ExpectedTuple(typ) => format!("{} is not a tuple", typ),
            Self::NotCompatible(typ1, typ2) => format!("expected {}, found {}", typ1, typ2),
            _ => format!("undefined message for error {}", self.get_id()),
        }
    }
}

#[derive(Debug)]
pub struct TypeError {
    loc: Location,
    info: TypeErrorInfo,
}

impl TypeError {
    pub fn wrong_mutability(loc: Location, expected: bool, got: bool) -> Self {
        Self {
            loc,
            info: TypeErrorInfo::WrongMutability(expected, got),
        }
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

    pub fn expected_tuple2(typ: PostType, loc: Location) -> Self {
        Self {
            loc,
            info: TypeErrorInfo::ExpectedTuple2(typ),
        }
    }

    /*    pub fn unknown_error(loc: Location) -> Self {
            todo!();
            Self {
                loc,
                info: TypeErrorInfo::Unknown,
            }
        }
    */

    pub fn not_compatible(loc: Location, got: Types, expected: Types) -> Self {
        Self {
            loc,
            info: TypeErrorInfo::NotCompatible(expected, got),
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

    pub fn out_of_bound_tuple(loc: Location, id: usize, len: usize) -> Self {
        Self {
            loc,
            info: TypeErrorInfo::OutOfBoundTuple(id, len),
        }
    }

    pub fn self_referencing_constant(id: Ident) -> Self {
        Self {
            loc: id.get_loc(),
            info: TypeErrorInfo::SelfRefConst(id.content()),
        }
    }

    pub fn report_error(&self, err_reporter: &ErrorReporter) {
        if self.loc.start() == usize::MAX {
            println!("Unknown line")
        } else {
            println!(
                "{}error[E{:0>4}]{}: {}",
                RED,
                self.info.get_id(),
                NC,
                self.info.get_error_name()
            );
            let fst_line_id = err_reporter.get_fst_line_id(self.loc);
            let lst_line_id = err_reporter.get_last_line_id(self.loc);
            let line_str = err_reporter.get_line(fst_line_id).unwrap();
            let char_id_fst = *err_reporter.get_line_start_char(fst_line_id).unwrap();
            println!(
                " {}-->{} {}:{}:{}:",
                BLUE,
                NC,
                err_reporter.get_file_name(),
                fst_line_id + 1,
                self.loc.start() - char_id_fst + 1,
            );
            if fst_line_id == lst_line_id {
                let mut i = 1;
                let mut str = String::new();
                while i <= fst_line_id + 1 {
                    str.push(' ');
                    i *= 10
                }
                println!("{str} {BLUE}|{NC}");
                print!("{BLUE}{} |{NC} {line_str}", fst_line_id + 1);
                print!("{str} {BLUE}|{NC} ");
                for _ in char_id_fst..self.loc.start() {
                    print!(" ")
                }
                print!("{}", RED);
                for _ in self.loc.start()..self.loc.end() {
                    print!("^")
                }
                println!(" {}{}", self.info.get_message(), NC);
            } else {
                println!("{:?}", self);
                todo!()
            }
        }
    }
}
