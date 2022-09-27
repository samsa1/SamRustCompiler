use crate::ast::rust::Types;
use crate::ast::typed_rust::PostTypeInner;
use crate::ast::common::{Ident, Location};

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
}

#[derive(Debug)]
pub struct TypeError {
    loc : Location,
    info : TypeErrorInfo,
}

impl TypeError {
    fn new(loc : Location, info : TypeErrorInfo) -> Self {
        Self {
            loc,
            info,
        }
    }

    pub fn expected_struct(typ : Types, loc : Location) -> Self {
        Self {
            loc,
            info : TypeErrorInfo::ExpectedStruct(typ),
        }
    }

    pub fn expected_tuple(typ : Types, loc : Location) -> Self {
        Self {
            loc,
            info : TypeErrorInfo::ExpectedTuple(typ),
        }
    }

    pub fn unknown_error(loc : Location) -> Self {
        todo!();
        Self {
            loc,
            info : TypeErrorInfo::Unknown,
        }
    }

    pub fn not_compatible(loc : Location, typ1 : Types, typ2 : Types) -> Self {
        Self {
            loc,
            info : TypeErrorInfo::NotCompatible(typ1, typ2)
        }
    }

    pub fn cannot_unref(loc : Location, typ : Types) -> Self {
        Self {
            loc,
            info : TypeErrorInfo::TryUnref(typ)
        }
    }

    pub fn cannot_affect(loc : Location) -> Self {
        Self {
            loc,
            info : TypeErrorInfo::CannotAffectValue
        }
    }

    pub fn unknown_var(id : Ident) -> Self {
        Self {
            loc : id.get_loc(),
            info : TypeErrorInfo::UndeclaredVariable(id.content())
        }
    }

    pub fn unknown_struct(id : Ident) -> Self {
        Self {
            loc : id.get_loc(),
            info : TypeErrorInfo::UndeclaredStruct(id.content())
        }
    }

    pub fn wrong_nb_args(loc : Location, got : usize, expected : usize) -> Self {
        Self {
            loc,
            info : TypeErrorInfo::WrongNbArgs(got, expected),
        }
    }

    pub fn expected_fun(loc : Location, typ : PostTypeInner) -> Self {
        Self {
            loc,
            info : TypeErrorInfo::ExpectedFun(typ)
        }
    }

    pub fn struct_no_field(loc : Location, struct_name : String, field_name : String) -> Self {
        Self {
            loc,
            info : TypeErrorInfo::StructDoesNotHasField(struct_name, field_name)
        }
    }

    pub fn missing_field(loc : Location, struct_name : String, field_name : String) -> Self {
        Self {
            loc,
            info : TypeErrorInfo::MissingField(struct_name, field_name)
        }
    }

    pub fn cannot_borrow_as_mut(loc : Location) -> Self {
        Self {
            loc,
            info : TypeErrorInfo::CannotBorrowAsMutable
        }
    }
}
