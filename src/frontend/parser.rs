use crate::ast::{common::*, rust::*};
use std::fs;


fn to_expr(e : ExprInner) -> Expr {
    Expr {
        content : Box::new(e),
        typed : None,
        loc : Location::default(),
    }
}

fn expr_of_bloc(b : Bloc) -> Expr {
    to_expr(ExprInner::Bloc(b))
}

peg::parser!{
    grammar list_parser() for str {
        rule number() -> u32
            = n:$(['0'..='9']+) {? n.parse().or(Err("u32")) }

        rule string() -> String
            = "\"" n:([' '|'!'|'#'..='~']+) "\""
                { n.into_iter().collect() }

        rule char() -> char =
            n:['a'..='z' | 'A'..='Z' | '_'] { n }

        rule num() -> char =
            n:['0'..='9'] { n }

        rule char_num() -> char = precedence! {
            n:char() { n }
            n:num() { n }
        }


        pub rule list() -> Vec<u32> =
            "[" l:(number() ** ",") "]" { l }

        pub rule file() -> Vec<Decl> =
            space() d:decl()* { d }

        rule spaces() -> () = (quiet!{[' ' | '\n']+} / expected!("space")) { () }
        rule _spaces() -> () = [' ' | '\n']+ { () }

        rule space() -> () = [' ' | '\n']* { () }

        rule decl() -> Decl = precedence!{
            df:decl_fun() space() { Decl::Fun(df) }
            ds:decl_struct() space() { Decl::Struct(ds) }
        }

        rule arrow_typ() -> PreType = precedence! {
            "->" space() t:typ() space() { t }
            "" { PreType::unit()}
        }


        rule decl_fun() -> DeclFun =
            "fn" spaces() n:name() "(" args:(fun_args() ** ",") ")"
                    space()
                    out:arrow_typ() b:bloc()
                {
                    DeclFun {
                        name : n,
                        args,
                        output : out,
                        content : b,
                    }
                }

        rule decl_struct() -> DeclStruct =
            "struct" spaces() n:name() "{"
                args:((spaces() name() spaces() ":" spaces() typ() spaces()) ** ",") "}" { todo!() }

        rule reserved() =
            "true" / "false" / "if" / "else" / "while" / "fn" / "pub" / "struct" / "as"

        rule name() -> Ident =
            !reserved() n1:char() n2:(char_num()*) {
                let mut n = String::from(n1);
                for c in n2.into_iter() {
                    n.push(c)
                }
                println!("parsed name {}", n);
                Ident::from_str(&n)
            }

        rule typ() -> PreType = precedence! {
            "&" space() "mut" spaces() typ:typ_no_ref() {
                PreTypeInner::Ref(Box::new(typ)).to_mut()
            }
            "&" space() typ:typ_no_ref() {
                PreTypeInner::Ref(Box::new(typ)).to_nonmut()
            }

            typ:typ_no_ref() { typ}
        }

        rule typ_no_ref() -> PreType = precedence! {
            n:name() "<" t:typ() ">" { PreTypeInner::IdentParametrized(n, vec![t]).to_nonmut()}
            n:name() { PreTypeInner::Ident(n).to_nonmut()}
        }
            
        rule fun_args() -> (Ident, bool, PreType) = precedence! {
            b:("mut" spaces())? n:name() space() ":" space() t:typ() { (n, b != None, t) }
        }
        
        rule bloc() -> Bloc =
            "{" v:instr()* e:expr_ws()? "}" { Bloc {
                content : Vec::new()
            } }

        rule bloc_inner() -> (Vec<Instr>, Option<Expr>) = precedence! {
            i:instr() "}" { (vec![i], None) }
            e:expr_ws() "}" { (Vec::new(), Some(e)) }
            i:instr() t:@
                { let (mut v, opt) = t; v.push(i); (v, opt)}
        } 

        rule instr() -> Instr = precedence! {
            e : expr_ws() (quiet!{";"} / expected!("end of expr")) { println!("parsed Instr"); Instr::Expr(e) }
            ";" { println!("parsed Instr"); Instr::Expr(Expr::unit()) }
            "let" spaces() b:("mut" spaces())? n:name() space() "=" e:expr_ws() ";"
                { println!("parsed Instr"); Instr::Binding(b != None, n, e) }
            "while" space() e:expr() space() b:bloc()
                { println!("parsed Instr"); Instr::While(e, b) }
            "return" spaces() e:expr()? ";"
                { println!("parsed Instr"); Instr::Return(e) }
            i:if() ";"? { println!("parsed Instr if"); Instr::Expr(i) }
        }

        rule if() -> Expr = precedence! {
            "if" spaces() e1:expr() spaces() e2:expr()
                { println!("parsed if 1"); to_expr(ExprInner::If(e1, e2, Expr::unit())) }
            "if" spaces() e1:expr() spaces() e2:expr() "else" e3:@
                { println!("parsed if 2"); to_expr(ExprInner::If(e1, e2, e3)) }
            "if" spaces() e1:expr() spaces() e2:expr() "else" e3:expr()
                { println!("parsed if 3"); to_expr(ExprInner::If(e1, e2, e3)) }
        }

        // expression starting with spaces
        rule expr_ws() -> Expr =
            spaces() e:expr() spaces() { println!("parsed 135 {:?}", e); e }

        rule expr() -> Expr = precedence! {
            "(" spaces() e:expr() spaces() ")" { println!("parsed 138 {:?}", e); e }
            e:expr_in() { println!("parsed expr_in : 139 {:?}", e); to_expr(e) }
        }

        rule small_expr() -> Expr = precedence! {
            "(" e:expr_ws() ")" { println!("parsed 143 {:?}", e); e }
            e:small_expr_i() { println!("parsed 144 {:?}", e); to_expr(e) }
        }

        rule small_expr_i() -> ExprInner = precedence! {
            n:number() { println!("parsed small_expr_i {:?}", n); ExprInner::Int(n as usize) }
            "true" { println!("true"); ExprInner::Bool(true) }
            "false" { println!("false"); ExprInner::Bool(false) }
           /* n:name() { ExprInner::Var(n) }*/
            b:bloc() { println!("bloc {:?}", b); ExprInner::Bloc(b) }
            s:string() { ExprInner::String(s) }
        }

        rule expr_in() -> ExprInner = precedence! {
            e:(quiet!{small_expr_i()} / expected!("value")) { println!("{:?}", e); e }
            e:small_expr() "." n:proj() { println!("{:?}", e); ExprInner::Proj(e, n) }
            e:small_expr() "." n:name() "(" v:(expr_ws() ** ",") ")" { println!("{:?}", e); ExprInner::Method(e, n, v)}
            e1:small_expr() "[" e2:expr_ws() "]" { println!("{:?}", e1); ExprInner::Method(e1, Ident::from_str("index"), vec![e2]) }
            n:name() "(" v:(expr_ws() ** ",") ")" { println!("call {:?}", n); ExprInner::FunCall(n, v)}
            "print" space() "!" space() "(" v:(expr_ws() ** ",") ")" { println!("print {:?}", v); ExprInner::MacroCall(Ident::from_str("print"), v) }
            "vec" space() "!" space() "[" v:(expr_ws() ** ",") "]" { println!("vec {:?}", v); ExprInner::MacroCall(Ident::from_str("vec"), v) }
            e1:@ spaces() (quiet!{"="}/ expected!("infix operator")) spaces() e2:(@) { println!("{:?} = {:?}", e1, e2); ExprInner::Method(to_expr(e1), todo!(), vec![to_expr(e2)]) }
            --
            e1:(@) spaces() (quiet!{"||"}/ expected!("infix operator")) spaces() e2:@ { println!("{:?} = {:?}", e1, e2); ExprInner::Method(to_expr(e1), Ident::from_str("or"), vec![to_expr(e2)]) }
            --
            e1:(@) spaces() (quiet!{"&&"}/ expected!("infix operator")) spaces() e2:@ { println!("{:?} = {:?}", e1, e2); ExprInner::Method(to_expr(e1), Ident::from_str("and"), vec![to_expr(e2)]) }
            --
            e1:(@) spaces() (quiet!{"=="}/ expected!("infix operator")) spaces() e2:@ { println!("{:?} = {:?}", e1, e2); ExprInner::Method(to_expr(e1), Ident::from_str("eq"), vec![to_expr(e2)]) }
            e1:(@) spaces() (quiet!{"!="}/ expected!("infix operator")) spaces() e2:@ { println!("{:?} = {:?}", e1, e2); ExprInner::Method(to_expr(e1), Ident::from_str("neq"), vec![to_expr(e2)]) }
            e1:(@) spaces() (quiet!{"<"}/ expected!("infix operator")) spaces() e2:@  { println!("{:?} = {:?}", e1, e2); ExprInner::Method(to_expr(e1), Ident::from_str("lower"), vec![to_expr(e2)]) }
            e1:(@) spaces() (quiet!{"<="}/ expected!("infix operator")) spaces() e2:@ { println!("{:?} = {:?}", e1, e2); ExprInner::Method(to_expr(e1), Ident::from_str("lower_eq"), vec![to_expr(e2)]) }
            e1:(@) spaces() (quiet!{">"}/ expected!("infix operator")) spaces() e2:@  { println!("{:?} = {:?}", e1, e2); ExprInner::Method(to_expr(e1), Ident::from_str("greater"), vec![to_expr(e2)]) }
            e1:(@) spaces() (quiet!{">="}/ expected!("infix operator")) spaces() e2:@ { println!("{:?} = {:?}", e1, e2); ExprInner::Method(to_expr(e1), Ident::from_str("greater_eq"), vec![to_expr(e2)]) }
            --
            e1:(@) spaces() (quiet!{"+"}/ expected!("infix operator")) spaces() e2:@ { println!("{:?} = {:?}", e1, e2); ExprInner::Method(to_expr(e1), Ident::from_str("add"), vec![to_expr(e2)]) }
            e1:(@) spaces() (quiet!{"-"}/ expected!("infix operator")) spaces() e2:@ { println!("{:?} = {:?}", e1, e2); ExprInner::Method(to_expr(e1), Ident::from_str("sub"), vec![to_expr(e2)]) }
            --
            e1:(@) spaces() (quiet!{"*"}/ expected!("infix operator")) spaces() e2:@ { println!("{:?} = {:?}", e1, e2); ExprInner::Method(to_expr(e1), Ident::from_str("times"), vec![to_expr(e2)]) }
            e1:(@) spaces() (quiet!{"/"}/ expected!("infix operator")) spaces() e2:@ { println!("{:?} = {:?}", e1, e2); ExprInner::Method(to_expr(e1), Ident::from_str("div"), vec![to_expr(e2)]) }
            e1:(@) spaces() (quiet!{"%"}/ expected!("infix operator")) spaces() e2:@ { println!("{:?} = {:?}", e1, e2); ExprInner::Method(to_expr(e1), Ident::from_str("mod"), vec![to_expr(e2)]) }
            --
            "&" space() b:("mut" space())? e:@ { println!("&{:?}", e); ExprInner::Ref(b != None, to_expr(e))}
            "*" space() e:@ { println!("&{:?}", e); ExprInner::Deref(to_expr(e)) }
            "!" space() e:@ { println!("&{:?}", e); ExprInner::Method(to_expr(e), Ident::from_str("not"), Vec::new()) }
            "-" space() e:@ { println!("&{:?}", e); ExprInner::Method(to_expr(e), Ident::from_str("neg"), Vec::new()) }
        }

        rule proj() -> Projector = precedence! {
            n:name()    { Projector::Name(n) }
            n:number()  { Projector::Int(n as usize) }
        }

    }
  }
  
pub fn test() {
    assert_eq!(list_parser::list("[1,1,2,3,5,8]"), Ok(vec![1, 1, 2, 3, 5, 8]));
}

pub fn parse_file(name : String) -> File {
    println!("parsing {}", name);
    let contents = fs::read_to_string(&name)
        .expect("Error reading file");

    match list_parser::file(&contents) {
        Ok(content) => File {content, name},
        Err(err) => {
                println!("error {:?} in file {}", err, name);
                std::process::exit(1)
            }
    }
}