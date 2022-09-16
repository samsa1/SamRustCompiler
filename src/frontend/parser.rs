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
        rule number() -> u64
            = n:$(['0'..='9']+) {println!("number {}", n); n.parse().unwrap() }

        rule special_chars() -> Vec<char> = precedence! {
            "\\n" {vec!['\\', '\\', 'n']}
            "\\\"" {vec!['\\', '\\', '\"']}
        }

        rule string() -> String
            = "\"" n:(([' '|'!'|'#'..='['|']'..='~']+ / special_chars())+) "\""
                { n.into_iter().flatten().collect() }

        rule char() -> char =
            n:['a'..='z' | 'A'..='Z' | '_'] { n }

        rule num() -> char =
            n:['0'..='9'] { n }

        rule char_num() -> char = precedence! {
            n:char() { n }
            n:num() { n }
        }


        pub rule list() -> Vec<u64> =
            "[" l:(number() ** ",") "]" { l }

        pub rule file() -> Vec<Decl> =
            space() d:decl()* { d }

        rule not_return() -> () = !"\n" ['\x00'..='\x7f'] { () }
        rule not_end_comment() -> () = !"*/" !"/*" n:([^'*'] / ['*']) { }

        rule comment() -> () = precedence! {
            "/*" not_end_comment()* "*/" {println!("sing comment")}
            "/*" not_end_comment()* comment() not_end_comment()* "*/" {println!("inner comment")}
        }

        rule sgn_space() -> () = precedence! {
            [' ' | '\n']  {}
            ("//" not_return()* "\n") {}
            comment() {println!("comment")}
        }

        rule spaces() -> () = (quiet!{sgn_space()+} / expected!("space")) { }
        rule _spaces() -> () = sgn_space()+ { () }

        rule space() -> () = quiet!{sgn_space()*} { () }

        rule decl() -> Decl = precedence!{
            df:decl_fun() space() { Decl::Fun(df) }
            ds:decl_struct() space() { Decl::Struct(ds) }
        }

        rule arrow_typ() -> PreType = precedence! {
            "->" space() t:typ() space() { t }
            "" { PreType::unit()}
        }


        rule decl_fun() -> DeclFun =
            "fn" spaces() n:name() "(" args:(fun_args() ** ",") space() ")"
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

        rule typ_decl() -> (Ident, PreType) =
                space() n:name() space() ":" space() t:typ() space() {(n,t)}

        rule decl_struct() -> DeclStruct =
            "struct" spaces() n:name() space() "{"
                args:(typ_decl() ** ",") ("," space())? "}" {
                println!("defined struct {:?}", n);
                DeclStruct {
                    name : n,
                    args,
                }
        }

        rule reserved() =
            "true" / "false" / "if" / "else" / "while" / "fn" / "pub" / "struct" / "as" / "let" / "mut"

        rule name() -> Ident =
            !reserved() n1:(quiet!{char()}/expected!("identifier")) n2:(char_num()*) {
                let mut n = String::from(n1);
                for c in n2.into_iter() {
                    n.push(c)
                }
                println!("parsed name {}", n);
                Ident::from_str(&n)
            }

        rule typ() -> PreType = precedence! {
            "&" space() "mut" spaces() typ:typ() {
                PreTypeInner::Ref(Box::new(typ)).to_mut()
            }
            "&" space() typ:typ() {
                PreTypeInner::Ref(Box::new(typ)).to_nonmut()
            }

            typ:typ_no_ref() { typ}
        }

        rule typ_no_ref() -> PreType = precedence! {
            n:name() "<" t:typ() ">" { PreTypeInner::IdentParametrized(n, vec![t]).to_nonmut()}
            n:name() { PreTypeInner::Ident(n).to_nonmut()}
        }
            
        rule fun_args() -> (Ident, bool, PreType) = precedence! {
            space() b:("mut" spaces())? n:name() space() ":" space() t:typ() space() { (n, b != None, t) }
        }
        
        // bloc with spaces
        rule bloc_ws() -> Bloc =
            space() b:bloc() space() { b }

        rule bloc() -> Bloc = precedence! {
            lbracket() space() "}" { Bloc { content : Vec::new() }}
            lbracket() space() b:bloc_inner() { Bloc {
                content : Vec::new()
            } }
        }

        rule bloc_inner() -> (Vec<Instr>, Option<Expr>) = precedence! {
            i:instr() space() "}" { (vec![i], None) }
            e:expr() space() "}" { (Vec::new(), Some(e)) }
            i:instr() space() t:@
                { let (mut v, opt) = t; v.push(i); (v, opt)}
        } 

        rule instr() -> Instr = precedence! {
            e:expr() space() (quiet!{";"} / expected!("end of expr")) { println!("parsed Instr"); Instr::Expr(e) }
            ";" { println!("parsed Instr"); Instr::Expr(Expr::unit()) }
            "let" spaces() b:("mut" spaces())? n:name() space() "=" e:expr_ws() ";"
                { println!("parsed Instr binding"); Instr::Binding(b != None, n, e) }
            "while" spaces() e:expr() space() b:bloc()
                { println!("parsed Instr"); Instr::While(e, b) }
            "return" spaces() e:expr()? ";"
                { println!("parsed Instr"); Instr::Return(e) }
            i:if() { println!("parsed Instr if {:?}", i); Instr::Expr(i) }
        }

        rule if() -> Expr = precedence! {
            "if" spaces() e1:expr() e2:bloc_ws()
                { println!("parsed if 1"); to_expr(ExprInner::If(e1, expr_of_bloc(e2), Expr::unit())) }
            "if" spaces() e1:expr() e2:bloc_ws() "else" spaces() e3:@
                { println!("parsed if 2"); to_expr(ExprInner::If(e1, expr_of_bloc(e2), e3)) }
            "if" spaces() e1:expr() e2:bloc_ws() "else" e3:bloc_ws()
                { println!("parsed if 3"); to_expr(ExprInner::If(e1, expr_of_bloc(e2), expr_of_bloc(e3))) }
        }

        // expression starting with spaces
        rule expr_ws() -> Expr =
            space() e:(quiet!{expr()} / expected!("expr")) space() { println!("parsed 135 {:?}", e); e }

        rule expr() -> Expr = precedence! {
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
            n:name() { ExprInner::Var(n) }
            b:bloc() { println!("bloc {:?}", b); ExprInner::Bloc(b) }
            s:string() { println!("string {}", s); ExprInner::String(s) }
            "(" e:expr_ws() ")" { println!("parsed (expr) {:?}", e); *e.content }
        }

        rule opt_expr_list() -> Vec<Expr> = precedence! {
            v:(expr_ws() ** ",") { v }
            n:spaces() { println!("empty of size unknown"); Vec::new() }
        }

        rule lsbracket() -> () = "[" {println!("here")}
        rule lbracket() -> () = "{" {println!("here 2")}

        rule expr_decl() -> (Ident, Expr) =
            space() n:name() space() ":" space() e:expr() space() {(n, e)}

        rule expr_in() -> ExprInner = precedence! {
            e:small_expr() space() "." space() n:name() space() "(" v:opt_expr_list() ")" { println!("{:?}", e); ExprInner::Method(e, n, v)}
            e:small_expr() space() "." space() n:proj() { println!("{:?}", e); ExprInner::Proj(e, n) }
            e1:small_expr() space() lsbracket() e2:expr_ws() "]" { println!("{:?}", e1); ExprInner::Method(e1, Ident::from_str("index"), vec![e2]) }
            n:name() space() "{" args:(expr_decl() ** ",") ","? space() "}"
                { println!("build struct {:?} {:?}", n, args); ExprInner::BuildStruct(n, args) }
            n:name() space() "(" v:opt_expr_list() ")" { println!("call {:?}", n); ExprInner::FunCall(n, v)}
            n:name() space() "!" space() "(" v:opt_expr_list() ")" { println!("{:?} ! {:?}", n, v); ExprInner::MacroCall(n, v) }
            "vec" space() "!" space() "[" v:opt_expr_list() "]" { println!("vec {:?}", v); ExprInner::MacroCall(Ident::from_str("vec"), v) }
            e1:@ space() (quiet!{"="}/ expected!("infix operator")) space() e2:(@) { println!("{:?} = {:?}", e1, e2); ExprInner::Method(to_expr(e1), todo!(), vec![to_expr(e2)]) }
            --
            e1:(@) space() (quiet!{"||"}/ expected!("infix operator")) space() e2:@ { println!("{:?} || {:?}", e1, e2); ExprInner::Method(to_expr(e1), Ident::from_str("or"), vec![to_expr(e2)]) }
            --
            e1:(@) space() (quiet!{"&&"}/ expected!("infix operator")) space() e2:@ { println!("{:?} && {:?}", e1, e2); ExprInner::Method(to_expr(e1), Ident::from_str("and"), vec![to_expr(e2)]) }
            --
            e1:(@) space() (quiet!{"=="}/ expected!("infix operator")) space() e2:@ { println!("{:?} == {:?}", e1, e2); ExprInner::Method(to_expr(e1), Ident::from_str("eq"), vec![to_expr(e2)]) }
            e1:(@) space() (quiet!{"!="}/ expected!("infix operator")) space() e2:@ { println!("{:?} != {:?}", e1, e2); ExprInner::Method(to_expr(e1), Ident::from_str("neq"), vec![to_expr(e2)]) }
            e1:(@) space() (quiet!{"<"}/ expected!("infix operator")) space() e2:@  { println!("{:?} < {:?}", e1, e2); ExprInner::Method(to_expr(e1), Ident::from_str("lower"), vec![to_expr(e2)]) }
            e1:(@) space() (quiet!{"<="}/ expected!("infix operator")) space() e2:@ { println!("{:?} <= {:?}", e1, e2); ExprInner::Method(to_expr(e1), Ident::from_str("lower_eq"), vec![to_expr(e2)]) }
            e1:(@) space() (quiet!{">"}/ expected!("infix operator")) space() e2:@  { println!("{:?} > {:?}", e1, e2); ExprInner::Method(to_expr(e1), Ident::from_str("greater"), vec![to_expr(e2)]) }
            e1:(@) space() (quiet!{">="}/ expected!("infix operator")) space() e2:@ { println!("{:?} >= {:?}", e1, e2); ExprInner::Method(to_expr(e1), Ident::from_str("greater_eq"), vec![to_expr(e2)]) }
            --
            e1:(@) space() (quiet!{"+"}/ expected!("infix operator")) space() e2:@ { println!("{:?} + {:?}", e1, e2); ExprInner::Method(to_expr(e1), Ident::from_str("add"), vec![to_expr(e2)]) }
            e1:(@) space() (quiet!{"-"}/ expected!("infix operator")) space() e2:@ { println!("{:?} - {:?}", e1, e2); ExprInner::Method(to_expr(e1), Ident::from_str("sub"), vec![to_expr(e2)]) }
            --
            e1:(@) space() (quiet!{"*"}/ expected!("infix operator")) space() e2:@ { println!("{:?} * {:?}", e1, e2); ExprInner::Method(to_expr(e1), Ident::from_str("times"), vec![to_expr(e2)]) }
            e1:(@) space() (quiet!{"/"}/ expected!("infix operator")) space() e2:@ { println!("{:?} / {:?}", e1, e2); ExprInner::Method(to_expr(e1), Ident::from_str("div"), vec![to_expr(e2)]) }
            e1:(@) space() (quiet!{"%"}/ expected!("infix operator")) space() e2:@ { println!("{:?} % {:?}", e1, e2); ExprInner::Method(to_expr(e1), Ident::from_str("mod"), vec![to_expr(e2)]) }
            --
            "&" space() b:("mut" space())? e:@ { println!("&{:?}", e); ExprInner::Ref(b != None, to_expr(e))}
            "*" space() e:@ { println!("&{:?}", e); ExprInner::Deref(to_expr(e)) }
            "!" space() e:@ { println!("&{:?}", e); ExprInner::Method(to_expr(e), Ident::from_str("not"), Vec::new()) }
            "-" space() e:@ { println!("&{:?}", e); ExprInner::Method(to_expr(e), Ident::from_str("neg"), Vec::new()) }
            e:(quiet!{small_expr_i()} / expected!("value")) { println!("{:?}", e); e }
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