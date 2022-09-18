use crate::ast::{common::*, rust::*};
use std::fs;


fn to_expr(start : usize, end : usize, e : ExprInner) -> Expr {
    Expr {
        content : Box::new(e),
        typed : None,
        loc : Location::new(start, end),
    }
}

fn expr_of_bloc(b : Bloc) -> Expr {
    Expr {
        loc : b.loc,
        content : Box::new(ExprInner::Bloc(b)),
        typed : None,
    }
}

fn test_no_cmp(e: &Expr) {
    match &*e.content {
        | ExprInner::Method(_, id, _) => {
            if id.get_content() == "eq"
                || id.get_content() == "neq"
                || id.get_content() == "greater"
                || id.get_content() == "greater_eq"
                || id.get_content() == "lower"
                || id.get_content() == "lower_eq" {
                    println!("invalid syntax");
                    std::process::exit(1)
            }
        },
        | _ => (),
    }
}

peg::parser!{
    grammar list_parser() for str {
        rule number() -> ((usize, usize), u64)
            = p1:position!() n:$(['0'..='9']+) p2:position!() { ((p1, p2), n.parse().unwrap()) }

        rule special_chars() -> Vec<char> = precedence! {
            "\\n"   {vec!['\\', '\\', 'n']}
            "\\\""  {vec!['\\', '\\', '\"']}
            "\\\\"  {vec!['\\', '\\']}
        }

        rule string() -> ((usize, usize), String)
            = p1:position!() "\"" n:((['\t'|'\n'|' '|'!'|'#'..='['|']'..='~']+ / special_chars())*) "\"" p2:position!()
                { ((p1, p2), n.into_iter().flatten().collect()) }

        rule char() -> char =
            n:['a'..='z' | 'A'..='Z' | '_'] { n }

        rule num() -> char =
            n:['0'..='9'] { n }

        rule char_num() -> char = precedence! {
            n:char() { n }
            n:num() { n }
        }

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
            ("//" [^'\n']* "\n") {}
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

        rule fun_arg() -> (Ident, bool, PreType) =
            space() b:("mut" spaces())? n:name() space() ":" space() t:typ() space() { (n, b != None, t) }

        rule fun_args() -> Vec<(Ident, bool, PreType)> = precedence! {
            args:(fun_arg() ++ ",")    { args }
            space()                     { Vec::new() }
        }


        rule decl_fun() -> DeclFun =
            "fn" spaces() n:name() "(" args:fun_args() ")"
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

        rule typ_decls() -> Vec<(Ident, PreType)> = precedence! {
            v:(typ_decl() ++ ",") ","? space() { v }
            space() { Vec::new() }
        }

        rule decl_struct() -> DeclStruct =
            "struct" spaces() n:name() space() "{"
                args:typ_decls() "}" {
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
        
        // bloc with spaces
        rule bloc_ws() -> Bloc =
            space() b:bloc() space() { b }

        rule bloc() -> Bloc = precedence! {
            start:position!() "{" space() b:bloc_inner() { Bloc {
                content : b.0.into_iter().rev().collect(),
                expr : b.1,
                loc : Location::new(start, b.2),
            } }
        }

        rule bloc_inner() -> (Vec<Instr>, Option<Expr>, usize) = precedence! {
            i:instr() space() "}" end:position!() { (vec![i], None, end) }
            e:expr() space() "}" end:position!() { (Vec::new(), Some(e), end) }
            i:instr() space() t:@
                { let (mut v, opt, end) = t; v.push(i); (v, opt, end)}
            "}" end:position!() { (Vec::new(), None, end) }
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
            start:position!() "if" !char_num() space() e1:expr() e2:bloc_ws() "else" spaces() e3:@
                { println!("parsed if 2"); to_expr(start, e3.loc.end(), ExprInner::If(e1, expr_of_bloc(e2), e3)) }
            start:position!() "if" !char_num() space() e1:expr() e2:bloc_ws() "else" e3:bloc_ws()
                { println!("parsed if 3"); to_expr(start, e3.loc.end(), ExprInner::If(e1, expr_of_bloc(e2), expr_of_bloc(e3))) }
            start:position!() "if" !char_num() space() e1:expr() e2:bloc_ws()
                { println!("parsed if 1"); to_expr(start, e2.loc.end(), ExprInner::If(e1, expr_of_bloc(e2), Expr::unit())) }
        }

        // expression starting with spaces
        rule expr_ws() -> Expr =
            space() e:(quiet!{expr()} / expected!("expr")) space() { println!("parsed 135 {:?}", e); e }

/*        rule expr() -> Expr = precedence! {
            e:expr_in() { println!("parsed expr_in : 139 {:?}", e); to_expr(e) }
        }*/

        rule true_expr() -> Expr =
            start:position!() "true" end:position!() { to_expr(start, end, ExprInner::Bool(true)) }

        rule false_expr() -> Expr =
            start:position!() "false" end:position!() { to_expr(start, end, ExprInner::Bool(false)) }

        rule small_expr() -> Expr = precedence! {
            n:number() { to_expr(n.0.0, n.0.1, ExprInner::Int(n.1 as usize)) }
            t:true_expr()   { t }
            f:false_expr()  { f }
            n:name() { to_expr(n.get_loc().start(), n.get_loc().end(), ExprInner::Var(n)) }
            b:bloc() { expr_of_bloc(b) }
            s:string() { to_expr(s.0.0, s.0.1, ExprInner::String(s.1)) }
            start:position!() "(" e:expr_ws() ")" end:position!() { to_expr(start, end, ExprInner::Parenthesis(e)) }
            e:@ space() "." space() n:name() space() "(" v:opt_expr_list() ")" end:position!() { to_expr(e.loc.start(), end, ExprInner::Method(e, n, v))}
            e:@ space() "." space() n:proj() end:position!() { to_expr(e.loc.start(), end, ExprInner::Proj(e, n)) }
        }

        rule opt_expr_list() -> Vec<Expr> = precedence! {
            v:(expr_ws() ++ ",") { v }
            n:space() { println!("empty of size unknown"); Vec::new() }
        }

        rule lsbracket() -> () = "[" {println!("here")}
        rule lbracket() -> () = "{" {println!("here 2")}

        rule expr_decl() -> (Ident, Expr) =
            space() n:name() space() ":" space() e:expr() space() {(n, e)}

        rule expr() -> Expr = precedence! {
            e1:small_expr() space() "[" e2:expr_ws() "]" end:position!()
                { to_expr(e1.loc.start(), end,
                    ExprInner::Method(e1, Ident::from_str("index"), vec![e2])) }
            n:name() space() "{" args:(expr_decl() ** ",") ","? space() "}" end:position!()
                { to_expr(n.get_loc().start(), end, ExprInner::BuildStruct(n, args)) }
            n:name() space() "(" v:opt_expr_list() ")" end:position!()
                { to_expr(n.get_loc().start(), end, ExprInner::FunCall(n, v)) }
            n:name() space() "!" space() "(" v:opt_expr_list() ")" end:position!()
                { to_expr(n.get_loc().start(), end, ExprInner::MacroCall(n, v)) }
            start:position!() "vec" space() "!" space() "[" v:opt_expr_list() "]" end:position!()
                { to_expr(start, end, ExprInner::MacroCall(Ident::from_str("vec"), v)) }
            e1:@ space() (quiet!{"="}/ expected!("infix operator")) space() e2:(@)
                { to_expr(e1.loc.start(), e2.loc.end(), ExprInner::Method(e1, Ident::from_str("set"), vec![e2])) }
            --
            e1:(@) space() (quiet!{"||"}/ expected!("infix operator")) space() e2:@
                { to_expr(e1.loc.start(), e2.loc.end(), ExprInner::Method(e1, Ident::from_str("or"), vec![e2])) }
            --
            e1:(@) space() (quiet!{"&&"}/ expected!("infix operator")) space() e2:@
                { to_expr(e1.loc.start(), e2.loc.end(), ExprInner::Method(e1, Ident::from_str("and"), vec![e2])) }
            --
            e1:(@) space() (quiet!{"=="}/ expected!("infix operator")) space() e2:@ {
                test_no_cmp(&e1); test_no_cmp(&e2);
                to_expr(e1.loc.start(), e2.loc.end(), ExprInner::Method(e1, Ident::from_str("eq"), vec![e2])) }
            e1:(@) space() (quiet!{"!="}/ expected!("infix operator")) space() e2:@ {
                test_no_cmp(&e1); test_no_cmp(&e2);
                to_expr(e1.loc.start(), e2.loc.end(), ExprInner::Method(e1, Ident::from_str("neq"), vec![e2])) }
            e1:(@) space() (quiet!{"<"}/ expected!("infix operator")) space() e2:@  {
                test_no_cmp(&e1); test_no_cmp(&e2);
                to_expr(e1.loc.start(), e2.loc.end(), ExprInner::Method(e1, Ident::from_str("lower"), vec![e2])) }
            e1:(@) space() (quiet!{"<="}/ expected!("infix operator")) space() e2:@ {
                test_no_cmp(&e1); test_no_cmp(&e2);
                to_expr(e1.loc.start(), e2.loc.end(), ExprInner::Method(e1, Ident::from_str("lower_eq"), vec![e2])) }
            e1:(@) space() (quiet!{">"}/ expected!("infix operator")) space() e2:@  {
                test_no_cmp(&e1); test_no_cmp(&e2);
                to_expr(e1.loc.start(), e2.loc.end(), ExprInner::Method(e1, Ident::from_str("greater"), vec![e2])) }
            e1:(@) space() (quiet!{">="}/ expected!("infix operator")) space() e2:@ {
                test_no_cmp(&e1); test_no_cmp(&e2);
                to_expr(e1.loc.start(), e2.loc.end(), ExprInner::Method(e1, Ident::from_str("greater_eq"), vec![e2])) }
            --
            e1:(@) space() (quiet!{"+"}/ expected!("infix operator")) space() e2:@
                { to_expr(e1.loc.start(), e2.loc.end(), ExprInner::Method(e1, Ident::from_str("and"), vec![e2])) }
            e1:(@) space() (quiet!{"-"}/ expected!("infix operator")) space() e2:@
                { to_expr(e1.loc.start(), e2.loc.end(), ExprInner::Method(e1, Ident::from_str("sub"), vec![e2])) }
            --
            e1:(@) space() (quiet!{"*"}/ expected!("infix operator")) space() e2:@
                { to_expr(e1.loc.start(), e2.loc.end(), ExprInner::Method(e1, Ident::from_str("times"), vec![e2])) }
            e1:(@) space() (quiet!{"/"}/ expected!("infix operator")) space() e2:@
                { to_expr(e1.loc.start(), e2.loc.end(), ExprInner::Method(e1, Ident::from_str("div"), vec![e2])) }
            e1:(@) space() (quiet!{"%"}/ expected!("infix operator")) space() e2:@
                { to_expr(e1.loc.start(), e2.loc.end(), ExprInner::Method(e1, Ident::from_str("mod"), vec![e2])) }
            --
            start:position!() "&" space() b:("mut" space())? e:@ { to_expr(start, e.loc.end(), ExprInner::Ref(b != None, e)) }
            start:position!() "*" space() e:@ { to_expr(start, e.loc.end(), ExprInner::Deref(e)) }
            start:position!() "!" space() e:@ { to_expr(start, e.loc.end(), ExprInner::Method(e, Ident::from_str("not"), Vec::new())) }
            start:position!() "-" space() e:@ { to_expr(start, e.loc.end(), ExprInner::Method(e, Ident::from_str("neg"), Vec::new())) }
            e:(quiet!{small_expr()} / expected!("value")) { println!("{:?}", e); e }
        }

        rule proj() -> Projector = precedence! {
            n:name()    { Projector::Name(n) }
            n:number()  { Projector::Int(n.1 as usize) }
        }

    }
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