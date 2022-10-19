use crate::ast::{common::*, rust::*};
use std::fs;
use std::str::FromStr;

fn to_expr(start: usize, end: usize, e: ExprInner) -> Expr {
    Expr {
        content: Box::new(e),
        typed: None,
        loc: Location::new(start, end),
    }
}

fn expr_of_bloc(b: Bloc) -> Expr {
    Expr {
        loc: b.loc,
        content: Box::new(ExprInner::Bloc(b)),
        typed: None,
    }
}

fn test_no_cmp(e: &Expr) {
    match &*e.content {
        ExprInner::BinaryOp(op, _, _)
            if *op == BinOperator::Eq
                || *op == BinOperator::Ne
                || *op == BinOperator::Lower
                || *op == BinOperator::LowerEq
                || *op == BinOperator::Greater
                || *op == BinOperator::GreaterEq =>
        {
            println!("invalid syntax");
            std::process::exit(1)
        }
        _ => (),
    }
}

peg::parser! {
  grammar list_parser() for str {
      rule number() -> ((usize, usize), u64)
          = p1:position!() n:$(['0'..='9']+) p2:position!() { ((p1, p2), n.parse().unwrap()) }

      rule special_chars() -> Vec<char> = precedence! {
          "\\n"   {vec!['\\', 'n']}
          "\\\""  {vec!['\\', '\"']}
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

      rule not_return() -> () = !"\n" ['\x00'..='\x7f'] { }
      rule not_end_comment() -> () = !"*/" !"/*" n:([^'*'] / ['*']) { }

      rule comment() -> () = precedence! {
          "/*" not_end_comment()* "*/" {}
          "/*" not_end_comment()* comment() not_end_comment()* "*/" {}
      }

      rule sgn_space() -> () = precedence! {
          [' ' | '\n' | '\t']  {}
          ("//" [^'\n']* "\n") {}
          comment() {}
      }

      rule spaces() -> () = (quiet!{sgn_space()+} / expected!("space")) { }

      rule space() -> () = quiet!{sgn_space()*} { }

      pub rule file() -> (Vec<Open>, Vec<Decl>) =
          space() o:open()*
          d:decl()* { (o, d) }

      rule open() -> Open = precedence!{
          "use" spaces() p:path() r:rename()? space() ";" space()
            { Open::Use(p, r) }
          p:("pub" spaces())? "mod" spaces() n:name() r:rename()? space() ";" space()
            { Open::Mod(p.is_some(), n, r) }
      }

      rule path() -> Path<PreType> = precedence!{
        n:name() "::" p:path_tl() {
            let (end, mut path_rev) = p;
            let loc = Location::new(n.get_loc().start(), end);
            path_rev.push(NamePath::Name(n));
            Path::new(path_rev.into_iter().rev().collect(), loc)
        }
        n:name() {
            let loc = n.get_loc();
            Path::new(vec![NamePath::Name(n)], loc)
        }
      }

      rule path_tl() -> (usize, Vec<NamePath<PreType>>) = precedence!{
        n:name() "::" p:path_tl() { let (pos, mut p) = p; p.push(NamePath::Name(n)); (pos, p) }
        n:name() pos:position!() { (pos, vec![NamePath::Name(n)]) }
      }

      rule rename() -> Ident =
        spaces() "as" spaces() n:name() { n }

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
          p:("pub" spaces())?
          "fn" spaces() n:name() "(" args:fun_args() ")"
                  space()
                  out:arrow_typ() b:bloc()
              {
                  DeclFun {
                      public : None::<()>.is_some(),
                      name : n,
                      args,
                      output : out,
                      content : b,
                      id_counter : IdCounter::new(),
                  }
              }

      rule typ_decl() -> (Ident, PreType) =
              space() n:name() space() ":" space() t:typ() space() {(n,t)}

      rule typ_decls() -> Vec<(Ident, PreType)> = precedence! {
          v:(typ_decl() ++ ",") ","? space() { v }
          space() { Vec::new() }
      }

      rule decl_struct() -> DeclStruct =
          p:("pub" spaces())?
          "struct" spaces() n:name() space() "{"
              args:typ_decls() "}" {
              DeclStruct {
                  public : None::<()>.is_some(),
                  name : n,
                  args,
              }
      }

      rule reserved_inner() =
          "fn" / "pub" / "struct" / "enum" / "mod" / "use"
          / "as" / "let" / "mut"
          / "true" / "false"
          / "if" / "else" / "while" / "for" / "in" / "return"

      rule reserved() = reserved_inner() [^('0'..='9' | 'a'..='z' | 'A' ..='Z' | '_')]

      rule name() -> Ident =
          start:position!() n:name_inner() end:position!() {
              Ident::new_from(n, start, end)
          }

      rule name_inner() -> String =
          !reserved() n1:(quiet!{char()}/expected!("identifier")) n2:(char_num()*) {
              let mut n = String::from(n1);
              for c in n2.into_iter() {
                  n.push(c)
              }
              n
          }

      rule typ() -> PreType = precedence! {
          "&" space() "mut" spaces() typ:typ() {
              PreTypeInner::Ref(true, Box::new(typ)).to_type()
          }
          "&" space() typ:typ() {
              PreTypeInner::Ref(false, Box::new(typ)).to_type()
          }

          typ:typ_no_ref() { typ}
      }

      rule typ_no_ref() -> PreType = precedence! {
          n:name() "<" t:typ() ">" { PreTypeInner::IdentParametrized(n, vec![t]).to_type()}
          n:name() { PreTypeInner::Ident(n).to_type()}
      }

      // bloc with spaces
      rule bloc_ws() -> Bloc =
          space() b:bloc() space() { b }

      rule bloc() -> Bloc = precedence! {
          start:position!() "{" space() b:bloc_inner() { Bloc {
              content : b.0.into_iter().rev().collect(),
              loc : Location::new(start, b.1),
          } }
      }

      rule bloc_inner() -> (Vec<Instr>, usize) = precedence! {
          i:instr() space() "}" end:position!() { (vec![i], end) }
          e:expr() space() "}" end:position!() { (vec![Instr::Expr(ComputedValue::Keep, e)], end) }
          i:instr() space() t:@
              { let (mut v, end) = t; v.push(i); (v, end)}
          "}" end:position!() { (Vec::new(), end) }
      }

      rule instr() -> Instr = precedence! {
          ";" { Instr::Expr(ComputedValue::Drop, Expr::unit()) }
          "let" spaces() b:("mut" spaces())? n:name() space() "=" e:expr_ws() ";"
              { Instr::Binding(b != None, n, e) }
          "while" spaces() e:expr() space() b:bloc()
              { Instr::While(e, b) }
          "return" space() ";"
              { Instr::Return(None) }
          "return" spaces() e:expr() ";"
              { Instr::Return(Some(e)) }
          e:expr() space() (quiet!{";"} / expected!("end of expr")) { Instr::Expr(ComputedValue::Drop, e) }
          i:if() { Instr::Expr(ComputedValue::Keep, i) }
      }

      rule if() -> Expr = precedence! {
          p:if_head() "else" spaces() e3:@ {
              let (start, e1, e2) = p;
              to_expr(start, e3.loc.end(), ExprInner::If(e1, e2, Bloc::from_expr(e3))) }
          p:if_head() "else" e3:bloc_ws() {
              let (start, e1, e2) = p;
              to_expr(start, e3.loc.end(), ExprInner::If(e1, e2, e3)) }
          p:if_head() {
              let (start, e1, e2) = p;
              to_expr(start, e2.loc.end(), ExprInner::If(e1, e2, Bloc::empty())) }
      }

      rule if_head() -> (usize, Expr, Bloc) = precedence! {
          start:position!() "if" !char_num() space() e1:expr_no_bracket() e2:bloc_ws()
              { (start, e1, e2) }
          start:position!() "if" !char_num() space() e1:expr() e2:bloc_ws()
              { (start, e1, e2) }
      }

      // expression starting with spaces
      rule expr_ws() -> Expr =
          space() e:(quiet!{expr()} / expected!("expr")) space() { e }

      rule true_expr() -> Expr =
          start:position!() "true" end:position!() { to_expr(start, end, ExprInner::Bool(true)) }

      rule false_expr() -> Expr =
          start:position!() "false" end:position!() { to_expr(start, end, ExprInner::Bool(false)) }

      rule small_expr() -> Expr = precedence! {
          n:number() { to_expr(n.0.0, n.0.1, ExprInner::Int(n.1)) }
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
          n:space() { Vec::new() }
      }

      rule expr_decl() -> (Ident, Expr) =
          space() n:name() space() ":" space() e:expr() space() {(n, e)}

      rule expr() -> Expr = precedence! {
          e1:small_expr() space() "[" e2:expr_ws() "]" end:position!()
              { to_expr(e1.loc.start(), end,
                  ExprInner::Index(e1, e2)) }
          n:name() space() "{" args:(expr_decl() ** ",") ","? space() "}" end:position!()
              { to_expr(n.get_loc().start(), end, ExprInner::BuildStruct(n, args)) }
          n:name() space() "(" v:opt_expr_list() ")" end:position!()
              { to_expr(n.get_loc().start(), end, ExprInner::FunCall(vec![], n, v)) }
          n:name() space() "!" space() "(" v:opt_expr_list() ")" end:position!()
              { to_expr(n.get_loc().start(), end, ExprInner::MacroCall(n, v)) }
          start:position!() "vec" space() "!" space() "[" v:opt_expr_list() "]" end:position!()
              { to_expr(start, end, ExprInner::MacroCall(Ident::from_str("vec").unwrap(), v)) }
          e1:@ space() (quiet!{"="}/ expected!("infix operator")) space() e2:(@)
              { to_expr(e1.loc.start(), e2.loc.end(), ExprInner::BinaryOp(BinOperator::Set, e1, e2)) }
          --
          e1:(@) space() (quiet!{"||"}/ expected!("infix operator")) space() e2:@
              { to_expr(e1.loc.start(), e2.loc.end(), ExprInner::BinaryOp(BinOperator::Or, e1, e2)) }
          --
          e1:(@) space() (quiet!{"&&"}/ expected!("infix operator")) space() e2:@
              { to_expr(e1.loc.start(), e2.loc.end(), ExprInner::BinaryOp(BinOperator::And, e1, e2)) }
          --
          e1:(@) space() (quiet!{"=="}/ expected!("infix operator")) space() e2:@ {
              test_no_cmp(&e1); test_no_cmp(&e2);
              to_expr(e1.loc.start(), e2.loc.end(), ExprInner::BinaryOp(BinOperator::Eq, e1, e2)) }
          e1:(@) space() (quiet!{"!="}/ expected!("infix operator")) space() e2:@ {
              test_no_cmp(&e1); test_no_cmp(&e2);
              to_expr(e1.loc.start(), e2.loc.end(), ExprInner::BinaryOp(BinOperator::Ne, e1, e2)) }
          e1:(@) space() (quiet!{"<"}/ expected!("infix operator")) space() e2:@  {
              test_no_cmp(&e1); test_no_cmp(&e2);
              to_expr(e1.loc.start(), e2.loc.end(), ExprInner::BinaryOp(BinOperator::Lower, e1, e2)) }
          e1:(@) space() (quiet!{"<="}/ expected!("infix operator")) space() e2:@ {
              test_no_cmp(&e1); test_no_cmp(&e2);
              to_expr(e1.loc.start(), e2.loc.end(), ExprInner::BinaryOp(BinOperator::LowerEq, e1, e2)) }
          e1:(@) space() (quiet!{">"}/ expected!("infix operator")) space() e2:@  {
              test_no_cmp(&e1); test_no_cmp(&e2);
              to_expr(e1.loc.start(), e2.loc.end(), ExprInner::BinaryOp(BinOperator::Greater, e1, e2)) }
          e1:(@) space() (quiet!{">="}/ expected!("infix operator")) space() e2:@ {
              test_no_cmp(&e1); test_no_cmp(&e2);
              to_expr(e1.loc.start(), e2.loc.end(), ExprInner::BinaryOp(BinOperator::GreaterEq, e1, e2)) }
          --
          e1:(@) space() (quiet!{"+"}/ expected!("infix operator")) space() e2:@
              { to_expr(e1.loc.start(), e2.loc.end(), ExprInner::BinaryOp(BinOperator::Add, e1, e2)) }
          e1:(@) space() (quiet!{"-"}/ expected!("infix operator")) space() e2:@
              { to_expr(e1.loc.start(), e2.loc.end(), ExprInner::BinaryOp(BinOperator::Sub, e1, e2)) }
          --
          e1:(@) space() (quiet!{"*"}/ expected!("infix operator")) space() e2:@
              { to_expr(e1.loc.start(), e2.loc.end(), ExprInner::BinaryOp(BinOperator::Mul, e1, e2)) }
          e1:(@) space() (quiet!{"/"}/ expected!("infix operator")) space() e2:@
              { to_expr(e1.loc.start(), e2.loc.end(), ExprInner::BinaryOp(BinOperator::Div, e1, e2)) }
          e1:(@) space() (quiet!{"%"}/ expected!("infix operator")) space() e2:@
              { to_expr(e1.loc.start(), e2.loc.end(), ExprInner::BinaryOp(BinOperator::Mod, e1, e2)) }
          --
          start:position!() "&" space() b:("mut" space())? e:@ { to_expr(start, e.loc.end(), ExprInner::Ref(b != None, e)) }
          start:position!() "*" space() e:@ { to_expr(start, e.loc.end(), ExprInner::Deref(e)) }
          start:position!() "!" space() e:@ { to_expr(start, e.loc.end(), ExprInner::UnaryOp(UnaOperator::Not, e)) }
          start:position!() "-" space() e:@ { to_expr(start, e.loc.end(), ExprInner::UnaryOp(UnaOperator::Neg, e)) }
          e:(quiet!{small_expr()} / expected!("value")) { e }
      }

      rule expr_no_bracket() -> Expr = precedence! {
          e1:small_expr() space() "[" e2:expr_ws() "]" end:position!()
              { to_expr(e1.loc.start(), end,
                  ExprInner::Index(e1, e2)) }
          n:name() space() "(" v:opt_expr_list() ")" end:position!()
              { to_expr(n.get_loc().start(), end, ExprInner::FunCall(vec![], n, v)) }
          n:name() space() "!" space() "(" v:opt_expr_list() ")" end:position!()
              { to_expr(n.get_loc().start(), end, ExprInner::MacroCall(n, v)) }
          start:position!() "vec" space() "!" space() "[" v:opt_expr_list() "]" end:position!()
              { to_expr(start, end, ExprInner::MacroCall(Ident::from_str("vec").unwrap(), v)) }
              e1:@ space() (quiet!{"="}/ expected!("infix operator")) space() e2:(@)
              { to_expr(e1.loc.start(), e2.loc.end(), ExprInner::BinaryOp(BinOperator::Set, e1, e2)) }
          --
          e1:(@) space() (quiet!{"||"}/ expected!("infix operator")) space() e2:@
              { to_expr(e1.loc.start(), e2.loc.end(), ExprInner::BinaryOp(BinOperator::Or, e1, e2)) }
          --
          e1:(@) space() (quiet!{"&&"}/ expected!("infix operator")) space() e2:@
              { to_expr(e1.loc.start(), e2.loc.end(), ExprInner::BinaryOp(BinOperator::And, e1, e2)) }
          --
          e1:(@) space() (quiet!{"=="}/ expected!("infix operator")) space() e2:@ {
              test_no_cmp(&e1); test_no_cmp(&e2);
              to_expr(e1.loc.start(), e2.loc.end(), ExprInner::BinaryOp(BinOperator::Eq, e1, e2)) }
          e1:(@) space() (quiet!{"!="}/ expected!("infix operator")) space() e2:@ {
              test_no_cmp(&e1); test_no_cmp(&e2);
              to_expr(e1.loc.start(), e2.loc.end(), ExprInner::BinaryOp(BinOperator::Ne, e1, e2)) }
          e1:(@) space() (quiet!{"<"}/ expected!("infix operator")) space() e2:@  {
              test_no_cmp(&e1); test_no_cmp(&e2);
              to_expr(e1.loc.start(), e2.loc.end(), ExprInner::BinaryOp(BinOperator::Lower, e1, e2)) }
          e1:(@) space() (quiet!{"<="}/ expected!("infix operator")) space() e2:@ {
              test_no_cmp(&e1); test_no_cmp(&e2);
              to_expr(e1.loc.start(), e2.loc.end(), ExprInner::BinaryOp(BinOperator::LowerEq, e1, e2)) }
          e1:(@) space() (quiet!{">"}/ expected!("infix operator")) space() e2:@  {
              test_no_cmp(&e1); test_no_cmp(&e2);
              to_expr(e1.loc.start(), e2.loc.end(), ExprInner::BinaryOp(BinOperator::Greater, e1, e2)) }
          e1:(@) space() (quiet!{">="}/ expected!("infix operator")) space() e2:@ {
              test_no_cmp(&e1); test_no_cmp(&e2);
              to_expr(e1.loc.start(), e2.loc.end(), ExprInner::BinaryOp(BinOperator::GreaterEq, e1, e2)) }
          --
          e1:(@) space() (quiet!{"+"}/ expected!("infix operator")) space() e2:@
              { to_expr(e1.loc.start(), e2.loc.end(), ExprInner::BinaryOp(BinOperator::Add, e1, e2)) }
          e1:(@) space() (quiet!{"-"}/ expected!("infix operator")) space() e2:@
              { to_expr(e1.loc.start(), e2.loc.end(), ExprInner::BinaryOp(BinOperator::Sub, e1, e2)) }
          --
          e1:(@) space() (quiet!{"*"}/ expected!("infix operator")) space() e2:@
              { to_expr(e1.loc.start(), e2.loc.end(), ExprInner::BinaryOp(BinOperator::Mul, e1, e2)) }
          e1:(@) space() (quiet!{"/"}/ expected!("infix operator")) space() e2:@
              { to_expr(e1.loc.start(), e2.loc.end(), ExprInner::BinaryOp(BinOperator::Div, e1, e2)) }
          e1:(@) space() (quiet!{"%"}/ expected!("infix operator")) space() e2:@
              { to_expr(e1.loc.start(), e2.loc.end(), ExprInner::BinaryOp(BinOperator::Mod, e1, e2)) }
          --
          start:position!() "&" space() b:("mut" space())? e:@ { to_expr(start, e.loc.end(), ExprInner::Ref(b != None, e)) }
          start:position!() "*" space() e:@ { to_expr(start, e.loc.end(), ExprInner::Deref(e)) }
          start:position!() "!" space() e:@ { to_expr(start, e.loc.end(), ExprInner::UnaryOp(UnaOperator::Not, e)) }
          start:position!() "-" space() e:@ { to_expr(start, e.loc.end(), ExprInner::UnaryOp(UnaOperator::Neg, e)) }
          e:(quiet!{small_expr()} / expected!("value")) { e }
      }

      rule proj() -> Projector = precedence! {
          n:name()    { Projector::Name(n) }
          n:number()  { Projector::Int(n.1 as usize) }
      }

  }
}

pub fn parse_file(name: String) -> File {
    println!("parsing {}", name);
    let contents = fs::read_to_string(&name).expect("Error reading file");

    match list_parser::file(&contents) {
        Ok((dep, content)) => File {
            err_reporter: ErrorReporter::new(name.clone(), contents),
            dep,
            content,
            name,
        },
        Err(err) => {
            println!("parsing error {:?} in file {}", err, name);
            std::process::exit(1)
        }
    }
}
