/* arbres binaires de recherche */

struct BST {
    value: i32,
    sub: Vec<BST> // de taille 0 ou 2
  }
  
fn null() -> BST {
    let r = BST { value: 42, sub: vec![] };
    r
}

impl BST {
  fn is_null(&self) -> bool {
    self.sub.len() == 0
  }
   
  fn contient(&self, x: i32) -> bool {
    if x == self.value { return true; }
    if x < self.value && !self.sub[0].is_null() { return self.sub[0].contient(x); }
    if !self.sub[1].is_null() { return self.sub[1].contient(x); }
    return false;
  }

  fn print(&self) {
    print!("(");
    if !self.sub[0].is_null() { self.sub[0].print() }
    print_int(self.value);
    if !self.sub[1].is_null() { self.sub[1].print() }
    print!(")");
  }
}

// Petit Rust ne permet pas de définir ces raccourcis ; tant pis...

// fn left(a: & BST) -> & BST { & a.sub[0] }
// fn right(a: & BST) -> & BST { & a.sub[1] }

// fn left_mut(a: &mut BST) -> &mut BST { &mut a.sub[0] }
// fn right_mut(a: &mut BST) -> &mut BST { &mut a.sub[1] }

fn leaf(v: i32) -> BST {
    let r = BST { value: v, sub: vec![null(), null()] };
    r
}
  
fn insert(a: &mut BST, x: i32) {
  if x == a.value { return; }
  if x < a.value {
    if a.sub[0].is_null()
      { a.sub[0] = leaf(x); }
    else
      { insert(&mut a.sub[0], x); }
  } else {
    if a.sub[1].is_null()
      { a.sub[1] = leaf(x); }
    else
      { insert(&mut a.sub[1], x); }
  }
}
  
fn print_bool(b: bool) {
    if b { print!("true\n") } else { print!("false\n") }
}

fn print_int(x: i32) {
    if x < 0 { print!("-") }
    else if x > 0 { print!("+") }
    else { print!("0") }
}

fn main() {
    let mut d = leaf(1);
    insert(&mut d, 17);
    insert(&mut d, -5);
    insert(&mut d, 8);
    print_bool(d.contient(-5));
    print_bool(d.contient(0));
    print_bool(d.contient(17));
    print_bool(d.contient(3));
    insert(&mut d, 42);
    insert(&mut d, 8);
    insert(&mut d, -1000);
    insert(&mut d, 0);
    d.print(); print!("\n")
}
  