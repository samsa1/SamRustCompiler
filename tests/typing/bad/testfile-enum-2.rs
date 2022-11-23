


enum Bool {
    True,
    False,
}


fn main() {
    let x = Bool::True;

    match x {
        Bool::True => 1usize,
        Bool::False => 1i32,
    };
}