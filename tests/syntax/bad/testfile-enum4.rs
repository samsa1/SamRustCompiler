


enum Bool {
    True,
    False,
    OneArg(usize),
}


fn main() {
    let x = Bool::True;

    match1 {
        Bool::True => print!("True\n")
        Bool::OneArg(id) => {
            id + 3;
            print!("something\n")
        },
        Bool::False => print!("False\n"),
    }
}