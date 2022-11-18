


enum Bool {
    True,
    False,
    OneArg(usize),
    TwoArgs(bool, u16),
}


fn main() {
    let x = Bool::True;

    match x {
        Bool::True => print!("True\n"),
        Bool::OneArg(id) => {
            id + 3;
            print!("something\n")
        },
        Bool::TwoArgs(id, _) => {
            if id {
                print!("true\n")
            } else {
                print!("false\n")
            }
        }
        Bool::False => print!("False\n"),
    }
}