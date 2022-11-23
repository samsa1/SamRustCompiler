


enum OptionUsize {
    MyNone,
    MySome(usize),
}


fn main() {
    let x = OptionUsize::MyNone;

    match x {
        OptionUsize::MyNone => print!("None\n"),
        OptionUsize::MySome(_) => {
            print!("something\n")
        },
    }
}