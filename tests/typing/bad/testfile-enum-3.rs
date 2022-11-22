


enum Bool {
    True,
    False(Info),
}

struct Info {
    field : Bool,
}


fn main() {
    let x = Bool::True;
}