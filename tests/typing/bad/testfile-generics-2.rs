use std::ops::Add;

struct S {

}

fn f<T : Add> (x : T) -> T {
    x
}

fn main() {
    f(S {});
}