

fn f<T : Add + >(x : T) -> T {
    x
}

fn main() {
    let x = f(3);
}