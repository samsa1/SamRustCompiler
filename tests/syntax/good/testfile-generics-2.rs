use std::ops::Add;
use std::ops::Mul;


fn f <T : Add + Mul > (x : T) -> T {
    x
}

fn f2 <T : Add > (x : T) -> T {
    x
}


fn main() {
    let x = f(1i32);
    let x = f(1i64);
}