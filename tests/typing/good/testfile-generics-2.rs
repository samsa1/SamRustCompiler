use std::ops::Add;
use std::ops::Mul;


fn f <T : Add > (x : T) -> T {
    x + x
}


fn main() {
    let x = f(1i32);
    let x = f(1i64);
}