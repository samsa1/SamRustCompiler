use std::ops::Add;

pub fn f<T : Add + PartialEq + Copy>(x : T) -> bool {
    let x2 = x + x;
    x == x
}

fn main() {
    let x1 = f(5i32);
    let x2 = f(3u8);
    let x3 = f(9usize);

    if x1 {
        print!("success\n")
    } else {
        print!("fail\n")
    }
}