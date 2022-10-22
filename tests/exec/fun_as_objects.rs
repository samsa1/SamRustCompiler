fn zero() -> i32 {
    0
}

fn one() -> i32 {
    1
}


fn main() -> () {
    let x = if zero() + 1 == one() {
        zero
    } else {
        one
    };

    if x() == 0 {
        print!("yes\n")
    } else {
        print!("oops\n")
    }
}
