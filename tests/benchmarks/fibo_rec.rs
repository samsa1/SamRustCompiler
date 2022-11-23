

fn fibo(n : usize) -> i32 {
    if n < 2 {
        0
    } else {
        fibo(n - 1) + fibo(n - 2)
    }
}


fn main() {
    fibo(46);
}