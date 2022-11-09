

fn arithmetic(i : i64) -> i64 {
    let x = (i << 10) * 543 + 252 - 245;
    let y = 12;
    x >> y << 3
}

fn loop_operations(n : i64) -> i64 {
    let mut i2 = 0;
    let mut s = 0;

    while i2 < n {
        s = (s & arithmetic(i2)) + arithmetic(i2);
        i2 = i2 + 1
    }
    s
}



fn main() {
    loop_operations(1000_000_000);
}