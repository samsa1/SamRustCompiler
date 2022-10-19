fn zero() -> u32 {
    0
}

fn main() {
    let x = 1;
    let y = zero();
    let v = vec![0, 1];
    v[x];
    let z = x + y;
}