


fn fun<T>(x : T) -> T {
    x
}

fn test32(i : i32) -> bool {
    i == 1
}

fn test64(i : u64) -> bool {
    i == 1
}

fn main() {
    let x = fun(1i32);
    let y = fun(1u64);

    if fun(test64(x)) == test32(y) {
        print!("yes\n")
    } else {
        print!("todo\n")
    }


}