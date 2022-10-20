
fn get_sum(x : &(i32, i32, i32, i32)) -> i32 {
    x.0 + x.1 + x.2 + x.3
}

fn main() {

    let mut x = (1, 2, 3, 4);

    if x.0 + x.1 == x.3 - 1 { print!("yes\n") } else { print!("oups\n") }
    if x.2 == 3 { print!("yes\n") } else { print!("oups\n") }

    x.1 = x.1 + x.3;

    if x.1 == 2 * x.2 { print!("yes\n") } else { print!("oups\n") }

    if get_sum(&x) == 14 { print!("yes\n") } else { print!("oups\n") }



}