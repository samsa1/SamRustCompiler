const C1 : usize = 1;

const C5 : usize = C2 + 2 + C1;

const C2 : usize = C1 + 1;

fn main() {
    if C5 == 5 && C2 == 2 && C1 == 1 {
        print!("yes\n")
    } else {
        print!("oups\n")
    }
}