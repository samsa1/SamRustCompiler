
fn zeros() -> (usize, i32, ) {
    ( 0 , 0 , )
}


fn ones() -> (usize, i32,) {
    ( 0 , 0 ,)
}

fn main() {
    let x = zeros();

    let y = (x,ones(),zeros());
}