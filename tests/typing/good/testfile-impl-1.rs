struct S {
    x : i32,
    y : u32,
}

impl S {
    fn get_x(&self) -> i32 {
        self.x
    }

    fn get_y(&self) -> u32 {
        self.y
    }
}

fn main() {
    let s = S { x : 0, y : 0};
    let _ = s.get_x() + 1i32;
}