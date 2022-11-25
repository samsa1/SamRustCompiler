

pub fn new<T>() -> Vec<T> {
    vec![]
}

pub fn push<T>(vec : &mut Vec<T>, t : T) {
}

pub fn len<T>(vec : &Vec<T>) -> usize {
    0
}

pub fn get<T>(vec : &Vec<T>, id : usize) -> &T {
    &vec[id]
}