

enum E1 {
    Nothing1,
    Nothing2,

    Something(usize),
}


enum E2 {
    APair(E1, E1),
    Singleton(E1),
    Nothing1
}

impl E1 {
    fn new(id : usize) -> Self {
        Self::Something(id)
    }

    fn val(&self) -> usize {
        match self {
            Self::Something(id) => *id,
            _ => 0,
        }
    }
}


impl E2 {
    fn pair(id1 : usize, id2 : usize) -> Self {
        Self::APair(E1::new(id1), E1::new(id2))
    }

    fn sing(id : usize) -> Self {
        Self::Singleton(E1::new(id))
    }

    fn sum(&self) -> usize {
        match self {
            Self::APair(e1, e2) => e1.val() + e2.val(),
            Self::Singleton(e) => e.val(),
            Self::Nothing1 => 0,
        }
    }
}


fn main() {
    let val1 = E2::pair(5, 54);
    let val2 = E2::sing(val1.sum());

    let val3 = E2::Nothing1;

    if val1.sum() == 57 {
        print!("yes\n")
    } else {
        print!("oups\n")
    }
    if val2.sum() == val1.sum() {
        print!("yes\n")
    } else {
        print!("oups\n")
    }

    if val3.sum() == 0 {
        print!("yes\n")
    } else {
        print!("oups\n")
    }


}