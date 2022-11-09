
struct S1 {
    a : bool,
    x : i32,
    y : i32,
    z : bool,
}


struct S2 {
    a : S1,
    b : S1,
}


struct S3 {
    a : S2,
    b : S2,
    c : S2,
}

fn build1() -> S1 {
    S1 {
        a : false,
        x : 4,
        y : 3,
        z : true,
    }
}

fn build2() -> S2 {
    S2 {
        a : build1(),
        b : build1()
    }
}

fn build3() -> S3 {
    S3 {
        a : build2(),
        b : build2(),
        c : build2(),
    }
}

fn test(s : S3) -> i32 {
    s.a.a.x + s.a.a.y +
    s.a.b.x + s.a.b.y +
    s.b.a.x + s.b.a.y +
    s.b.b.x + s.b.b.y +
    s.c.a.x + s.c.a.y +
    s.c.b.x + s.c.b.y
}


fn main() {

    let mut i = 0;
    while i < 300_000_000 {
        test(build3());
        i = i + 1;
    }
}