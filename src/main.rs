use std::env;

pub mod ast;
mod typing;
mod frontend;

fn main() {

    for name in env::args().skip(1) {
        frontend::parser::parse_file(name);
    }
}
