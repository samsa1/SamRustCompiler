mod module1;
mod module2;


fn main() {
    module1::exec1();
    module2::exec();
    module2::module3::exec_top();
    let module1 = 3;
    if module1 == 3 {
        print!("test\n")
    }
}