all: build
#	./test -1 target/debug/sam_rust_compiler
	./test -1d target/debug/sam_rust_compiler
#	./test -2 target/debug/sam_rust_compiler
	./test -2d target/debug/sam_rust_compiler
#	./test -3 target/debug/sam_rust_compiler
	./test -3d target/debug/sam_rust_compiler

clean:
	echo "pub fn stdlib() -> Option<crate::frontend::Module<crate::ast::rust::File>> { None }"  > src/std_file.rs
	cargo fmt

test: build
	./test -3d target/debug/sam_rust_compiler

build:
	echo "pub fn stdlib() -> Option<crate::frontend::Module<crate::ast::rust::File>> { None }" > src/std_file.rs
	cargo build
	echo "Compiling std"
	./target/debug/sam_rust_compiler --generate-std
	cargo build
