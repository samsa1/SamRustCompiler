all: build
#	./test -1 target/debug/sam_rust_compiler
	./test -1d target/debug/sam_rust_compiler
#	./test -2 target/debug/sam_rust_compiler
	./test -2d target/debug/sam_rust_compiler
#	./test -3 target/debug/sam_rust_compiler
	./test -3d target/debug/sam_rust_compiler

test: build
	./test -3d target/debug/sam_rust_compiler

build:
	cargo build
