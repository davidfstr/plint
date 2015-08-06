# ------------------------------------------------------------------------------
# Commands

.PHONY: test clean

# Build and run tests
test: build build/PlintTest build/test_data build/parse_ast.py
	(cd ./build; ./PlintTest)

# Test the PyAst module
# TODO: Integrate with the main unit test runner
test_ast: build/PyAst
	(cd ./build; ./PyAst)

# Clean all build outputs
clean:
	rm -rf build
	(cd ./src; rm *.cmi *.cmx *.o)

# ------------------------------------------------------------------------------
# Dependencies

build:
	mkdir build

build/PlintTest: src/PlintTest.ml
	ocamlfind ocamlopt -linkpkg \
		-package batteries \
		-package ounit \
		-o build/PlintTest \
		src/PlintTest.ml

build/PyAst: src/PyAst.ml
	ocamlfind ocamlopt -linkpkg \
		-package core -thread \
		-package sexplib.syntax -syntax camlp4o \
		-package yojson \
		-o build/PyAst \
		src/PyAst.ml

build/test_data:
	ln -s ../src/test_data build/test_data

build/parse_ast.py:
	ln -s ../src/parse_ast.py build/parse_ast.py