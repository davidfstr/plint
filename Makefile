# ------------------------------------------------------------------------------
# Commands

.PHONY: test clean

# Build and run tests
test: build build/PlintTest build/test_data build/parse_ast.py
	(cd ./build; ./PlintTest)

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
		-package ounit,batteries \
		-o build/PlintTest \
		src/PlintTest.ml

build/test_data:
	ln -s ../src/test_data build/test_data

build/parse_ast.py:
	ln -s ../src/parse_ast.py build/parse_ast.py