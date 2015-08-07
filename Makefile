# ------------------------------------------------------------------------------
# Commands

.PHONY: test clean

# Build and run tests
test: PlintTest.native
	./PlintTest.native

# Test the PyAst module
# TODO: Integrate with the main unit test runner
test_ast: PyAst.native
	./PyAst.native

# Clean all build outputs
clean:
	rm -rf _build

# ------------------------------------------------------------------------------
# Dependencies

PlintTest.native: src/PlintTest.ml src/Subprocess.ml
	ocamlbuild -use-ocamlfind \
		src/PlintTest.native

PyAst.native: src/PyAst.ml
	# -w -30: Disables warnings about different record types sharing a key name
	ocamlbuild -use-ocamlfind \
		-cflags -w,-30 \
		src/PyAst.native
