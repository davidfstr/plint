# ------------------------------------------------------------------------------
# Commands

.PHONY: test clean

# Build and run tests
test: PlintTest.native
	./PlintTest.native

# Clean all build outputs
clean:
	rm -rf _build

# ------------------------------------------------------------------------------
# Dependencies

PlintTest.native: src/PlintTest.ml src/Subprocess.ml src/PyAst.ml
	# -w -30: Disables warnings about different record types sharing a key name
	ocamlbuild -use-ocamlfind \
		-cflags -w,-30 \
		src/PlintTest.native
