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

PlintTest.native: src/*.ml
	@# -w @8: Treat non-exhaustive pattern matching as an error
	@# -w -30: Disables warnings about different record types sharing a key name
	ocamlbuild -use-ocamlfind \
		-cflags -w,@8-30 \
		src/PlintTest.native
