# ------------------------------------------------------------------------------
# Commands

.PHONY: test clean

# Build and run tests
test: PlintTest.native
	./PlintTest.native

# Clean all build outputs
clean:
	rm -rf _build ./PlintTest.native src/PyAstGrammar.ml

# ------------------------------------------------------------------------------
# Dependencies

PlintTest.native: src/*.ml src/PyAstGrammar.ml
	@# -w @8: Treat non-exhaustive pattern matching as an error
	@# -w @26: Treat unused variables as an error
	@# -w -30: Disables warnings about different record types sharing a key name
	ocamlbuild -use-ocamlfind \
		-cflags -w,@8@26-30 \
		src/PlintTest.native

src/PyAstGrammar.ml: src/grammar/make_python_ast_json_parser.py src/grammar/Python.asdl.json
	(cd src/grammar; python3 make_python_ast_json_parser.py > ../PyAstGrammar.ml )
