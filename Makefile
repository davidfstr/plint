.PHONY: test clean

test: PlintTest
	./PlintTest

clean:
	rm PlintTest *.cmi *.cmx *.o

PlintTest: PlintTest.ml
	ocamlfind ocamlopt -o PlintTest -linkpkg -package ounit,batteries PlintTest.ml
