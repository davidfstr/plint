# Plint

A Python type checker and linter that just works.

Plint's main objective is to find any misspelled function, module, method, or class names in your Python program *without* requiring you to make any up-front changes to it, such as adding type annotations.


## Requirements

* Make
* Python 3.4.x
* OCaml 4.02.1
    * On OS X, run `brew install opam` to get OPAM.
    * With OPAM, run `opam switch 4.02.1` and `eval \`opam config env\``
* ocamlfind 1.5.5
    * With OPAM, run `opam install ocamlfind`
* OUnit 2.0.0
    * With OPAM, run `opam install ounit`
* Batteries 2.3.1
    * With OPAM, run `opam install batteries`
* yojson 1.2.1
    * With OPAM, run `opam install yojson`
* core 112.35.01
    * With OPAM, run `opam install core`

## Running the unit tests

```
make test
```


## License

Copyright (c) 2015 David Foster
