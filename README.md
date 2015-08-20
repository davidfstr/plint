# Plint

A Python type checker and linter that just works.

Plint's main objective is to find any misspelled function, module, method, or class names in your Python program *without* requiring you to make any up-front changes to it, such as adding type annotations.

## Status

Presently Plint is **pre-alpha** and undergoing heavy development. See the [wiki] for more information.

[wiki]: https://github.com/davidfstr/plint/wiki


## Requirements

* Python 3.4.x
    * Download from [python.org](https://www.python.org/downloads/).
* OPAM
    * On OS X, run `brew install opam` to get OPAM.
* Make
    * On OS X, installed by default.

With OPAM, install the remaining dependencies:

```
opam switch 4.02.1     # OCaml 4.02.1
eval `opam config env`

opam install ocamlfind # ocamlfind 1.5.5
opam install ounit     # OUnit 2.0.0
opam install batteries # Batteries 2.3.1
opam install yojson    # yojson 1.2.1
opam install core      # core 112.35.01
```

## Running the unit tests

```
make test
```


## License

Copyright (c) 2015 David Foster
