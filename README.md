# RML

An ML-style language with liquid refinement types.

## Installation

This project is written in OCaml and uses opam for package management.
The following guide explains how to install OCaml and opam at the same time.

- https://ocaml.org/docs/up-and-running

### Dependencies

Once installed opam can be used to install dependencies of the
project.
This project uses
 
- `dune` for build management
- `base`/`core` as standard library replacements
- `menhir` for parsing
- `z3` as an SMT solver

Install the above dependencies with the command:

```
opam install dune base core menhir z3
```

## Usage

Compile the project using the command:

```
dune build --profile=release
```

which produces an executable called `main.exe` in the subfolder
`_build/default/bin`.

Alternatively build and execute RML at the same time by running:

```
dune exec rml <file>
```

For example, running 

```
dune exec rml examples/inc.rml
```

should produce the output `5`, whereas running

```
dune exec rml examples/inc_bad.rml
```

should fail with the following liquid type error:

```
Liquid type error: unsatisfiable refinement in file "examples/ok/inc_bad.rml" line 7 character 10 to line 9 character 1
```

## Project structure

- `bin` contains the entry point of the program in `main.ml` 
- `lib` contains the libraries that implement the interpreter and type checking
- `examples` contains some example programs

## Commit hooks

Set up pre-commit hooks with `make hooks`.

The commit hook scripts are stored in the `.hooks/` folder
which git doesn't read from,
so `make hooks` copies the contents of `.hooks/` to `.git/hooks/`.
If you change any files in `.hooks/` you'll need to run `make hooks` again.