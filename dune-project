(lang dune 3.3)

(name rml)

(using menhir 2.1)

(generate_opam_files true)

(source
 (github tabbeyy/rml))

(authors "Tom Abbott")

(license LICENSE)

(package
 (name rml)
 (synopsis "A functional programming language with refinement types")
 (depends ocaml dune z3))
