# Factorisation in Z[X]

## Introduction 
This project aims to implement the factorisation of polynomials in OCaml. 

&rarr; TODO: Use OWL

## Dune 
[Dune](https://dune.build/) is a build system for OCaml projects. Using it, you can build executables, libraries, run tests, and much more.
### Useful commands
- `dune build`: build the current project.
- `dune exec fact`: execute the main file.
- `dune utop lib`: play with the lib files in the toplevel utop.
- `dune build doc`: create the documentation with odoc.

### Make warning non-fatal 
Put that everywhere :)
```dune
(env
  (dev
    (flags (:standard -warn-error -A))))
```
