(lang dune 2.5)
(name filum)
(version 0.1.0)

(generate_opam_files true)

(source (github oss6/filum))
(license MIT)
(authors "Ossama Edbali <ossedb@gmail.com>")
(maintainers "Ossama Edbali <ossedb@gmail.com>")

(package
 (name filum)
 (synopsis "A string manipulation library")
 (depends
  (ocaml (>= 4.07))
  dune
  base
  re2))
