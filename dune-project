(lang dune 3.7)
(name libpoly_bindings)
(using ctypes 0.3)

(generate_opam_files true)

(version 0.2)


(maintainers "Stephane Graham-Lengrand <stephane.graham-lengrand@csl.sri.com>")
(authors "Stephane Graham-Lengrand <stephane.graham-lengrand@csl.sri.com>")
(license GLPv3)
(homepage "https://github.com/SRI-CSL/libpoly_ocaml_bindings")
(bug_reports "https://github.com/SRI-CSL/libpoly_ocaml_bindings/issues")

(package
 (name libpoly_bindings)
 (synopsis "OCaml bindings for libpoly")
 (description "OCaml bindings for libpoly")
 (depends
  (ocaml (>= 4.12))
  ctypes
  ctypes-foreign
  zarith))
