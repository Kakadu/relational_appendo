(lang dune 3.6)

(cram enable)

(generate_opam_files true)

(name demo)

(package
 (name demo)
 (allow_empty)
 (depends
  dune-configurator
  ocamlfind
  camlp5
  logger-p5
  (GT
   (>= 0.5.2))
  (benchmark :with-test)
  ocamlformat
  ppx_inline_test
  (mtime
   (< 2.0.0))))
