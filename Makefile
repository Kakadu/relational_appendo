.PHONY: test deps
all: test

test:
	dune test peano reverso

deps:
	opam install -y camlp5 ocamlfind mtime.1.4.0 ppxlib.0.28.0 bisect_ppx ppx_inline_test ppx_expect logger-p5