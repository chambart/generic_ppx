all: generic_ppx

generic_ppx: generic_ppx.ml
	ocamlfind ocamlc -w -40 -package compiler-libs.common -linkpkg generic_ppx.ml -o generic_ppx

test.mli: generic_ppx test.ml
	ocamlc -w -30 -i -dsource -ppx ./generic_ppx test.ml

test: generic_ppx test.ml
	ocamlc -w -30 -ppx ./generic_ppx test.ml -o test

clean:
	rm -f generic_ppx test.mli test *.cm? *.o *.a
