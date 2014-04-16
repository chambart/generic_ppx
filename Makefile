all: generic_ppx special_format.cmo
run: all test_format
	./test_format

generic_ppx: generic_ppx.ml
	ocamlfind ocamlc -w -40 -package compiler-libs.common -linkpkg generic_ppx.ml -o generic_ppx

test.mli: generic_ppx test.ml
	ocamlc -w -30 -i -ppx ./generic_ppx test.ml > test.mli

test: generic_ppx test.ml
	ocamlc -w -30 -ppx ./generic_ppx test.ml -o test

test_other: generic_ppx test_other.ml
	ocamlc -w -30 -ppx ./generic_ppx test_other.ml -o test_other

special_format.mli: generic_ppx special_format.ml
	ocamlc -w -30 -i -ppx ./generic_ppx special_format.ml > special_format.mli

special_format.cmi: generic_ppx special_format.mli
	ocamlc -w -30 -ppx ./generic_ppx -c special_format.mli

special_format.cmo: generic_ppx special_format.cmi special_format.ml
	ocamlc -w -30 -ppx ./generic_ppx -c special_format.ml

test_format: generic_ppx special_format.cmo test_format.ml
	ocamlc -w -30 -ppx ./generic_ppx \
	  special_format.cmo test_format.ml -o test_format

clean:
	rm -f generic_ppx test_format special_format.mli test.mli test *.cm? *.o *.a a.out
