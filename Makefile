COMPILER=ocamlc

all:
	$(COMPILER) -c ast.ml
	ocamlfind $(COMPILER) -c -linkpkg -package aez assumptions.ml
	$(COMPILER) -c state.ml
	$(COMPILER) -c mmodel.ml
	ocamlyacc parse.mly
	$(COMPILER) -c parse.mli
	$(COMPILER) -c parse.ml
	ocamllex lex.mll
	$(COMPILER) -c lex.ml
	$(COMPILER) -c sym_error.ml
	$(COMPILER) -c eval.ml
	ocamlfind $(COMPILER) -o interp -linkpkg -package aez \
		ast.cmo state.cmo parse.cmo lex.cmo assumptions.cmo mmodel.cmo \
		sym_error.cmo eval.cmo

test:
	python test.py

clean:
	-rm *.cmo *.cmi parse.ml parse.mli lex.ml interp solve

.PHONY: test clean
