COMPILER=ocamlc

weak:
	make build MMODEL=WEAK

seq:
	make build MMODEL=SEQ

build:
	$(COMPILER) -c ast.ml
	$(COMPILER) -c prettyprint.ml
	ocamlfind $(COMPILER) -c -linkpkg -package aez assumptions.ml
	$(COMPILER) -c state.ml
	$(COMPILER) -c -pp "cppo -D $(MMODEL)" mmodel.ml
	ocamlyacc parse.mly
	$(COMPILER) -c parse.mli
	$(COMPILER) -c parse.ml
	ocamllex lex.mll
	$(COMPILER) -c lex.ml
	ocamlfind $(COMPILER) -c -linkpkg -package yojson log.ml
	$(COMPILER) -c eval.ml
	ocamlfind $(COMPILER) -o interp -linkpkg -package aez -package yojson \
		ast.cmo state.cmo parse.cmo lex.cmo assumptions.cmo mmodel.cmo \
		prettyprint.cmo log.cmo eval.cmo

test:
	python test.py

clean:
	-rm *.cmo *.cmi parse.ml parse.mli lex.ml interp

.PHONY: test clean build seq weak
