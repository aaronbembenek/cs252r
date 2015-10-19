COMPILER=ocamlc

all:
	$(COMPILER) -c ast.ml
	$(COMPILER) -c state.ml
	ocamlyacc parse.mly
	$(COMPILER) -c parse.mli
	$(COMPILER) -c parse.ml
	ocamllex lex.mll
	$(COMPILER) -c lex.ml
	$(COMPILER) -c eval.ml
	ocamlfind $(COMPILER) -o interp -linkpkg -package aez \
		ast.cmo state.cmo parse.cmo lex.cmo eval.cmo
	ocamlfind $(COMPILER) -o solve -linkpkg -package aez assumptions.ml
# $(COMPILER) -I +alt-ergo-zero unix.cma nums.cma aez.cma assumptions.ml -o solve

clean:
	-rm *.cmo *.cmi parse.ml parse.mli lex.ml interp solve
