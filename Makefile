COMPILER=ocamlc

all:
	$(COMPILER) -c ast.ml
	ocamlyacc parse.mly
	$(COMPILER) -c parse.mli
	$(COMPILER) -c parse.ml
	ocamllex lex.mll
	$(COMPILER) -c lex.ml
	$(COMPILER) -c eval.ml
	$(COMPILER) -o interp ast.cmo parse.cmo lex.cmo eval.cmo

clean:
	-rm *.cmo *.cmi parse.ml parse.mli lex.ml interp
