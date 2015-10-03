COMPILER=ocamlc

all:
	$(COMPILER) -c ast.ml

yacc: com
	ocamlyacc parse.mly
	$(COMPILER) -c parse.mli
	$(COMPILER) -c parse.ml
	ocamllex lex.mll
	$(COMPILER) -c lex.ml
	$(COMPILER) -c yacc.ml
	$(COMPILER) -o cs252yacc ast.cmo parse.cmo lex.cmo eval.cmo yacc.cmo

# need to add executables
clean:
	-rm *.cmo *.cmi parse.ml parse.mli lex.ml
