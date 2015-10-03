COMPILER=ocamlc

all: yacc
	$(COMPILER) -c ast.cmo parse.cmo lex.cmo eval.cmo yacc.cmo

yacc: com
	ocamlyacc parse.mly
	$(COMPILER) -c parse.mli
	$(COMPILER) -c parse.ml
	ocamllex lex.mll
	$(COMPILER) -c lex.ml
	$(COMPILER) -c yacc.ml

com:
	$(COMPILER) -c ast.ml
	$(COMPILER) -c eval.ml

# need to add executables
clean:
	-rm *.cmo *.cmi parse.ml parse.mli lex.ml
