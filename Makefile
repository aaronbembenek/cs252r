COMPILER=ocamlc

all:
	$(COMPILER) -c ast.ml

# need to add executables
clean:
	-rm *.cmo *.cmi parse.ml parse.mli lex.ml
