

eff: eff.ml  
	ocamlopt -ccopt -static eff.ml -o eff

clean: eff *.cmi *.cmx *.o
	rm eff *.cmi *.cmx *.o 