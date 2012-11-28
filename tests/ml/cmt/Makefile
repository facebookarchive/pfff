all: graph_code.marshall

.PHONY: graph_code.marshall
#foo.ml nested_module.ml
graph_code.marshall: 
	ocamlc -c -bin-annot foo.ml	
	ocamlc -c -bin-annot nested_module.ml
	cg -lang cmt -build .
