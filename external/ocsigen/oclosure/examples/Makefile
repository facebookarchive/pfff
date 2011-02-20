SRC=$(wildcard *.ml)
EXECS=$(SRC:.ml=.byte)
TARGETS=$(SRC:.ml=.js)
LIB=-package js_of_ocaml oclosure.cma
SCRIPTS=../scripts
OCAMLFLAGS=-package js_of_ocaml.syntax -syntax camlp4o -I $(GOOG)
OCAMLLFLAGS=
GOOG=../goog
all: $(TARGETS)

%.byte: %.ml $(GOOG)/oclosure.cma
	@echo "[CC]" $@
	ocamlfind ocamlc -linkpkg -o $@ $(OCAMLFLAGS) $(LIB) $<

%.js: %.byte
	js_of_ocaml $^ -pretty
	ocaml str.cma $(SCRIPTS)/requirements.ml $@

ocamldep:
	ocamlfind ocamldep -I $(GOOG) -I $(GOOG)/ui $(OCAMLFLAGS) $(SRC) 

clean:
	@echo "[CLEAN]"
	-rm -f *.cmo *.cmi $(TARGETS) $(EXECS) *_req.js

var: 
	@echo $(TARGETS)
	@echo $(EXECS)
