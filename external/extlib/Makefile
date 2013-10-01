# Makefile contributed by Alain Frisch

MODULES = \
 enum bitSet dynArray extArray extHashtbl extList extString global IO option \
 pMap std uChar uTF8 base64 unzip refList optParse dllist

# the list is topologically sorted

MLI = $(MODULES:=.mli)
CMI = $(MODULES:=.cmi)
CMX = $(MODULES:=.cmx)
SRC = $(MLI) $(MODULES:=.ml) extLib.ml

.PHONY: all opt cmxs doc install uninstall clean

all: extLib.cma

extLib.cma: $(SRC)
	ocamlc $(OCAMLCFLAGS_EXTRA) -a -o extLib.cma $(SRC)

opt: extLib.cmxa
all.opt: extLib.cmxa

extLib.cmxa: $(SRC)
	ocamlopt -a -o extLib.cmxa $(SRC)

cmxs: opt
	ocamlopt -shared -linkall extLib.cmxa -o extLib.cmxs
doc:
	ocamlc -c $(MLI)
	ocamldoc -sort -html -d doc/ $(MLI)

install:
	ocamlfind install extlib META extLib.cma $(MLI) $(CMI) -optional extLib.cmxa $(CMX) extLib.cmxs extLib.a extLib.lib

uninstall:
	ocamlfind remove extlib

clean:
	rm -f *.cmo *.cmx *.o *.obj *.cmi *.cma *.cmxa *.cmxs *.a *.lib doc/*.html *.cmt*

depend: