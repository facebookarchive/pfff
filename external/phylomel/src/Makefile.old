OCAMLMAKEFILE = ../OCamlMakefile

SOURCES = \
	distMat.mli \
	distMat.ml \
	genotype.mli \
	genotype.ml \
	genotypes.mli \
	genotypes.ml \
	genoMat.mli \
	genoMat.ml \
	dynMat.mli \
	dynMat.ml \
	svg.ml \
	dendogram.mli \
	dendogram.ml \
	vec2.ml \
	tree.mli \
	tree.ml \
	phylogram.ml \
	clustering.mli \
	clustering.ml \
	barnesHut.mli \
	barnesHut.ml \
	forceDirectedLayout.ml \

LIB_PACK_NAME = phylomel
OCAMLFLAGS = -for-pack Phylomel -dtypes -unsafe
OCAMLNCFLAGS = -inline 1000
PACKS = extlib
RESULT = phylomel

all: byte-code-library native-code-library

LIBINSTALL_FILES = \
	phylomel.cmi phylomel.cma phylomel.cmxa phylomel.a

install: libinstall
uninstall: libuninstall

clean :: clean-doc

DOC_DIR = ../doc

-include $(OCAMLMAKEFILE)