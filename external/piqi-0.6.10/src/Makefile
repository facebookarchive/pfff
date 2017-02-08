include ../Makefile.config
include $(PIQI_ROOT)/make/Makefile.ocaml


RESULT = piqi


PACKS = unix

# pick piqilib manually -- we don't install it to the build directory to keep it
# in sync with ocamldep-generated stuff; at the same time, we avoid extending
# OCAMLPATH to reduce complexity of the windows build (e.g. avoid converting :
# path separators to ; along with reverting slashes); we can't simply symlink it
# to the build dir either because symlinks don't work on windows
PACKS += ulex easy-format xmlm base64
INCDIRS = ../piqilib
LIBS = ../piqilib/piqilib


SOURCES = \
	$(PIQI_ML_FILES) \
	main.ml \
	\
	convert.ml \
	check.ml \
	expand.ml \
	light.ml \
	getopt.ml \
	compile.ml \
	cc.ml \
	\
	json_pp.ml \
	pp.ml \
	\
	to_proto.ml \
	of_proto.ml \
	\
	piqi_http.ml \
	piqi_rpc.ml \
	\
	server.ml \
	call.ml \
	\
	run.ml


# make sure piqi gets re-linked when the piqilib library changes
RESULTDEPS := $(wildcard ../piqilib/piqi.cm*a)

# allow ocamldep to pick up ../piqilib/*.cm[ox] as dependencies for ./*.cm[ox]
DINCDIRS := ../piqilib


PIQI_FILES = descriptor.proto.piqi piqi_tools.piqi piqi_rpc.piqi
PIQI_ML_FILES = descriptor_piqi.ml piqi_tools_piqi.ml piqi_rpc_piqi.ml


PIQI_OCAML_DIR ?= ../../piqi-ocaml
PIQIC = $(PIQI_OCAML_DIR)/piqic-ocaml/piqic-ocaml
PIQIC_FLAGS = --pp --runtime Piqi_piqirun


#OCAMLFLAGS += -verbose
#OCAMLLDFLAGS += -verbose


all: piqilib nc


debug: piqilib dc top


# NOTE: we have to unset these variables to prevent piqilib's OCamlMakefile from
# inheriting them
piqilib:
	unset RESULTDEPS PACKS LIBS INCDIRS DINCDIRS REAL_OCAMLFIND && $(MAKE) -C ../piqilib $(MAKECMDGOALS)


# rebuilding *_piqi.ml from .piqi -- called manually as "make piqi" when there
# were changes in the .piqi files or to take advantage of new versions of
# piqic-ocaml and the piqirun.pb runtime library
piqi:
	$(PIQIC) $(PIQIC_FLAGS) descriptor.proto.piqi
	$(PIQIC) $(PIQIC_FLAGS) piqi_rpc.piqi
	$(PIQIC) $(PIQIC_FLAGS) --embed-piqi piqi_tools.piqi


# re-creating descriptor.proto.piqi from a (newer) descriptor.proto
# -- called as manually "make descriptor" when when there are changes in the
# upstream descriptor.proto; should be followed by "make piqi" to generate
# descriptor_piqi.ml
descriptor: descriptor.proto
	./piqi of-proto --normalize $<
	$(MAKE) piqi


.PHONY: descriptor piqi piqilib


include $(OCAMLMAKEFILE)
