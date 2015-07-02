OCAMLMAKEFILE := ../../make/OCamlMakefile


RESULT = riak_pb


SOURCES = \
	$(PIQI_ML_FILES)


PROTO_FILES = riak.proto riak_kv.proto riak_search.proto

PIQI_FILES = $(PROTO_FILES:%=%.piqi)

PIQI_ML_FILES = $(PROTO_FILES:%.proto=%_piqi.ml)


export OCAMLPATH = ../..
PACKS = piqirun.pb


PIQI ?= piqi
PIQIC = ../../piqic-ocaml/piqic-ocaml
#PIQIC_FLAGS =


PRE_TARGETS = $(PIQI_FILES) $(PIQI_ML_FILES)


all: native-code-library #byte-code


$(PIQI_ML_FILES): $(PIQI_FILES)
	set -e; \
	for i in $^; do \
		$(PIQIC) $(PIQIC_FLAGS) $$i ; \
	done


$(PIQI_FILES): $(PROTO_FILES)
	set -e; \
	for i in $^; do \
		$(PIQI) of-proto $$i ; \
	done


clean::
	rm -f *.tmp.ml *.proto.piqi *.ml


include $(OCAMLMAKEFILE)
