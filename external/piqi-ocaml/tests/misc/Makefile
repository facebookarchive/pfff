OCAMLMAKEFILE := ../../make/OCamlMakefile


RESULT = test


SOURCES = $(PIQI_ML_FILES)


PIQI_FILES = \
	empty-record.piqi \
	\
	Ad.piqi P.piqi Protocol.piqi \
	\
	variant-1.piqi variant-2.piqi


PIQI_ML_FILES = \
	empty_record_piqi.ml \
	\
	ad.ml p.ml protocol.ml \
	\
	variant_1_piqi.ml variant_2_piqi.ml


PRE_TARGETS = $(PIQI_ML_FILES)


PIQIC = ../../piqic-ocaml/piqic-ocaml
#PIQIC_FLAGS =


export OCAMLPATH = ../..
PACKS = piqirun.pb


all: nc #top


$(PIQI_ML_FILES): $(PIQI_FILES)
	set -e; \
	for i in $^; do \
		$(PIQIC) $(PIQIC_FLAGS) $$i ; \
	done



include $(OCAMLMAKEFILE)
