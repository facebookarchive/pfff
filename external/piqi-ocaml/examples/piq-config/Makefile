OCAMLMAKEFILE := ../../make/OCamlMakefile


RESULT = config


SOURCES = \
	$(PIQI_ML_FILES) \
	config.ml


export OCAMLPATH = ../..
PACKS = piqirun.ext


PIQI_FILES = config.piqi
PIQI_ML_FILES = config_piqi.ml config_piqi_ext.ml


PRE_TARGETS = $(PIQI_ML_FILES)


PIQIC = ../../piqic-ocaml/piqic-ocaml
PIQIC_FLAGS = --multi-format


all: native-code #byte-code debug-code


$(PIQI_ML_FILES): $(PIQI_FILES)
	set -e; \
	for i in $^; do \
		$(PIQIC) $(PIQIC_FLAGS) $$i ; \
	done


clean::
	rm -f *.tmp.ml


include $(OCAMLMAKEFILE)
