include ../../Makefile.config


PROTO_FILES = riak.proto riak_kv.proto riak_search.proto

PIQI_FILES = $(PROTO_FILES:%=%.piqi)


all: $(PIQI_FILES)
	set -e; \
	for i in $^; do \
		$(PIQI) check -e ocaml $$i ; \
	done



$(PIQI_FILES): $(PROTO_FILES)
	set -e; \
	for i in $^; do \
		$(PIQI) of-proto $$i ; \
	done


clean:
	rm -f *.proto.piqi

