
SRC=pervasives.ml list.ml hashtbl.ml \
  bar.ml foo.ml \
  variant.ml record.ml exn.ml ref.ml tuple.ml \
  for.ml let_tuple_defs.ml \
  nested_module.ml alias_module.ml

#STDLIB=pervasives.cmt list.cmt hashtbl.cmt

OBJS=$(SRC:.ml=.cmt) $(STDLIB)

all: graph_code.marshall

.PHONY: graph_code.marshall


graph_code.marshall: $(OBJS)
	cg -symlinks -lang cmt -build .

.SUFFIXES: .ml .cmt

.ml.cmt:
	ocamlc -c -nostdlib -nopervasives -bin-annot $<

clean:
	rm -f *.cm* graph_code.*