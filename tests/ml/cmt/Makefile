
SRC=foo.ml nested_module.ml
OBJS=$(SRC:.ml=.cmt)

all: graph_code.marshall

.PHONY: graph_code.marshall


graph_code.marshall: $(OBJS)
	cg -lang cmt -build .

.SUFFIXES: .ml .cmt

.ml.cmt:
	ocamlc -bin-annot $<
