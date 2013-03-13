
SRC=pervasives.ml list.ml hashtbl.ml \
  bar.ml foo.ml x.ml \
  variant.ml record.ml exn.ml ref.ml tuple.ml \
  for.ml let_tuple_defs.ml let_typed_def.ml \
  open_module.ml nested_module.ml \
  alias_module.ml alias_type.ml alias_module_type.ml \
  namespace_type_value.ml namespace_type_field.ml \
  letrec.ml typerec.ml letrec_nested.ml \
  alias_type_function.ml use_hashtbl.ml \
  dupe_exn.ml let_scope.ml \
  toplevel_dep.ml

# not handled well
#  functor.ml \
# include_module.ml
# alias_hashtbl.ml


#STDLIB=pervasives.cmt list.cmt hashtbl.cmt

OBJS=$(SRC:.ml=.cmt) $(STDLIB)

all: graph_code.marshall

.PHONY: graph_code.marshall

CG = ../../../codegraph
graph_code.marshall: $(OBJS)
	$(CG) -symlinks -lang cmt -build .

.SUFFIXES: .ml .cmt

.ml.cmt:
	ocamlc -c -nostdlib -nopervasives -dtypes -bin-annot $<

clean::
	rm -f *.cm* *.annot graph_code.*

clean::
	rm -f prolog_compiled_db
