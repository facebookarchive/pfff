TOP=../..
##############################################################################
# Variables
##############################################################################
TARGET=lib

-include $(TOP)/Makefile.config

SRC= flag_parsing_cpp.ml \
     token_cpp.ml ast_cpp.ml \
     type_cpp.ml \
     meta_ast_cpp.ml \
     visitor_cpp.ml lib_parsing_cpp.ml \
     parser_cpp_mly_helper.ml parser_cpp.ml lexer_cpp.ml \
     token_helpers_cpp.ml token_views_cpp.ml token_views_context.ml \
     parsing_hacks_lib.ml pp_token.ml \
     parsing_hacks_pp.ml parsing_hacks_cpp.ml parsing_hacks_typedef.ml \
     parsing_hacks_define.ml \
     parsing_hacks.ml \
     parsing_recovery_cpp.ml \
     parse_cpp.ml \
     test_parsing_cpp.ml unit_parsing_cpp.ml \

SYSLIBS= str.cma unix.cma 

LIBS=$(TOP)/commons/lib.cma \
 $(TOP)/h_program-lang/lib.cma

INCLUDEDIRS= \
 $(TOP)/commons $(TOP)/commons/ocamlextra \
 $(TOP)/globals \
 $(TOP)/h_program-lang \
 $(TOP)/matcher

##############################################################################
# Generic variables
##############################################################################

-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################
all:: $(TARGET).cma
all.opt:: $(TARGET).cmxa

$(TARGET).cma: $(OBJS)
	$(OCAMLC) -a -o $(TARGET).cma $(OBJS)

$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $(TARGET).cmxa $(OPTOBJS)

$(TARGET).top: $(OBJS) $(LIBS)
	$(OCAMLMKTOP) -o $(TARGET).top $(SYSLIBS) $(LIBS) $(OBJS)

clean::
	rm -f $(TARGET).top


lexer_cpp.ml: lexer_cpp.mll
	$(OCAMLLEX) $<
clean::
	rm -f lexer_cpp.ml
beforedepend:: lexer_cpp.ml


parser_cpp.ml parser_cpp.mli: parser_cpp.mly
	$(OCAMLYACC) $<
clean::
	rm -f parser_cpp.ml parser_cpp.mli parser_cpp.output
beforedepend:: parser_cpp.ml parser_cpp.mli


visitor_cpp.cmo: visitor_cpp.ml
	$(OCAMLC) -w y -c $<

parsing_hacks.cmo: parsing_hacks.ml
	$(OCAMLC) -w -9 -c $<
parsing_hacks_cpp.cmo: parsing_hacks_cpp.ml
	$(OCAMLC) -w -9 -c $<
parsing_hacks_pp.cmo: parsing_hacks_pp.ml
	$(OCAMLC) -w -9 -c $<
parsing_hacks_typedef.cmo: parsing_hacks_typedef.ml
	$(OCAMLC) -w -9 -c $<
token_views_context.cmo: token_views_context.ml
	$(OCAMLC) -w -9 -c $<


##############################################################################
# install
##############################################################################
LIBNAME=pfff-lang_cpp
EXPORTSRC=meta_ast_cpp.mli \
   parser_cpp.mli parse_cpp.mli \
   lib_parsing_cpp.mli visitor_cpp.mli \

install-findlib:
	ocamlfind install $(LIBNAME) META   lib.cma lib.cmxa lib.a \
          $(EXPORTSRC) $(EXPORTSRC:%.mli=%.cmi) \
          ast_cpp.ml ast_cpp.cmi
