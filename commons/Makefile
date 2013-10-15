##############################################################################
# Variables
##############################################################################

-include ../Makefile.config

TARGET=lib

# Here are the sources without any external dependencies. The core of commons!

# note: if you add a file (a .mli or .ml), dont forget to redo a 'make depend'
MYSRC=common.ml common2.ml common_extra.ml \
      interfaces.ml objet.ml \
      int.ml \
      ocollection.ml \
      seti.ml \
      oset.ml oassoc.ml osequence.ml ograph.ml \
      ocollection/oseti.ml ocollection/oseth.ml ocollection/osetb.ml ocollection/osetpt.ml \
      ocollection/oassocb.ml ocollection/oassoch.ml ocollection/oassoc_buffer.ml ocollection/oassoc_cache.ml ocollection/oassocid.ml \
      oarray.ml \
      ocollection/ograph2way.ml ograph_extended.ml ograph_simple.ml  \
      ofullcommon.ml \
      math.ml \
      unicode.ml \
      simple_color.ml file_type.ml file_format.ml \
      glimpse.ml parser_combinators.ml \
      concurrency.ml parallel.ml \


# src from other authors, got from the web or caml hump, too small to be
# in their own external/ocamlxxx/
SRC=ocamlextra/dumper.ml
SRC+=ocamlextra/ANSITerminal.ml
# defunctorized version of standard set/map; g for generic
SRC+=ocamlextra/set_poly.ml ocamlextra/map_poly.ml
SRC+=ocamlextra/setPt.ml
SRC+=$(MYSRC)
SRC+=ocamlextra/enum.ml ocamlextra/dynArray.ml
SRC+=ocamlextra/suffix_tree.ml ocamlextra/suffix_tree_ext.ml
SRC+=ocamlextra/graph_ocamldot.ml
#SRC+=ocamlextra/graph_ocamldoc.ml
SRC+=ocamlextra/pretty_printer_indent.ml
SRC+=ocamlextra/oUnit.ml
SRC+=ocamlextra/xML2.ml
SRC+=ocamlextra/xHTML2.ml

SYSLIBS=str.cma bigarray.cma unix.cma nums.cma

INCLUDEDIRS=ocamlextra ocollection
SUBDIRS=ocamlextra ocollection

# sexp/json/xml are so helpful to debug code that they are considered
# fundamental and so they are part of commons.cma

ifeq ($(FEATURE_SEXP_BUILTIN_COMMONS),1)
SRC+= \
  lib-sexp/type.ml \
  lib-sexp/parser.ml \
  lib-sexp/lexer_sexp.ml \
  lib-sexp/pre_sexp.ml \
  lib-sexp/sexp_intf.ml \
  lib-sexp/sexp.ml \
  lib-sexp/conv.ml \
  lib-sexp/conv_error.ml
#  lib-sexp/sexp_path.ml
#for make depend
INCLUDEDIRS+=lib-sexp
#for make clean
SUBDIRS+=lib-sexp
endif

ifeq ($(FEATURE_JSON_BUILTIN_COMMONS),1)
SRC+= \
  lib-json/json_type.ml \
  lib-json/json_out.ml \
  lib-json/netconversion2.ml \
  lib-json/json_parser.ml \
  lib-json/json_lexer.ml \
  lib-json/json_in.ml \
  lib-json/json_io.ml \

INCLUDEDIRS+=lib-json
SUBDIRS+=lib-json
endif

SRC+=logger.ml

ifeq ($(FEATURE_XML_BUILTIN_COMMONS),1)
SRC+= \
  lib-xml/xml_types.ml \
  lib-xml/dtd_types.ml \
  lib-xml/xml_parser.ml \
  lib-xml/xml_lexer.ml \
  lib-xml/xmlParser.ml \
  lib-xml/dtd.ml \
  lib-xml/xml_parse.ml \
  lib-xml/xml_utils.ml \

INCLUDEDIRS+=lib-xml
SUBDIRS+=lib-xml
endif


ifeq ($(FEATURE_JSON_BUILTIN_COMMONS),1)
SRC+=
#NOMORE: json_common.ml
endif

ifeq ($(FEATURE_SEXP_BUILTIN_COMMONS),1)
SRC+=sexp_common.ml
SRC+=
endif

SRC+=ocaml.ml

OBJS = $(SRC:.ml=.cmo)
OPTOBJS = $(SRC:.ml=.cmx)

COBJS=realpath.o

# so one can just include common.cma to compile
# but not worth it, because does not work well for native code
ifeq ($(FEATURE_SYSLIB_BUILTIN_COMMONS),1)
BUILTINLIBS= $(SYSLIBS)
# this does not seem to work :( only ocamlc can pack .cma together ?
#BUILTINLIBSOPT= $(SYSLIBS:.cma=.cmxa)
endif

#-----------------------------------------------------------------------------
# Other common (thin wrapper) libraries
#-----------------------------------------------------------------------------

#format: XXXSRC, XXXINCLUDE, XXXSYSLIBS

#gdbm
MYGDBMSRC=ocollection/oassocdbm.ml
GDBMSYSLIBS=dbm.cma

#berkeley db (ocamlbdb)
MYBDBSRC=ocollection/oassocbdb.ml ocollection/oassocbdb_string.ml
BDBINCLUDES=-I ../external/ocamlbdb
BDBSYSLIBS=bdb.cma

#lablgtk (ocamlgtk)
MYGUISRC=gui.ml
GUIINCLUDES= -I ../external/ocamlgtk/src
#-I +lablgtk2 -I +lablgtksourceview
#GUISYSLIBS=lablgtk.cma lablgtksourceview.cma

#pycaml (ocamlpython)
MYPYSRC=python.ml
PYINCLUDES=-I ../ocamlpython -I ../../ocamlpython
PYSYSLIBS=python.cma

#ocamlmpi
MYMPISRC=distribution.ml
MPIINCLUDES=-I ../external/ocamlmpi -I +ocamlmpi
MPISYSLIBS=mpi.cma

#pcre
MYREGEXPSRC=regexp.ml
REGEXPINCLUDES=-I ../external/  -I ../../ocamlpcre/lib
#REGEXPINCLUDES=-I +pcre

#sexplib
MYSEXPSRC=sexp_common.ml
SEXPINCLUDES=-I ../ocamlsexp -I ../external/ocamltarzan/lib-sexp -I ../../ocamltarzan/lib-sexp

#binprot
MYBINSRC=
#NOMORE: bin_common.ml
BININCLUDES=-I ../ocamltarzan/lib-binprot -I ../../ocamltarzan/lib-binprot

#ocamlgraph
MYGRAPHSRC=graph.ml
GRAPHINCLUDES=-I ../external/ocamlgraph
GRAPHSYSLIBS=graph.cma

#-----------------------------------------------------------------------------
# Other stuff
#-----------------------------------------------------------------------------

#backtrace
MYBACKTRACESRC=backtrace.ml
BACKTRACEINCLUDES=-I $(shell ocamlc -where)


##############################################################################
# Generic variables
##############################################################################

INCLUDES=$(INCLUDEDIRS:%=-I %) $(INCLUDESEXTRA)

##############################################################################
# Generic ocaml variables
##############################################################################

# This flag can also be used in subdirectories so don't change its name here.
# For profiling use:  -p -inline 0
OPTFLAGS=-thread

# The OPTBIN variable is here to allow to use ocamlc.opt instead of
# ocaml, when it is available, which speeds up compilation. So
# if you want the fast version of the ocaml chain tools, set this var
# or setenv it to ".opt" in your startup script.
OPTBIN ?= #.opt

# coupling: ../Makefile.common, but want independent commons/
OCAMLCFLAGS ?= -g -dtypes $(OCAMLCFLAGS_EXTRA) -thread -w +9

# The OCaml tools.
OCAMLC =ocamlc$(OPTBIN) $(OCAMLCFLAGS) $(INCLUDES)
OCAMLOPT=ocamlopt$(OPTBIN) $(OPTFLAGS) $(INCLUDES)
OCAMLLEX = ocamllex$(OPTBIN)
OCAMLYACC= ocamlyacc -v
OCAMLDEP = ocamldep$(OPTBIN) $(INCLUDES)
OCAMLMKTOP=ocamlmktop -g -custom $(INCLUDES)

# if need C code
OCAMLMKLIB=ocamlmklib
CC=gcc

##############################################################################
# Top rules
##############################################################################
LIB=$(TARGET).cma
OPTLIB=$(LIB:.cma=.cmxa)


all: $(LIB)
all.opt: $(OPTLIB)
opt: all.opt
top: $(TARGET).top

$(LIB): $(OBJS) $(COBJS)
	$(OCAMLMKLIB) -o lib $(BUILTINLIBS) $^

$(OPTLIB): $(OPTOBJS) $(COBJS)
	$(OCAMLMKLIB) -o lib $(BUILTINLIBSOPT) $^

$(TARGET).top: $(OBJS)
	$(OCAMLMKTOP) -o $@ $(SYSLIBS) $^

clean::
	rm -f $(TARGET).top

##############################################################################
# Other commons libs target
##############################################################################

all_libs: gdbm bdb gui mpi regexp backtrace

#-----------------------------------------------------------------------------
gdbm: commons_gdbm.cma
gdbm.opt: commons_gdbm.cmxa

commons_gdbm.cma: $(MYGDBMSRC:.ml=.cmo)
	$(OCAMLC) -a -o $@ $^

commons_gdbm.cmxa: $(MYGDBMSRC:.ml=.cmx)
	$(OCAMLOPT) -a -o $@ $^


#-----------------------------------------------------------------------------
bdb:
	$(MAKE) INCLUDESEXTRA="$(BDBINCLUDES)" commons_bdb.cma
bdb.opt:
	$(MAKE) INCLUDESEXTRA="$(BDBINCLUDES)" commons_bdb.cmxa

commons_bdb.cma: $(MYBDBSRC:.ml=.cmo)
	$(OCAMLC) -a -o $@ $^

commons_bdb.cmxa: $(MYBDBSRC:.ml=.cmx)
	$(OCAMLOPT) -a -o $@ $^



#-----------------------------------------------------------------------------
gui:
	$(MAKE) INCLUDESEXTRA="$(GUIINCLUDES)" commons_gui.cma
gui.opt:
	$(MAKE) INCLUDESEXTRA="$(GUIINCLUDES)" commons_gui.cmxa

commons_gui.cma: $(MYGUISRC:.ml=.cmo)
	$(OCAMLC) -a -o $@ $^

commons_gui.cmxa: $(MYGUISRC:.ml=.cmx)
	$(OCAMLOPT) -a -o $@ $^



#-----------------------------------------------------------------------------
mpi:
	$(MAKE) INCLUDESEXTRA="$(MPIINCLUDES)" commons_mpi.cma
mpi.opt:
	$(MAKE) INCLUDESEXTRA="$(MPIINCLUDES)" commons_mpi.cmxa

commons_mpi.cma: $(MYMPISRC:.ml=.cmo)
	$(OCAMLC) -a -o $@ $^

commons_mpi.cmxa: $(MYMPISRC:.ml=.cmx)
	$(OCAMLOPT) -a -o $@ $^

#alias
distribution: mpi
distribution.opt: mpi.opt



#-----------------------------------------------------------------------------
python:
	$(MAKE) INCLUDESEXTRA="$(PYINCLUDES)" commons_python.cma
python.opt:
	$(MAKE) INCLUDESEXTRA="$(PYINCLUDES)" commons_python.cmxa


commons_python.cma: $(MYPYSRC:.ml=.cmo)
	$(OCAMLC) -a -o $@ $^

commons_python.cmxa: $(MYPYSRC:.ml=.cmx)
	$(OCAMLOPT) -a -o $@ $^


#-----------------------------------------------------------------------------
regexp:
	$(MAKE) INCLUDESEXTRA="$(REGEXPINCLUDES)" commons_regexp.cma
regexp.opt:
	$(MAKE) INCLUDESEXTRA="$(REGEXPINCLUDES)" commons_regexp.cmxa

commons_regexp.cma: $(MYREGEXPSRC:.ml=.cmo)
	$(OCAMLC) -a -o $@ $^

commons_regexp.cmxa: $(MYREGEXPSRC:.ml=.cmx)
	$(OCAMLOPT) -a -o $@ $^


#-----------------------------------------------------------------------------
backtrace: commons_backtrace.cma
backtrace.opt: commons_backtrace.cmxa

backtrace_c.o: backtrace_c.c
	$(CC) $(BACKTRACEINCLUDES) -c $^

commons_backtrace.cma: $(MYBACKTRACESRC:.ml=.cmo) backtrace_c.o
	$(OCAMLMKLIB) -o commons_backtrace $^

commons_backtrace.cmxa: $(MYBACKTRACESRC:.ml=.cmx) backtrace_c.o
	$(OCAMLMKLIB) -o commons_backtrace  $^

realpath.o: ocamlextra/realpath.c
	$(OCAMLC) -c $^ -o $@

clean::
	rm -f dllcommons_backtrace.so



#-----------------------------------------------------------------------------
sexp:
	$(MAKE) INCLUDESEXTRA="$(SEXPINCLUDES)" commons_sexp.cma
sexp.opt:
	$(MAKE) INCLUDESEXTRA="$(SEXPINCLUDES)" commons_sexp.cmxa

commons_sexp.cma: $(MYSEXPSRC:.ml=.cmo)
	$(OCAMLC) -a -o $@ $^

commons_sexp.cmxa: $(MYSEXPSRC:.ml=.cmx)
	$(OCAMLOPT) -a -o $@ $^

binprot:
	$(MAKE) INCLUDESEXTRA="$(BININCLUDES)" commons_bin.cma
binprot.opt:
	$(MAKE) INCLUDESEXTRA="$(BININCLUDES)" commons_bin.cmxa

commons_bin.cma: $(MYBINSRC:.ml=.cmo)
	$(OCAMLC) -a -o $@ $^

commons_bin.cmxa: $(MYBINSRC:.ml=.cmx)
	$(OCAMLOPT) -a -o $@ $^


#-----------------------------------------------------------------------------
graph:
	$(MAKE) INCLUDESEXTRA="$(GRAPHINCLUDES)" commons_graph.cma
graph.opt:
	$(MAKE) INCLUDESEXTRA="$(GRAPHINCLUDES)" commons_graph.cmxa

commons_graph.cma: $(MYGRAPHSRC:.ml=.cmo)
	$(OCAMLC) -a -o $@ $^

commons_graph.cmxa: $(MYGRAPHSRC:.ml=.cmx)
	$(OCAMLOPT) -a -o $@ $^

##############################################################################
# The global "features" lib wrapper
##############################################################################

features: commons_features.cma
features.opt: commons_features.cmxa

commons_features.cma: features.cmo
	$(OCAMLC) -a -o $@ $^

commons_features.cmxa: features.cmx
	$(OCAMLOPT) -a -o $@ $^


##############################################################################
# install
##############################################################################
OCAMLSTDLIB=`ocamlc -where`
DESTDIR=$(OCAMLSTDLIB)/pfff-commons

install: all all.opt
	mkdir -p $(DESTDIR)
	cp lib.cma lib.cmxa \
          dlllib.so lib.a liblib.a \
          common.mli ocaml.mli \
          $(DESTDIR)


install-findlib:
	ocamlfind install pfff-commons META \
          lib.cma lib.cmxa \
          dlllib.so lib.a liblib.a \
          common.mli ocaml.mli

uninstall-findlib:
	ocamlfind remove pfff-commons

##############################################################################
# Literate Programming rules
##############################################################################

# must be in the same order of the #include for syncweb multi files support
# to work
SRCNW=Commons.tex.nw Commons_intro.tex.nw \
      common__overview.mli.nw \
      common__basic.mli.nw common__basic_types.mli.nw \
      common__collections.mli.nw common__misc.mli.nw \
      ocommon__overview.mli.nw ocollection.mli.nw ocommon__other.mli.nw \
      extra.mli.nw \
      common.ml.nw


# user oriented, public API in some sense + ml that share code with mli
LPSRC1=common.mli \
 objet.mli \
 ocollection.mli oset.mli oassoc.mli osequence.mli oarray.mli \
 ograph.mli ograph_simple.mli ograph_extended.mli \
 glimpse.mli parser_combinators.mli

LPSRCSHARED=common.ml interfaces.ml concurrency.ml
# internal oriented
LPSRC2=

LPSRC=$(LPSRC1) $(LPSRCSHARED) $(LPSRC2)

#------------------------------------------------------------------------------
SYNCFLAGS=-md5sum_in_auxfile -less_marks

SYNCWEB=~/c__syncweb/syncweb $(SYNCFLAGS)
NOWEB=~/c__syncweb/scripts/noweblatex
OCAMLDOC=ocamldoc $(INCLUDES)

PDFLATEX=pdflatex --shell-escape

#------------------------------------------------------------------------------
sync:
	for i in $(LPSRC); do $(SYNCWEB) $(SRCNW) $$i || exit 1; done

pdf:
	$(NOWEB) Commons.tex.nw > Commons.tex
	pdflatex Commons.tex
	pdflatex Commons.tex

lpclean::
	rm -f Commons.tex Commons_total.nw

dot:

doti:

touch:
	for i in $(SRCNW); do touch $$i; done

#------------------------------------------------------------------------------
lpclean::
	rm -f $(LPSRC)
	rm -f .md5sum*

lpclean::
	rm -f *.aux *.dvi *.log *.toc

#------------------------------------------------------------------------------
WEBSITE=~/mobile/homepage/docs/
website:
	cp Commons.pdf $(WEBSITE)

##############################################################################
# Developer rules
##############################################################################

clean::
	rm -f gmon.out

forprofiling:
	$(MAKE) OPTFLAGS="-p -inline 0 " opt

dependencygraph:
	ocamldep *.mli *.ml > /tmp/dependfull.depend
	ocamldot -fullgraph /tmp/dependfull.depend > /tmp/dependfull.dot
	dot -Tps /tmp/dependfull.dot > /tmp/dependfull.ps

dependencygraph2:
	find  -name "*.ml" |grep -v "scripts" | xargs ocamldep -I commons -I globals -I ctl -I parsing_cocci -I parsing_c -I engine -I popl -I extra > /tmp/dependfull.depend
	ocamldot -fullgraph /tmp/dependfull.depend > /tmp/dependfull.dot
	dot -Tps /tmp/dependfull.dot > /tmp/dependfull.ps


##############################################################################
# Generic rules
##############################################################################

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC)  -c $<
.mli.cmi:
	$(OCAMLC)  -c $<
.ml.cmx:
	$(OCAMLOPT)  -c $<

clean::
	rm -f *.cm[iox] *.o *.a *.cma *.cmxa *.annot *.cmt *.cmti
	rm -f *~ .*~ #*#

clean::
	for i in $(SUBDIRS); do (cd $$i; \
        rm -f *.cm[iox] *.cmt* *.o *.a *.cma *.cmxa *.annot *~ .*~ ; \
        cd ..; ) \
	done

depend:
	$(OCAMLDEP) *.mli *.ml  > .depend
	for i in $(SUBDIRS); do $(OCAMLDEP) $$i/*.ml $$i/*.mli >> .depend; done

distclean::
	rm -f .depend

-include .depend
