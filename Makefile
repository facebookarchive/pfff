#############################################################################
# Configuration section
#############################################################################

-include Makefile.config

##############################################################################
# Variables
##############################################################################
TOP:=$(shell pwd)

SRC=find_source.ml

TARGET=pfff

#------------------------------------------------------------------------------
# Program related variables
#------------------------------------------------------------------------------

PROGS=pfff \
 sgrep spatch \
 stags \
 scheck \
 codequery \
 codeslicer \
 pfff_db

PROGS+=pfff_test

ifeq ($(FEATURE_VISUAL), 1)
PROGS+=codemap codegraph
endif

OPTPROGS= $(PROGS:=.opt)

#------------------------------------------------------------------------------
#package dependencies
#------------------------------------------------------------------------------

#format: XXXDIR, XXXCMD, XXXCMDOPT, XXXINCLUDE (if different XXXDIR), XXXCMA
#template:
#  ifeq ($(FEATURE_XXX), 1)
#  XXXDIR=xxx
#  XXXCMD= $(MAKE) -C xxx &&  $(MAKE) xxx -C commons
#  XXXCMDOPT= $(MAKE) -C xxx &&  $(MAKE) xxx.opt -C commons
#  XXXCMA=xxx/xxx.cma  commons/commons_xxx.cma
#  XXXSYSCMA=xxx.cma
#  XXXINCLUDE=xxx
#  else
#  XXXCMD=
#  XXXCMDOPT=
#  endif

JSONDIR=external/jsonwheel
JSONCMA=external/jsonwheel/lib.cma

ifeq ($(FEATURE_VISUAL),1)
GUIDIR=external/ocamlgtk
GUICMD= $(MAKE) all -C $(GUIDIR) && $(MAKE) gui -C commons
GUICMDOPT= $(MAKE) opt -C $(GUIDIR) && $(MAKE) gui.opt -C commons;
GTKINCLUDE=external/ocamlgtk/src

CAIRODIR=external/ocamlcairo
CAIROINCLUDE=external/ocamlcairo/src

VISUALDIRS=code_map code_graph
endif

# should be FEATURE_OCAMLGRAPH, or should give dependencies between features
GRAPHCMA=external/ocamlgraph/ocamlgraph.cma commons/graph/lib.cma
GRAPHDIR=external/ocamlgraph
GRAPHCMD= $(MAKE) all -C $(GRAPHDIR) && $(MAKE) -C commons/graph
GRAPHCMDOPT= $(MAKE) all.opt -C $(GRAPHDIR) && $(MAKE) all.opt -C commons/graph

ifeq ($(FEATURE_BYTECODE), 1)
ZIPDIR=external/ocamlzip
ZIPCMA=external/ocamlzip/zip.cma
EXTLIBDIR=external/extlib
EXTLIBCMA=external/extlib/extLib.cma
PTDIR=external/ptrees
PTCMA=external/ptrees/ptrees.cma
JAVALIBDIR=external/javalib/src
JAVALIBCMA=external/javalib/src/lib.cma

BYTECODEDIRS=lang_bytecode/parsing lang_bytecode/analyze
BYTECODECMAS=lang_bytecode/parsing/lib.cma lang_bytecode/analyze/lib.cma
endif

ifeq ($(FEATURE_CMT), 1)
OCAMLCOMPILERDIR=$(shell ocamlc -where)/compiler-libs
OCAMLCOMPILERCMA=ocamlcommon.cma

CMTDIRS=lang_cmt/parsing lang_cmt/analyze
CMTCMAS=lang_cmt/parsing/lib.cma lang_cmt/analyze/lib.cma

endif

#------------------------------------------------------------------------------
# Main variables
#------------------------------------------------------------------------------
SYSLIBS=nums.cma bigarray.cma str.cma unix.cma

SYSLIBS+=$(OCAMLCOMPILERCMA)

# used for sgrep and other small utilities which I dont want to depend
# on too much things
BASICLIBS=commons/lib.cma \
 $(JSONCMA) \
 globals/lib.cma \
 h_files-format/lib.cma \
 h_program-lang/lib.cma \
 matcher/lib.cma \
 lang_ml/parsing/lib.cma \
 lang_nw/parsing/lib.cma \
 lang_php/parsing/lib.cma \
  lang_php/matcher/lib.cma \
  lang_php/pretty/lib.cma \
 lang_cpp/parsing/lib.cma \
 lang_c/parsing/lib.cma \
 lang_clang/parsing/lib.cma \
 lang_java/parsing/lib.cma \
 lang_python/parsing/lib.cma \
 lang_csharp/parsing/lib.cma \
 lang_rust/parsing/lib.cma \
 lang_opa/parsing/lib.cma \
 lang_erlang/parsing/lib.cma \
 lang_haskell/parsing/lib.cma \
 lang_lisp/parsing/lib.cma \
 lang_html/parsing/lib.cma \
 lang_js/parsing/lib.cma \
 lang_css/parsing/lib.cma \
 lang_web/parsing/lib.cma \
 lang_text/lib.cma \
 lang_sql/parsing/lib.cma \
 mini/lib.cma

BASICSYSLIBS=nums.cma bigarray.cma str.cma unix.cma

# use for the other programs
LIBS= commons/lib.cma \
       $(JSONCMA) \
       $(GRAPHCMA) \
       $(EXTLIBCMA) $(PTCMA) $(ZIPCMA) $(JAVALIBCMA) \
       commons/commons_features.cma \
    globals/lib.cma \
    h_files-format/lib.cma \
    h_version-control/lib.cma \
    h_program-lang/lib.cma \
    h_visualization/lib.cma \
    h_program-visual/lib.cma \
    graph_code/lib.cma \
    matcher/lib.cma \
    lang_ml/parsing/lib.cma \
     lang_ml/analyze/visual/lib.cma \
     lang_ml/analyze/lib.cma \
    $(CMTCMAS) \
    lang_nw/parsing/lib.cma \
     lang_nw/analyze/lib.cma \
    lang_lisp/parsing/lib.cma \
     lang_lisp/analyze/lib.cma \
    lang_haskell/parsing/lib.cma \
     lang_haskell/analyze/lib.cma \
    lang_php/parsing/lib.cma \
     lang_php/analyze/foundation/lib.cma \
     lang_php/analyze/tools/lib.cma \
     lang_php/analyze/checker/lib.cma \
     lang_php/matcher/lib.cma \
     lang_php/analyze/qa_test/lib.cma \
     lang_php/analyze/visual/lib.cma \
     lang_php/analyze/lib.cma \
     lang_php/pretty/lib.cma \
    lang_sql/parsing/lib.cma \
    lang_js/parsing/lib.cma \
     lang_js/analyze/lib.cma \
    lang_cpp/parsing/lib.cma \
     lang_cpp/analyze/lib.cma \
    lang_c/parsing/lib.cma \
     lang_c/analyze/lib.cma \
    lang_clang/parsing/lib.cma \
     lang_clang/analyze/lib.cma \
    lang_java/parsing/lib.cma \
     lang_java/analyze/lib.cma \
    $(BYTECODECMAS) \
    lang_python/parsing/lib.cma \
     lang_python/analyze/lib.cma \
    lang_csharp/parsing/lib.cma \
     lang_csharp/analyze/lib.cma \
    lang_rust/parsing/lib.cma \
     lang_rust/analyze/lib.cma \
    lang_opa/parsing/lib.cma \
     lang_opa/analyze/lib.cma \
    lang_erlang/parsing/lib.cma \
     lang_erlang/analyze/lib.cma \
    lang_text/lib.cma \
    lang_html/parsing/lib.cma \
     lang_html/analyze/lib.cma \
    lang_css/parsing/lib.cma \
    lang_web/parsing/lib.cma \
    mini/lib.cma

MAKESUBDIRS=commons \
  commons/graph \
  $(JSONDIR) \
  $(GRAPHDIR) \
  $(GUIDIR) $(CAIRODIR) \
  $(ZIPDIR) $(EXTLIBDIR) $(PTDIR) $(JAVALIBDIR) \
  globals \
  h_version-control \
  h_visualization \
  h_files-format \
  h_program-lang \
  graph_code \
  h_program-visual \
  matcher \
  lang_ml/parsing \
   lang_ml/analyze \
  $(CMTDIRS) \
  lang_nw/parsing \
   lang_nw/analyze \
  lang_lisp/parsing \
   lang_lisp/analyze \
  lang_haskell/parsing \
   lang_haskell/analyze \
  lang_php/parsing \
   lang_php/matcher \
   lang_php/pretty \
  lang_sql/parsing \
  lang_js/parsing \
   lang_js/analyze \
  lang_cpp/parsing \
   lang_cpp/analyze \
  lang_c/parsing \
   lang_c/analyze \
  lang_clang/parsing \
   lang_clang/analyze \
  lang_java/parsing \
   lang_java/analyze \
  $(BYTECODEDIRS) \
  lang_python/parsing \
   lang_python/analyze \
  lang_csharp/parsing \
   lang_csharp/analyze \
  lang_rust/parsing \
   lang_rust/analyze \
  lang_opa/parsing \
   lang_opa/analyze \
  lang_erlang/parsing \
   lang_erlang/analyze \
  lang_php/analyze \
   lang_php/analyze/foundation \
   lang_php/analyze/checker \
   lang_php/analyze/tools \
   lang_php/analyze/qa_test \
  lang_html/parsing \
   lang_html/analyze \
  lang_css/parsing \
  lang_web/parsing \
  lang_text \
  mini \
  $(VISUALDIRS) \
  demos

INCLUDEDIRS=$(MAKESUBDIRS) \
 commons/ocamlextra commons/ocollection \
 $(GTKINCLUDE) $(CAIROINCLUDE) \
 $(OCAMLCOMPILERDIR)

PP=-pp "cpp $(CLANG_HACK) -DFEATURE_BYTECODE=$(FEATURE_BYTECODE) -DFEATURE_CMT=$(FEATURE_CMT)"

##############################################################################
# Generic
##############################################################################
-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################

.PHONY:: all all.opt opt top clean distclean

#note: old: was before all: rec $(EXEC) ... but can not do that cos make -j20
#could try to compile $(EXEC) before rec. So here force sequentiality.

all:: Makefile.config
	$(MAKE) rec
	$(MAKE) $(PROGS)
opt:
	$(MAKE) rec.opt
	$(MAKE) $(OPTPROGS)
all.opt: opt
top: $(TARGET).top

rec:
	$(MAKE) -C commons
	$(GRAPHCMD)
	$(GUICMD)
	$(MAKE) features -C commons
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all || exit 1; done

rec.opt:
	$(MAKE) all.opt -C commons
	$(GRAPHCMDOPT)
	$(GUICMDOPT)
	$(MAKE) features.opt -C commons
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all.opt || exit 1; done


$(TARGET): $(BASICLIBS) $(OBJS) main.cmo
	$(OCAMLC) $(BYTECODE_STATIC) -o $@ $(SYSLIBS) $^

$(TARGET).opt: $(BASICLIBS:.cma=.cmxa) $(OPTOBJS) main.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa)  $^


$(TARGET).top: $(LIBS) $(OBJS)
	$(OCAMLMKTOP) -o $@ $(SYSLIBS) threads.cma $^




clean::
	rm -f $(TARGET)
clean::
	rm -f $(TARGET).top
clean::
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i clean; done
clean::
	rm -f *.opt

depend::
	set -e; for i in $(MAKESUBDIRS); do echo $$i; $(MAKE) -C $$i depend; done

Makefile.config:
	@echo "Makefile.config is missing. Have you run ./configure?"
	@exit 1


distclean:: clean
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i $@; done
	rm -f .depend
	rm -f Makefile.config
	rm -f globals/config_pfff.ml
	rm -f TAGS
#	find -name ".#*1.*" | xargs rm -f

# add -custom so dont need add e.g. ocamlbdb/ in LD_LIBRARY_PATH
CUSTOM=-custom

static:
	rm -f $(EXEC).opt $(EXEC)
	$(MAKE) STATIC="-ccopt -static" $(EXEC).opt
	cp $(EXEC).opt $(EXEC)

purebytecode:
	rm -f $(EXEC).opt $(EXEC)
	$(MAKE) BYTECODE_STATIC="" $(EXEC)

#------------------------------------------------------------------------------
# stags targets (was pfff_tags)
#------------------------------------------------------------------------------

stags: $(LIBS) $(OBJS) main_stags.cmo
	$(OCAMLC) $(CUSTOM) -o $@ $(SYSLIBS) $^
stags.opt: $(LIBS:.cma=.cmxa) $(OPTOBJS) main_stags.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa) $^
clean::
	rm -f stags

#------------------------------------------------------------------------------
# sgrep/spatch targets
#------------------------------------------------------------------------------

sgrep: $(BASICLIBS) $(OBJS) main_sgrep.cmo
	$(OCAMLC) $(CUSTOM) -o $@ $(BASICSYSLIBS) $^
sgrep.opt: $(BASICLIBS:.cma=.cmxa) $(OPTOBJS) main_sgrep.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(BASICSYSLIBS:.cma=.cmxa) $^
clean::
	rm -f sgrep

spatch: $(BASICLIBS) $(OBJS) main_spatch.cmo
	$(OCAMLC) $(CUSTOM) -o $@ $(BASICSYSLIBS) $^
spatch.opt: $(BASICLIBS:.cma=.cmxa) $(OPTOBJS) main_spatch.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(BASICSYSLIBS:.cma=.cmxa) $^
clean::
	rm -f spatch

#------------------------------------------------------------------------------
# scheck targets
#------------------------------------------------------------------------------

scheck: $(LIBS) $(OBJS) main_scheck.cmo
	$(OCAMLC) $(CUSTOM) -o $@ $(SYSLIBS) $^
scheck.opt: $(LIBS:.cma=.cmxa) $(OPTOBJS) main_scheck.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa) $^
clean::
	rm -f scheck

#------------------------------------------------------------------------------
# codequery targets
#------------------------------------------------------------------------------

codequery: $(LIBS) $(OBJS) main_codequery.cmo
	$(OCAMLC) $(CUSTOM) -o $@ $(SYSLIBS) $^
codequery.opt: $(LIBS:.cma=.cmxa) $(LIBS2:.cma=.cmxa) $(OBJS2:.cmo=.cmx) $(OPTOBJS) main_codequery.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa)   $^
clean::
	rm -f codequery

#------------------------------------------------------------------------------
# codeslicer targets
#------------------------------------------------------------------------------

codeslicer: $(LIBS) $(OBJS) main_codeslicer.cmo
	$(OCAMLC) $(CUSTOM) -o $@ $(SYSLIBS) $^
codeslicer.opt: $(LIBS:.cma=.cmxa) $(OPTOBJS) main_codeslicer.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa) $^
clean::
	rm -f codeslicer

#------------------------------------------------------------------------------
# pfff_db targets
#------------------------------------------------------------------------------

pfff_db: $(LIBS) $(OBJS) main_db.cmo
	$(OCAMLC) $(CUSTOM) -o $@ $(SYSLIBS) $^
pfff_db.opt: $(LIBS:.cma=.cmxa) $(LIBS2:.cma=.cmxa) $(OBJS2:.cmo=.cmx) $(OPTOBJS) main_db.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa)   $^
clean::
	rm -f pfff_db

#------------------------------------------------------------------------------
# codemap target (was pfff_visual)
#------------------------------------------------------------------------------
SYSLIBS_CM= \
 external/ocamlgtk/src/lablgtk.cma \
 external/ocamlcairo/src/cairo.cma \
 external/ocamlcairo/src/cairo_lablgtk.cma
OBJS_CM=code_map/lib.cma

GTKLOOP=gtkThread.cmo

codemap: $(LIBS) commons/commons_gui.cma $(OBJS_CM) $(OBJS) main_codemap.cmo
	$(OCAMLC) -thread $(CUSTOM) -o $@ $(SYSLIBS) threads.cma \
            $(SYSLIBS_CM) $(GTKLOOP) $^

codemap.opt: $(LIBS:.cma=.cmxa) commons/commons_gui.cmxa $(OBJS_CM:.cma=.cmxa) $(OPTOBJS) main_codemap.cmx
	$(OCAMLOPT) -thread $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa) threads.cmxa\
          $(SYSLIBS_CM:.cma=.cmxa) $(GTKLOOP:.cmo=.cmx)  $^

clean::
	rm -f codemap

#------------------------------------------------------------------------------
# codegraph (was pm_depend)
#------------------------------------------------------------------------------

SYSLIBS_CG=$(SYSLIBS_CM)
OBJS_CG=code_graph/lib.cma

codegraph: $(LIBS) commons/commons_gui.cma $(OBJS_CG) $(OBJS) main_codegraph.cmo
	$(OCAMLC) -thread $(CUSTOM) -o $@ $(SYSLIBS) threads.cma \
           $(SYSLIBS_CG) $(GTKLOOP) $^

codegraph.opt: $(LIBS:.cma=.cmxa) commons/commons_gui.cmxa $(OBJS_CG:.cma=.cmxa) $(OPTOBJS) main_codegraph.cmx
	$(OCAMLOPT) -thread $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa) threads.cmxa\
          $(SYSLIBS_CG:.cma=.cmxa) $(GTKLOOP:.cmo=.cmx)  $^

clean::
	rm -f codegraph

#------------------------------------------------------------------------------
# pfff_test targets
#------------------------------------------------------------------------------

pfff_test: $(LIBS) $(OBJS) main_test.cmo
	$(OCAMLC) $(CUSTOM) -o $@ $(SYSLIBS) $^
pfff_test.opt: $(LIBS:.cma=.cmxa) $(OPTOBJS) main_test.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa) $^
clean::
	rm -f pfff_test


##############################################################################
# Build documentation
##############################################################################
.PHONY:: docs

##############################################################################
# Install
##############################################################################

VERSION=$(shell cat globals/config_pfff.ml.in |grep version |perl -p -e 's/.*"(.*)".*/$$1/;')

# note: don't remove DESTDIR, it can be set by package build system like ebuild
install: all
	mkdir -p $(DESTDIR)$(BINDIR)
	mkdir -p $(DESTDIR)$(SHAREDIR)
	cp -a $(PROGS) $(DESTDIR)$(BINDIR)
	cp -a data $(DESTDIR)$(SHAREDIR)
	@echo ""
	@echo "You can also install pfff by copying the programs"
	@echo "available in this directory anywhere you want and"
	@echo "give it the right options to find its configuration files."

uninstall:
	rm -rf $(DESTDIR)$(SHAREDIR)/data


INSTALL_SUBDIRS= \
  commons \
  commons/graph \
  h_program-lang    matcher \
  h_version-control h_files-format h_visualization \
  lang_ml/parsing \
  lang_php/analyze lang_php/matcher lang_php/parsing  lang_php/pretty \
  lang_cpp/parsing \
  lang_java/parsing \
  lang_js/parsing lang_css/parsing lang_html/parsing \
  lang_nw/parsing lang_nw/analyze\
  external/jsonwheel \
  external/ocamlgraph \
  graph_code

LIBNAME=pfff
install-findlib:: all all.opt
	ocamlfind install $(LIBNAME) META
	set -e; for i in $(INSTALL_SUBDIRS); do echo $$i; $(MAKE) -C $$i install-findlib; done

uninstall-findlib::
	set -e; for i in $(INSTALL_SUBDIRS); do echo $$i; $(MAKE) -C $$i uninstall-findlib; done

version:
	@echo $(VERSION)


install-bin:
	cp $(PROGS) ../pfff-binaries/mac

##############################################################################
# Package rules
##############################################################################

PACKAGE=$(TARGET)-$(VERSION)
TMP=/tmp

package:
	make srctar

srctar:
	make clean
	cp -a .  $(TMP)/$(PACKAGE)
	cd $(TMP); tar cvfz $(PACKAGE).tgz  --exclude=CVS --exclude=_darcs  $(PACKAGE)
	rm -rf  $(TMP)/$(PACKAGE)

#todo? automatically build binaries for Linux, Windows, etc?
#http://stackoverflow.com/questions/2689813/cross-compile-windows-64-bit-exe-from-linux

# making an OPAM package:
# - git push from pfff to github
# - make a new release on github: https://github.com/facebook/pfff/releases
# - get md5sum of new archive
# - update opam file in opam-repository/pfff-xxx/
# - test locally?
# - commit, git push
# - do pull request on github

##############################################################################
# Website rules
##############################################################################

# see also ~/github/pfff-wiki/
WEBSITE=/home/pad/mobile/homepage/software/project-pfff

gen-html:
	emacs -l ~/.emacs --eval "(progn (htmlize-many-files '(\"changes.txt\")) (kill-emacs))"

website:
	cp $(TMP)/$(PACKAGE).tgz                $(WEBSITE)
#	make gen-html
#	cp changes.txt.html $(WEBSITE)/changes-$(VERSION).html

##############################################################################
# Developer rules
##############################################################################

.PHONY:: tags graph prolog  db layers visual   tests test

graph:
	./codegraph.opt -derived_data -lang cmt -build .
prolog:
	./codequery.opt -lang cmt -build .
	mv facts.pl facts_pl

# superseded by codegraph -derived_data above
tags:
	./stags.opt -lang cmt .
db:
	./pfff_db.opt -db_of_graph_code graph_code.marshall
layers:
	./codegraph.opt -gen_bottomup_layer graph_code.marshall layer_graph_code.json
#./pfff_db_heavy -gen_age_layer /home/pad/local/pfff-for-layers layer_age.marshall
#./pfff_db_heavy -gen_age_layer /home/pad/local/pfff-for-layers layer_age.json


visual:
	./codemap -no_legend -profile -screen_size 2 -filter pfff .
loc:
	./codemap -no_legend -profile -screen_size 2 -filter pfff -test_loc .

tests:
	$(MAKE) rec && $(MAKE) pfff_test
	./pfff_test -verbose all
test:
	make tests

push:
	git push origin master
pull:
	git pull
	cd facebook; git pull


##############################################################################
# Other/Old developer rules
##############################################################################

visual2:
	./codemap -no_legend -profile -ss 2 \
	   -with_info DB_LIGHT.marshall -with_layers . .
visualhead:
	./codemap -ss 1 -ft 0.5 -commitid HEAD

graph2:
	./codegraph.opt -lang ml -build .

#refactoring:
# git grep -l Source_high | xargs perl -p -i -e 's/Source_highlight/Highlight_code/g'

# TODO: replace all of that with a graphviz plugin for codegraph
DSRC=$(SRC)
DIRS= $(filter-out commons external/ocamlgtk/src external/ocamlcairo external/ocamlgraph facebook, $(MAKESUBDIRS))
#DIRS=lang_php/parsing
DSRC+=$(DIRS:=/*.ml)
DSRC+=$(wildcard main_*.ml)

#PP1=-pp camlp4o
DOTCOLORS=green,darkgoldenrod2,cyan,red,magenta,yellow,burlywood1,aquamarine,purple,lightpink,salmon,mediumturquoise,black,slategray3

archi:
	ocamldoc $(PP1) -I +threads $(INCLUDES) $(DSRC)  \
	  -dot -dot-reduce -dot-colors $(DOTCOLORS)
	dot -Tps ocamldoc.out > dot.ps
	mv dot.ps Fig_graph_ml.ps
	ps2pdf Fig_graph_ml.ps
	rm -f Fig_graph_ml.ps

##############################################################################
# Facebook specific rules
##############################################################################

fbpull:
	proxycmd.sh git pull
	cd facebook; git pull
fbpush:
	proxycmd.sh git push
	cd facebook; git push

fb:
	$(MAKE)
	$(MAKE) -C facebook
fb.opt:
	$(MAKE) opt
	$(MAKE) opt -C facebook

fbdepend:
	$(MAKE) depend
	$(MAKE) depend -C facebook

##############################################################################
# Pad specific rules
##############################################################################

DARCSFORESTS=commons \
 ocamltarzan \
 h_version-control h_program-lang \
 lang_php/parsing \
 lang_php/analyze \
 gui \
 facebook

update_darcs:
	darcs pull
	set -e; for i in $(DARCSFORESTS); do cd $$i; darcs pull; cd ..; done

diff_darcs:
	@echo "----- REPO:" top "----------------------"
	darcs diff -u
	set -e; for i in $(DARCSFORESTS); do cd $$i; echo "----- REPO:" $$i "-----------------"; darcs diff -u; cd $(TOP); done
