#############################################################################
# Configuration section
#############################################################################

-include Makefile.config

##############################################################################
# Variables
##############################################################################
TOP=$(shell pwd)

SRC=main.ml 

TARGET=pfff

#------------------------------------------------------------------------------
# Program related variables
#------------------------------------------------------------------------------

PROGS=pfff \
 sgrep spatch \
 stags \
 scheck \
 pfff_db \
 codequery \
 pfff_test

ifeq ($(FEATURE_VISUAL), 1)
PROGS+=codemap
PROGS+=codegraph
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


# cf also below the target for pfff_browser
ifeq ($(FEATURE_GUI),1)
GUIDIR=external/ocamlgtk
GUICMD= $(MAKE) all -C $(GUIDIR) && $(MAKE) gui       -C commons 
GUICMDOPT= $(MAKE) opt -C $(GUIDIR) && $(MAKE) gui.opt       -C commons;
GTKINCLUDE=external/ocamlgtk/src
endif

# cf also below for target pfff_visual
ifeq ($(FEATURE_VISUAL),1)
CAIRODIR=external/ocamlcairo
CAIROINCLUDE=external/ocamlcairo/src
endif


#todo: remove?
ifeq ($(FEATURE_BACKTRACE), 1)
BTCMD= $(MAKE) backtrace -C commons
BTCMDOPT= $(MAKE) backtrace.opt -C commons
BTCMA=commons/commons_backtrace.cma
else
endif

#------------------------------------------------------------------------------

# should be FEATURE_OCAMLGRAPH, or should give dependencies between features
GRAPHCMA=external/ocamlgraph/ocamlgraph.cma commons/commons_graph.cma
GRAPHDIR=external/ocamlgraph
GRAPHCMD= $(MAKE) all -C $(GRAPHDIR) && $(MAKE) graph -C commons
GRAPHCMDOPT= $(MAKE) all.opt -C $(GRAPHDIR) && $(MAKE) graph.opt -C commons

ZIPDIR=external/ocamlzip
ZIPCMA=external/ocamlzip/zip.cma

EXTLIBDIR=external/extlib
EXTLIBCMA=external/extlib/extLib.cma

PTDIR=external/ptrees
PTCMA=external/ptrees/ptrees.cma

JAVALIBDIR=external/javalib/src
JAVALIBCMA=external/javalib/src/lib.cma

ifeq ($(FEATURE_GRAPHICS), 1)
#GRAPHICSCMXA=graphics.cmxa
endif

ifeq ($(FEATURE_VISUAL),1)
VISUALDIRS=code_map code_graph
endif

OCAMLCOMPILERDIR=$(shell ocamlc -where)/compiler-libs
OCAMLCOMPILERCMA=ocamlcommon.cma

#------------------------------------------------------------------------------
# Main variables
#------------------------------------------------------------------------------
SYSLIBS=nums.cma bigarray.cma str.cma unix.cma

SYSLIBS+=$(OCAMLCOMPILERCMA)


# used for sgrep and other small utilities which I dont want to depend
# on too much things
BASICLIBS=commons/lib.cma \
  $(GRAPHCMA) \
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
 lang_objc/parsing/lib.cma \
 lang_clang/parsing/lib.cma \
 lang_java/parsing/lib.cma \
 lang_python/parsing/lib.cma \
 lang_csharp/parsing/lib.cma \
 lang_opa/parsing/lib.cma \
 lang_erlang/parsing/lib.cma \
 lang_haskell/parsing/lib.cma \
 lang_lisp/parsing/lib.cma \
 lang_html/parsing/lib.cma \
 lang_js/parsing/lib.cma \
 lang_css/parsing/lib.cma \
 lang_web/parsing/lib.cma \
 lang_text/lib.cma \

BASICSYSLIBS=nums.cma bigarray.cma str.cma unix.cma

LIBS= commons/lib.cma \
       $(BTCMA) \
       $(GRAPHCMA) \
       $(EXTLIBCMA) $(PTCMA) $(ZIPCMA) \
       $(JAVALIBCMA) \
       commons/commons_features.cma \
    globals/lib.cma \
    h_version-control/lib.cma \
    h_visualization/lib.cma \
    h_files-format/lib.cma \
    h_program-lang/lib.cma \
    h_program-visual/lib.cma \
    matcher/lib.cma \
    lang_ml/parsing/lib.cma \
     lang_ml/analyze/visual/lib.cma \
     lang_ml/analyze/lib.cma \
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
    lang_objc/parsing/lib.cma \
     lang_objc/analyze/lib.cma \
    lang_clang/parsing/lib.cma \
     lang_clang/analyze/lib.cma \
    lang_java/parsing/lib.cma \
     lang_java/analyze/lib.cma \
    lang_bytecode/parsing/lib.cma \
     lang_bytecode/analyze/lib.cma \
    lang_python/parsing/lib.cma \
     lang_python/analyze/lib.cma \
    lang_csharp/parsing/lib.cma \
     lang_csharp/analyze/lib.cma \
    lang_opa/parsing/lib.cma \
     lang_opa/analyze/lib.cma \
    lang_erlang/parsing/lib.cma \
     lang_erlang/analyze/lib.cma \
    lang_text/lib.cma \
    lang_html/parsing/lib.cma \
     lang_html/analyze/lib.cma \
    lang_css/parsing/lib.cma \
    lang_web/parsing/lib.cma \

MAKESUBDIRS=commons \
  $(GRAPHDIR) \
  $(GUIDIR) $(CAIRODIR) \
  $(ZIPDIR)    $(EXTLIBDIR) $(PTDIR) $(JAVALIBDIR) \
  globals \
  h_version-control \
  h_visualization \
  h_files-format \
  h_program-lang \
  h_program-visual \
  matcher \
  lang_ml/parsing \
   lang_ml/analyze \
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
  lang_objc/parsing \
   lang_objc/analyze \
  lang_clang/parsing \
   lang_clang/analyze \
  lang_java/parsing \
   lang_java/analyze \
  lang_bytecode/parsing \
   lang_bytecode/analyze \
  lang_python/parsing \
   lang_python/analyze \
  lang_csharp/parsing \
   lang_csharp/analyze \
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
  $(VISUALDIRS) \
  demos

INCLUDEDIRS=$(MAKESUBDIRS) \
 commons/ocamlextra commons/ocollection \
 commons/lib-json commons/lib-xml commons/lib-sexp \
 $(GTKINCLUDE) $(CAIROINCLUDE) \
 $(EXTLIBDIR) $(PTDIR) $(ZIPDIR) $(JAVALIBDIR) \
 $(OCAMLCOMPILERDIR)

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
	$(BTCMD)
	$(GRAPHCMD)
	$(GUICMD)
	$(MAKE) features -C commons 
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all || exit 1; done 

rec.opt:
	$(MAKE) all.opt -C commons 
	$(BTCMDOPT)
	$(GRAPHCMDOPT)
	$(GUICMDOPT)
	$(MAKE) features.opt -C commons 
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all.opt || exit 1; done 


$(TARGET): $(BASICLIBS) $(OBJS)
	$(OCAMLC) $(BYTECODE_STATIC) -o $@ $(SYSLIBS) $^

$(TARGET).opt: $(BASICLIBS:.cma=.cmxa) $(OPTOBJS) 
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

stags: $(LIBS) main_stags.cmo 
	$(OCAMLC) $(CUSTOM) -o $@ $(SYSLIBS) $^
stags.opt: $(LIBS:.cma=.cmxa) main_stags.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa) $^
clean::
	rm -f stags

#------------------------------------------------------------------------------
# sgrep/spatch targets
#------------------------------------------------------------------------------

sgrep: $(BASICLIBS) main_sgrep.cmo 
	$(OCAMLC) $(CUSTOM) -o $@ $(BASICSYSLIBS) $^
sgrep.opt: $(BASICLIBS:.cma=.cmxa) main_sgrep.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(BASICSYSLIBS:.cma=.cmxa) $^

clean::
	rm -f sgrep

spatch: $(BASICLIBS) main_spatch.cmo 
	$(OCAMLC) $(CUSTOM) -o $@ $(BASICSYSLIBS) $^
spatch.opt: $(BASICLIBS:.cma=.cmxa) main_spatch.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(BASICSYSLIBS:.cma=.cmxa) $^
clean::
	rm -f spatch

#------------------------------------------------------------------------------
# scheck targets
#------------------------------------------------------------------------------

scheck: $(LIBS) main_scheck.cmo 
	$(OCAMLC) $(CUSTOM) -o $@ $(SYSLIBS) $^
scheck.opt: $(LIBS:.cma=.cmxa) main_scheck.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa) $^
clean::
	rm -f scheck

#------------------------------------------------------------------------------
# codequery targets
#------------------------------------------------------------------------------

codequery: $(LIBS) main_codequery.cmo 
	$(OCAMLC) $(CUSTOM) -o $@ $(SYSLIBS) $^
codequery.opt: $(LIBS:.cma=.cmxa) $(LIBS2:.cma=.cmxa) $(OBJS2:.cmo=.cmx) main_codequery.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa)   $^ 
clean:: 
	rm -f codequery

#------------------------------------------------------------------------------
# pfff_db targets
#------------------------------------------------------------------------------

pfff_db: $(LIBS) main_db.cmo 
	$(OCAMLC) $(CUSTOM) -o $@ $(SYSLIBS) $^
pfff_db.opt: $(LIBS:.cma=.cmxa) $(LIBS2:.cma=.cmxa) $(OBJS2:.cmo=.cmx) main_db.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa)   $^ 
clean:: 
	rm -f pfff_db

#------------------------------------------------------------------------------
# codemap target (was pfff_visual)
#------------------------------------------------------------------------------
SYSLIBS3= \
 external/ocamlgtk/src/lablgtk.cma \
 external/ocamlcairo/src/cairo.cma \
 external/ocamlcairo/src/cairo_lablgtk.cma \

OBJS3=code_map/lib.cma

GTKLOOP=gtkThread.cmo

codemap: $(LIBS) commons/commons_gui.cma $(OBJS3) main_codemap.cmo
	$(OCAMLC) -thread $(CUSTOM) -o $@ $(SYSLIBS) threads.cma  $(SYSLIBS3) $(GTKLOOP) $^

codemap.opt: $(LIBS:.cma=.cmxa) commons/commons_gui.cmxa $(OBJS3:.cma=.cmxa) main_codemap.cmx
	$(OCAMLOPT) -thread $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa) threads.cmxa  $(SYSLIBS3:.cma=.cmxa) $(GTKLOOP:.cmo=.cmx)  $^

clean::
	rm -f codemap

#------------------------------------------------------------------------------
# codegraph (was pm_depend)
#------------------------------------------------------------------------------

#SYSLIBS_PM= external/phylomel/src/lib.cma
OBJS4=code_graph/lib.cma

codegraph: $(LIBS) commons/commons_gui.cma $(OBJS4) main_codegraph.cmo
	$(OCAMLC) -thread $(CUSTOM) -o $@ $(SYSLIBS) threads.cma  $(SYSLIBS3) $(GTKLOOP) $^

codegraph.opt: $(LIBS:.cma=.cmxa) commons/commons_gui.cmxa $(OBJS4:.cma=.cmxa) main_codegraph.cmx
	$(OCAMLOPT) -thread $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa) threads.cmxa  $(SYSLIBS3:.cma=.cmxa) $(GTKLOOP:.cmo=.cmx)  $^

clean::
	rm -f codegraph

#------------------------------------------------------------------------------
# pfff_test targets
#------------------------------------------------------------------------------

pfff_test: $(LIBS) main_test.cmo 
	$(OCAMLC) $(CUSTOM) -o $@ $(SYSLIBS) $^
pfff_test.opt: $(LIBS:.cma=.cmxa) main_test.cmx
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
	cp -a $(PROGS) $(BINDIR)
	cp -a data $(DESTDIR)$(SHAREDIR)
	@echo ""
	@echo "You can also install pfff by copying the programs"
	@echo "available in this directory anywhere you want and"
	@echo "give it the right options to find its configuration files."

uninstall:
	rm -rf $(DESTDIR)$(SHAREDIR)/data

version:
	@echo $(VERSION)

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

##############################################################################
# Website rules
##############################################################################

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

tags:
	./stags.opt -lang cmt .
graph:
	./codegraph.opt -lang cmt -build .
prolog:
	./codequery.opt -lang cmt -build .
	mv facts.pl facts_pl
db:
	./pfff_db.opt -db_of_graph_code graph_code.marshall
layers:
	./codegraph.opt -gen_bottomup_layer graph_code.marshall layer_graph_code.json
#./pfff_db_heavy -gen_age_layer /home/pad/local/pfff-for-layers layer_age.marshall
#./pfff_db_heavy -gen_age_layer /home/pad/local/pfff-for-layers layer_age.json

visual:
	./codemap -no_legend -profile -ss 2 -filter pfff .

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




visual2:
	./codemap -no_legend -profile -ss 2 \
	   -with_info DB_LIGHT.marshall -with_layers . .
visualhead:
	./codemap -ss 1 -ft 0.5 -commitid HEAD

graph2:
	./codegraph.opt -lang ml -build .


#refactoring:
# git grep -l Source_high | xargs perl -p -i -e 's/Source_highlight/Highlight_code/g'

# TODO: replace with graphviz plugin to codegraph

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
# Pad specific rules
##############################################################################

DARCSFORESTS=commons commons/lib-sexp \
 ocamltarzan ocamltarzan/lib-sexp \
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
