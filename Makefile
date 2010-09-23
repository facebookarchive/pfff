#############################################################################
# Configuration section
#############################################################################

-include Makefile.config

##############################################################################
# Variables
##############################################################################
TOP=$(shell pwd)

SRC=test.ml main.ml 

TARGET=pfff

#------------------------------------------------------------------------------
# Program related variables
#------------------------------------------------------------------------------

PROGS=pfff

PROGS+=pfff_tags
PROGS+=sgrep
PROGS+=spatch
PROGS+=ppp

ifeq ($(FEATURE_BDB),1)
PROGS+=pfff_db
PROGS+=pfff_db_light
PROGS+=scheck
endif

ifeq ($(FEATURE_VISUAL), 1)
PROGS+=pfff_visual
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


# cf also below for target pfff_db
ifeq ($(FEATURE_BDB), 1)
BDBDIR=external/ocamlbdb
BDBCMD= $(MAKE) all -C $(BDBDIR) && $(MAKE) bdb -C commons
BDBCMDOPT= $(MAKE) all.opt -C $(BDBDIR) && $(MAKE) bdb.opt -C commons
BDBCMA=external/ocamlbdb/bdb.cma commons/commons_bdb.cma
BDBSYSCMA=
else
endif

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
CAIROCMD= $(MAKE) -C $(CAIRODIR) 
CAIROCMDOPT= $(MAKE) -C $(CAIRODIR) 
CAIROINCLUDE=external/ocamlcairo/src
endif

ifeq ($(FEATURE_BACKTRACE), 1)
BTCMD= $(MAKE) backtrace -C commons
BTCMDOPT= $(MAKE) backtrace.opt -C commons
BTCMA=commons/commons_backtrace.cma
else
endif

ifeq ($(FEATURE_PCRE), 1)
REGEXPDIR=external/ocamlpcre
REGEXPCMD= $(MAKE) -C $(REGEXPDIR) &&  $(MAKE) regexp -C commons
REGEXPCMDOPT= $(MAKE) -C $(REGEXPDIR) &&  $(MAKE) regexp.opt -C commons
REGEXPCMA=external/ocamlpcre/lib/pcre.cma  commons/commons_regexp.cma
PCREINCLUDE=external/ocamlpcre/lib
else
endif

ifeq ($(FEATURE_MPI),1)
MPIDIR=external/ocamlmpi
MPICMD=    $(MAKE) all -C $(MPIDIR) && $(MAKE) distribution -C commons
MPICMDOPT= $(MAKE) all.opt -C $(MPIDIR) && $(MAKE) distribution.opt -C commons
MPICMA=external/ocamlmpi/mpi.cma commons/commons_mpi.cma
endif

#------------------------------------------------------------------------------

# should be FEATURE_OCAMLGRAPH, or should give dependencies between features
ifeq ($(FEATURE_BDB), 1)
GRAPHCMA=external/ocamlgraph/ocamlgraph.cma commons/commons_graph.cma
GRAPHDIR=external/ocamlgraph
GRAPHCMD= $(MAKE) all -C $(GRAPHDIR) && $(MAKE) graph -C commons
GRAPHCMDOPT= $(MAKE) all.opt -C $(GRAPHDIR) && $(MAKE) graph.opt -C commons
else
endif

ifeq ($(FEATURE_GRAPHICS), 1)
#GRAPHICSCMXA=graphics.cmxa
endif

ifeq ($(FEATURE_GUI),1)
ANALYZEPHPCMA_EXTRA+=lang_php/analyze/finder/lib.cma
else
endif

# pfff_misc run the tests and so requires almost all libs
ifeq ($(FEATURE_BDB),1)
ANALYZEPHPCMA_EXTRA+= \
 lang_php/analyze/static_analysis/lib.cma \
 lang_php/analyze/qa_test/lib.cma \

else
endif


#make analyze_php toggable
ifeq ($(FEATURE_BDB), 1)
ANALYZEPHPCMA= \
 lang_php/analyze/database/lib.cma \
 $(ANALYZEPHPCMA_EXTRA) \
 lang_php/analyze/analyze_php.cma \

ANALYZEPHPDIR=\
 lang_php/analyze/ \
 lang_php/analyze/database \
 lang_php/analyze/static_analysis \

else
endif

ifeq ($(FEATURE_VISUAL),1)
VISUALDIR=visual
endif

#------------------------------------------------------------------------------
# Main variables
#------------------------------------------------------------------------------
SYSLIBS=nums.cma bigarray.cma str.cma unix.cma

# used for sgrep and other small utilities which I dont want to depend
# on bdb, even when I did a configure -bdb
BASICLIBS=commons/commons.cma \
 globals/globals.cma \
 h_program-lang/lib.cma \
 lang_ml/parsing/lib.cma \
 lang_php/parsing/lib.cma \
 lang_js/parsing/lib.cma \
 lang_cpp/parsing/lib.cma \
 lang_php/matcher/lib.cma \

BASICSYSLIBS=nums.cma bigarray.cma str.cma unix.cma

LIBS= commons/commons.cma \
       $(BTCMA) \
       $(BDBCMA) \
       $(REGEXPCMA) \
       $(MPICMA) \
       $(GRAPHCMA) \
       commons/commons_features.cma \
    h_version-control/lib.cma \
    h_visualization/lib.cma \
    h_program-lang/lib.cma \
    h_program-visual/lib.cma \
    globals/globals.cma \
    lang_ml/parsing/lib.cma \
     lang_ml/analyze/lib.cma \
    lang_php/parsing/lib.cma \
     lang_php/analyze/basic/lib.cma \
     lang_php/analyze/foundation/lib.cma \
     lang_php/analyze/typing/lib.cma \
     lang_php/analyze/tools/lib.cma \
     lang_php/analyze/annotaters/lib.cma \
     lang_php/analyze/checker/lib.cma \
     lang_php/mini/lib.cma \
     lang_php/matcher/lib.cma \
     $(ANALYZEPHPCMA) \
    lang_sql/parsing/lib.cma \
    lang_js/parsing/lib.cma \
     lang_js/analyze/lib.cma \
    lang_cpp/parsing/lib.cma \
     lang_cpp/analyze/lib.cma \

MAKESUBDIRS=commons \
  $(BDBDIR) $(REGEXPDIR) $(MPIDIR) \
  $(GRAPHDIR) \
  $(GUIDIR) $(CAIRODIR) \
  h_version-control \
  h_visualization \
  h_program-lang \
  h_program-visual \
  globals \
  lang_ml/parsing \
   lang_ml/analyze \
  lang_php/parsing \
   lang_php/mini \
   lang_php/matcher \
  lang_sql/parsing \
  lang_js/parsing \
   lang_js/analyze \
  lang_cpp/parsing \
   lang_cpp/analyze \
  lang_php/analyze \
   lang_php/analyze/basic \
   lang_php/analyze/foundation \
   lang_php/analyze/checker \
   $(ANALYZEPHPDIR) \
  $(VISUALDIR) \

INCLUDEDIRS=$(MAKESUBDIRS) \
 commons/ocamlextra commons/lib-json commons/lib-xml \
 $(GTKINCLUDE) $(CAIROINCLUDE) $(PCREINCLUDE)

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
	$(BDBCMD)
	$(REGEXPCMD)
	$(MPICMD)
	$(GRAPHCMD)
	$(GUICMD)
	$(CAIROCMD)
	$(MAKE) features -C commons 
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all || exit 1; done 

rec.opt:
	$(MAKE) all.opt -C commons 
	$(BTCMDOPT)
	$(BDBCMDOPT)
	$(REGEXPCMDOPT)
	$(MPICMDOPT)
	$(GRAPHCMDOPT)
	$(GUICMDOPT)
	$(CAIROCMDOPT)
	$(MAKE) features.opt -C commons 
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all.opt || exit 1; done 


$(TARGET): $(LIBS) $(OBJS)
	$(OCAMLC) $(BYTECODE_STATIC) -o $@ $(SYSLIBS) $^

$(TARGET).opt: $(LIBS:.cma=.cmxa) $(OPTOBJS) 
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
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i depend; done

Makefile.config:    
	@echo "Makefile.config is missing. Have you run ./configure?"
	@exit 1


distclean:: clean
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i $@; done
	rm -f .depend
	rm -f Makefile.config
	rm -f globals/config.ml
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
# pfff_tags targets
#------------------------------------------------------------------------------

pfff_tags: $(LIBS) main_tags.cmo 
	$(OCAMLC) $(CUSTOM) -o $@ $(SYSLIBS) $^

pfff_tags.opt: $(LIBS:.cma=.cmxa) main_tags.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(BASICSYSLIBS:.cma=.cmxa) $^

clean::
	rm -f pfff_tags

#------------------------------------------------------------------------------
# sgrep targets
#------------------------------------------------------------------------------

sgrep: $(LIBS) main_sgrep.cmo 
	$(OCAMLC) $(CUSTOM) -o $@ $(SYSLIBS) $^

sgrep.opt: $(BASICLIBS:.cma=.cmxa) main_sgrep.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(BASICSYSLIBS:.cma=.cmxa) $^

clean::
	rm -f sgrep

#------------------------------------------------------------------------------
# spatch targets
#------------------------------------------------------------------------------

spatch: $(LIBS) main_spatch.cmo 
	$(OCAMLC) $(CUSTOM) -o $@ $(SYSLIBS) $^

spatch.opt: $(LIBS:.cma=.cmxa) main_spatch.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa) $^

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
# ppp targets
#------------------------------------------------------------------------------

ppp: $(LIBS) main_ppp.cmo 
	$(OCAMLC) $(CUSTOM) -o $@ $(SYSLIBS) $^

ppp.opt: $(LIBS:.cma=.cmxa) main_ppp.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa) $^

clean::
	rm -f ppp

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
# pfff_db_light targets
#------------------------------------------------------------------------------

pfff_db_light: $(LIBS) main_db_light.cmo 
	$(OCAMLC) $(CUSTOM) -o $@ $(SYSLIBS) $^

pfff_db_light.opt: $(LIBS:.cma=.cmxa) $(LIBS2:.cma=.cmxa) $(OBJS2:.cmo=.cmx) main_db_light.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa)   $^ 

clean:: 
	rm -f pfff_db_light

#------------------------------------------------------------------------------
# pfff_browser target
#------------------------------------------------------------------------------
SYSLIBS2=external/ocamlgtk/src/lablgtk.cma 
LIBS2=commons/commons_gui.cma gui/gui.cma
OBJS2=

#need linker to find dlllabltk2.so so need adjust LD_LIBRARY_PATH if
# use ocamlgtk/src instead of the standard -I +lablgtk2
# cf env.sh

pfff_browser: $(LIBS) $(LIBS2) $(OBJS2) main_gui.cmo 
	$(OCAMLC) $(CUSTOM) -o $@ $(SYSLIBS)  $(SYSLIBS2)  $^

pfff_browser.opt: $(LIBS:.cma=.cmxa) $(LIBS2:.cma=.cmxa) $(OBJS2:.cmo=.cmx) main_gui.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa)  $(SYSLIBS2:.cma=.cmxa)   $^ 

clean::
	rm -f pfff_browser

#------------------------------------------------------------------------------
# pfff_visual target
#------------------------------------------------------------------------------
SYSLIBS3= \
 external/ocamlgtk/src/lablgtk.cma \
 external/ocamlcairo/src/cairo.cma \
 external/ocamlcairo/src/cairo_lablgtk.cma \

OBJS3=visual/lib.cma

GTKLOOP=gtkThread.cmo gtkInit.cmo

pfff_visual: $(LIBS) commons/commons_gui.cma $(OBJS3) main_visual.cmo
	$(OCAMLC) -thread $(CUSTOM) -o $@ $(SYSLIBS) threads.cma  $(SYSLIBS3) $(GTKLOOP) $^

pfff_visual.opt: $(LIBS:.cma=.cmxa) commons/commons_gui.cmxa $(OBJS3:.cma=.cmxa) main_visual.cmx
	$(OCAMLOPT) -thread $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa) threads.cmxa  $(SYSLIBS3:.cma=.cmxa) $(GTKLOOP:.cmo=.cmx)  $^

clean::
	rm -f pfff_visual


#------------------------------------------------------------------------------
# pfff_misc targets
#------------------------------------------------------------------------------
pfff_misc: $(LIBS) main_misc.cmo 
	$(OCAMLC) $(CUSTOM) -o $@ $(SYSLIBS) $(SYSLIBS4) $^

pfff_misc.opt: $(LIBS:.cma=.cmxa) main_misc.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa)  $(SYSLIBS4:.cma=.cmxa)   $^ 

clean:: 
	rm -f pfff_misc


##############################################################################
# Build documentation
##############################################################################
.PHONY:: docs

##############################################################################
# Install
##############################################################################

VERSION=$(shell cat globals/config.ml.in |grep version |perl -p -e 's/.*"(.*)".*/$$1/;')

# note: don't remove DESTDIR, it can be set by package build system like ebuild
install: all
	mkdir -p $(DESTDIR)$(SHAREDIR)
	cp -a config/ $(DESTDIR)$(SHAREDIR)
	@echo ""
	@echo "You can also install XXX by copying the program XXX"
	@echo "(available in this directory) anywhere you want and"
	@echo "give it the right options to find its configuration files."

uninstall:
	rm -rf $(DESTDIR)$(SHAREDIR)/config/

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

.PHONY:: tags visual db

tags:
	./pfff_tags -verbose -lang ml .
db:
	./pfff_db_light -verbose  -lang ml -o DB_LIGHT .

visual:
	./pfff_visual -profile -ss 2 \
	   -with_info DB_LIGHT  .
visualopt:
	./pfff_visual.opt -profile -ss 2 \
	   -with_info DB_LIGHT .


visual_test: pfff_visual
	./pfff_visual -verbose -profile -ss 1 -ft 1. \
          -with_info DB_LIGHT -filter 'pad:ml' commons/

visualhead:
	./pfff_visual -ss 1 -ft 0.5 -commitid HEAD

#VCS related
#test related
#refactoring:
# git grep -l Source_high | xargs perl -p -i -e 's/Source_highlight/Highlight_code/g'

push:
	git push origin master


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
