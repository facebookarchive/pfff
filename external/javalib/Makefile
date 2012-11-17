-include Makefile.config

.PHONY:all javalib install clean cleanall cleandoc doc
# ptrees camlzip extlib 

# should do : 
all:javalib

javalib:
	$(MAKE) -C src

# Package-specific targets
#camlzip installcamlzip removecamlzip:%camlzip:
#	$(MAKE) -C camlzip $*
#
#extlib installextlib removeextlib:%extlib:
#	$(MAKE) -C extlib $*
#
#ptrees installptrees removeptrees:%ptrees:
#	$(MAKE) -C ptrees $*

install remove:
	$(MAKE) -C src $@

distclean:clean
	$(RM) Makefile.config

cleanall clean:
	$(MAKE) -C src $@
	$(MAKE) -C doc $@
	$(RM) *~

#	$(MAKE) -C ptrees $@
#	$(MAKE) -C camlzip $@
#	$(MAKE) -C extlib $@

cleandoc doc:
	$(MAKE) -C src $@

# Documentation for release (INSTALL and README)
cleandocr docr:
	$(MAKE) -C doc $@
