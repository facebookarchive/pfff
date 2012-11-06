-include Makefile.config

.PHONY:all ptrees camlzip extlib javalib install clean cleanall cleandoc doc

# should do : 
all:javalib

javalib:
	$(MAKE) -C src

# Package-specific targets
camlzip installcamlzip removecamlzip:%camlzip:
	$(MAKE) -C camlzip $*

extlib installextlib removeextlib:%extlib:
	$(MAKE) -C extlib $*

ptrees installptrees removeptrees:%ptrees:
	$(MAKE) -C ptrees $*

install remove:
	$(MAKE) -C src $@

distclean:clean
	$(RM) Makefile.config

cleanall clean:
	$(MAKE) -C src $@
	$(MAKE) -C ptrees $@
	$(MAKE) -C camlzip $@
	$(MAKE) -C extlib $@
	$(MAKE) -C doc $@
	$(RM) *~

cleandoc doc:
	$(MAKE) -C src $@

# Documentation for release (INSTALL and README)
cleandocr docr:
	$(MAKE) -C doc $@
