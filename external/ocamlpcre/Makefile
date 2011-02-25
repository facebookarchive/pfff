-include Makefile.conf

EXAMPLES = $(filter-out examples/OMakefile examples/CVS, $(wildcard examples/*))

.PHONY: all
all:
	@cd lib && $(MAKE) byte-code-library native-code-library

.PHONY:	examples
examples:
	@for dir in $(EXAMPLES); do (cd $$dir && $(MAKE)); done

.PHONY:	doc
doc:
	@cd lib && $(MAKE) $@
	ln -sf lib/doc

.PHONY: htdoc
htdoc:
	@cd lib && $(MAKE) $@

.PHONY:	install
install:
	@cd lib && $(MAKE) $@

.PHONY:	uninstall
uninstall:
	@cd lib && $(MAKE) $@

.PHONY:	clean
clean:
	@cd lib && $(MAKE) clean
	@for dir in $(EXAMPLES); do (cd $$dir && $(MAKE) $@); done
	@rm -f doc

#pad:
depend::

all.opt:

distclean::
