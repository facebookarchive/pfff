# Toplevel makefile for LablGtk2

all opt doc install byte world: config.make
all opt doc install byte clean depend world:
	$(MAKE) -C src $@

arch-clean:
	@rm -f config.status config.make config.cache config.log
	@rm -f \#*\# *~ aclocal.m4
	@rm -rf autom4te*.cache

configure: configure.in
	aclocal
	autoconf

config.make: config.make.in
	@echo config.make is not up to date. Execute ./configure first.
	@exit 2

.PHONY: all opt doc install byte world clean depend arch-clean headers

headers:
	find examples -name "*.ml" -exec headache -h header_examples {} \;
	find applications -name "*.ml" -exec headache -h header_apps {} \;
	find applications -name "*.mli" -exec headache -h header_apps {} \;
	find src -name "*.ml" -exec headache -h header {} \;
	find src -name "*.mli" -exec headache -h header {} \;
	find src -name "*.c" -exec headache -h header {} \;
	find src -name "*.h" -exec headache -h header {} \;