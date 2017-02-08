include ../Makefile.config
include $(PIQI_ROOT)/make/Makefile.dirs


# Are Protocol Buffers tools installed? Assuming here that the C++ .so libraries
# are installed as well.
ifneq ($(shell which protoc), )
DIRS_PROTO = \
       proto \
       proto1 \
       packed \
       extensions \
       cpp \

endif


DIRS = \
       examples \
       self_spec \
       misc \
       infinite_types \
       server \
       compile \
       pp \
       piq \
       $(DIRS_PROTO)


.PHONY: proto proto-clean


post_target::
	@echo "ALL TESTS OK"


proto:
	set -e; \
	for i in $(DIRS_PROTO); do \
		$(MAKE) -C $$i; \
	done


proto-clean:
	for i in $(DIRS_PROTO); do \
		$(MAKE) -C $$i clean; \
	done


clean::

