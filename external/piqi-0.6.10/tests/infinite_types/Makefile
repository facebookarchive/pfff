include ../../Makefile.config


INFINITE = \
	infinite_alias.piqi \
	infinite_alias2.piqi \
	infinite_list.piqi \
	infinite_list2.piqi \
	infinite_record.piqi \
	infinite_record2.piqi \
	infinite_variant.piqi \
	infinite_variant2.piqi \


FINITE = \
	finite_nested.piqi \


test:
	$(PIQI) check finite.piqi
	for i in $(INFINITE); do \
		$(PIQI) check $$i; \
		test $$? -eq 0 && exit 1; \
	done; true
	for i in $(FINITE); do \
		$(PIQI) check $$i; \
		test $$? -ne 0 && exit 1; \
	done; true


clean:

