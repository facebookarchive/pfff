include ../../Makefile.config


all:
	$(PIQI) pp t.piq | cmp - t.piq.pp
	$(PIQI) pp --expand-abbr t.piq | cmp - t.piq.pp-expand-abbr
	$(PIQI) pp --expand-abbr --parse-literals t.piq | cmp - t.piq.pp-expand-abbr-parse-literals


clean:

