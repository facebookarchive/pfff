include ../../Makefile.config


test:
	$(PIQI) convert --piq-relaxed-parsing true 1.piq | cmp - 1.piq.converted

	$(PIQI) convert --piq-relaxed-parsing true -t pib 1.piq
	$(PIQI) convert 1.piq.pib | cmp - 1.piq.converted

	$(PIQI) convert --piq-relaxed-parsing true -t json 1.piq
	$(PIQI) convert 1.piq.json | cmp - 1.piq.json.converted

	$(PIQI) convert --piq-relaxed-parsing true string_relaxed_parsing.piq | cmp - string_relaxed_parsing.piq.converted


clean:
	rm -f *.pib *.json

