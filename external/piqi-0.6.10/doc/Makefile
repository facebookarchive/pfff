SRCS=$(filter-out %-patched.md,$(wildcard *.md))
HTML=$(patsubst %.md,%.html,$(SRCS))

.PHONY: all clean man html


ifneq ($(shell which pandoc), )
all: html

else
$(warning "WARNING: skipping building Piqi documentation, because couldn't find 'pandoc'")
all:

endif


test:
	./check-links

man: piqi.1

html: $(HTML) CHANGES.html

piqi.1: tools.md
	ts=`git log -1 --format=%ct $<`; \
	date=`date -d @$$ts '+%B %d, %Y'`; \
	(echo "% PIQI(1) Piqi User Manual"; \
	echo "% Anton Lavrik <alavrik@piqi.org>"; \
	echo "% $$date"; \
	echo) | cat - $< | pandoc -s -t man -o $@
	sed -i -e 's/-/\\-/g' $@

CHANGES.html: ../CHANGES
	pandoc -s $< -o $@

%-patched.md: %.md
	sed -e 's|](\(/doc/erlang\)| (external)](http://piqi.org\1|g' \
	        -e 's|](\(/doc/ocaml\)| (external)](http://piqi.org\1|g' \
		-e 's|](\(/doc/piqi-rpc\)| (external)](http://piqi.org\1|g' \
		-e 's|](\(/faq\)| (external)](http://piqi.org\1|g' \
		-e 's|](\(/examples\)| (external)](http://piqi.org\1|g' \
		-e 's|](\(/self-spec\)| (external)](http://piqi.org\1|g' \
		-e 's|](\(/rationale\)| (external)](http://piqi.org\1|g' \
		-e '# Local stuff 2: /doc(/?)*#stuff) points to *.html#stuff' \
		-e 's|(/doc/\([^/]*\)/\?#\([^)]*\))|(\1.html#\2)|g' \
		-e '# Local stuff: /doc/*/) points to *.html' \
		-e 's|(/doc/\([^/]*\)/\?)|(\1.html)|g' $< > $@
	if [ "$@" = "index-patched.md" ]; then \
		sed -i 's|https://github.com/alavrik/piqi/blob/master/CHANGES|CHANGES.html|' $@; \
	fi

%.html: %-patched.md
	pandoc -s $< -o $@

clean:
	rm -f $(HTML) CHANGES.html
