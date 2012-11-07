# This is the tutorial Makefile
# You need to install pandoc

# Just for test on tutorial code
-include ../../Makefile.config

all: javalib_tut.pdf javalib-tut.html

release: javalib_tut.pdf javalib-tut-release.html

.DELETE_ON_ERROR: javalib-tut.html

javalib-tut.html: javalib_tutorial.md header.html
	pandoc -s --base-header-level 2 -c http://javalib.gforge.inria.fr/style.css \
	-B header.html -A footer.html --toc -T 'Javalib tutorial' -f \
	markdown -t html javalib_tutorial.md | tidy -q > javalib-tut.html

javalib-tut-release.html: javalib_tutorial.md header.html
	cp ../../../../site/style.css .
	pandoc -s --base-header-level 2 -c style.css -B header.html -A footer.html \
	--toc -T 'Javalib tutorial' -f \
		markdown -t html javalib_tutorial.md | tidy -utf8 -q > javalib-tut.html

javalib_tut.pdf: 
	pandoc -T 'Javalib tutorial' --toc header.md javalib_tutorial.md -o javalib-tut.pdf

test: tutorial.ml
	$(OCAMLC) -package javalib -c $^
	@echo "tutorial.ml file is compiling with version of Javalib installed."
	@echo "please check javalib_tutorial.md correspond to the same code."
	rm tutorial.cmi tutorial.cmo tutorial.annot

clean:
	rm -f javalib_tut.pdf javalib-tut.html style.css
