@echo off
rem set objext to "o" and libext to "dll.a" for mingw
set objext=obj
set libext=lib
echo Library extension is "%libext%", object extension is "%objext%"
echo Builing lablrsvg.cma...
ocamlc -c rsvg.mli
ocamlc -a -o lablrsvg.cma rsvg.ml -cclib -llablrsvg -dllib -llablrsvg -cclib librsvg-2.%libext%
del rsvg.cmo
echo Building lablrsvg.cmxa...
ocamlopt -a -o lablrsvg.cmxa rsvg.ml -cclib -llablrsvg -cclib librsvg-2.%libext%
del rsvg.%objext%
