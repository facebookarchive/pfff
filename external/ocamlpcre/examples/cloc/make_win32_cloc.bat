@echo off

set OCAMLC=ocamlc -g
set OCAMLOPT=ocamlopt

set MAIN=cloc

@echo on
%OCAMLC% -o %MAIN%.exe pcre.cma %MAIN%.ml
%OCAMLOPT% -o %MAIN%.com pcre.cmxa %MAIN%.ml

@REM Example use:
@REM %MAIN%.exe < ..\..\lib\pcre_stubs.c
@REM %MAIN%.com < ..\..\lib\pcre_stubs.c
