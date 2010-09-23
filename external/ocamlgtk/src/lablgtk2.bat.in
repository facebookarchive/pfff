@echo off
rem launcher for lablgtk2

set gtklibs=lablgtk.cma

if "%1" == "-thread" goto thread
set initobjs=gtkInit.cmo
goto next

:thread
shift
set initobjs=-I +threads unix.cma threads.cma gtkThread.cmo gtkInit.cmo gtkThInit.cmo

:next
ocaml -w s -I +lablgtk2 %gtklibs% %initobjs% %1 %2 %3 %4 %5 %6 %7 %8 %9
