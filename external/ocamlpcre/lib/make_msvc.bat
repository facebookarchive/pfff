@echo off
REM Compile pcre-ocaml on windows with MSVC
REM (C) Troestler Ch., June 2007

set INSTALLDIR=%OCAMLLIB%\pcre
set STUBDIR=%OCAMLLIB%\stublibs

set PCRE_H=C:\pcre\pcre-7.7\include
REM Full path of the pcre C lib:
set PCRE_LIB=C:\pcre\pcre-7.7\lib\libpcre.lib

set OCAMLC=ocamlc
set OCAMLOPT=ocamlopt

echo ----------------------------------------------------------------------
REM Please read the README.win32 file in the ocaml distribution,
REM get the appropriate software for MSVC and compile in a shell with the
REM appropriate PATH set.  The latter is typically done by executing .bat :
REM
REM CALL "C:\Program Files\Microsoft Visual Studio 8\Common7\Tools\vsvars32.bat"
REM CALL "c:\Program Files\Microsoft Platform SDK"\SetEnv.Cmd /SRV32
echo Assumes VC++ installed:
echo i.e. "cl", "link", and "lib" are available
echo ----------------------------------------------------------------------

prompt $G$S
@echo on

%OCAMLC% -c pcre.mli
@echo --- Byte code library ---
REM cl /nologo /Ox /MT  /DPCRE_STATIC /I "%OCAMLLIB%" /I "%PCRE_H%" /c pcre_stubs.c /Fopcre_stubs.s.obj
REM lib /nologo /out:libpcre_stubs.lib pcre_stubs.s.obj
%OCAMLC% -I "%PCRE_H%" -ccopt /DPCRE_STATIC pcre_stubs.c
ocamlmklib -o pcre_stubs pcre_stubs.obj

@REM cl /nologo /Ox /MD /DCAML_DLL /I "%OCAMLLIB%" /I "%PCRE_H%" /c pcre_stubs.c /Fopcre_stubs.d.obj
@REM link /nologo /dll /out:dllpcre_stubs.dll /def:pcre_stubs.DEF pcre_stubs.d.obj "%OCAMLLIB%"\ocamlrun.lib %PCRE_LIB%
@REM copy dllpcre_stubs.dll "%STUBDIR%" >NUL

%OCAMLC% -custom -a -o pcre.cma pcre.ml pcre.mli -cclib "%PCRE_LIB%" -cclib -lpcre_stubs

@echo --- Native code library ---
%OCAMLOPT% -a -o pcre.cmxa pcre.ml pcre.mli -cclib "%PCRE_LIB%" -cclib -lpcre_stubs

@echo --- Installation ---
ocamlfind install pcre META libpcre_stubs.lib pcre.mli pcre.cmi pcre.cma pcre.cmxa pcre.lib
REM mkdir "%INSTALLDIR%"
REM copy libpcre_stubs.lib "%INSTALLDIR%" >NUL
REM copy pcre.mli       "%INSTALLDIR%"  >NUL
REM copy pcre.cmi       "%INSTALLDIR%"  >NUL
REM copy pcre.cma       "%INSTALLDIR%"  >NUL
REM copy pcre.cmxa      "%INSTALLDIR%"  >NUL
REM copy pcre_stubs.lib "%INSTALLDIR%"  >NUL
REM copy pcre.lib       "%INSTALLDIR%"  >NUL
REM copy META           "%INSTALLDIR%"  >NUL

@prompt $P$G$S
