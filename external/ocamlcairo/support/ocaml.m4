dnl -*- autoconf -*- macros for OCaml
dnl by Olivier Andrieu
dnl from a configure.in by Jean-Christophe Filliâtre,
dnl from a first script by Georges Mariano
dnl
dnl defines AC_PROG_OCAML that will check the OCaml compiler
dnl and set the following variables :
dnl   OCAMLC        "ocamlc" if present in the path, or a failure
dnl                 or "ocamlc.opt" if present with same version number as ocamlc
dnl   OCAMLOPT      "ocamlopt" (or "ocamlopt.opt" if present), or unset
dnl   OCAMLBEST     either "byte" if no native compiler was found, 
dnl                 "opt" otherwise
dnl   OCAMLDEP      "ocamldep"
dnl   OCAMLLIB      the path to the ocaml standard library
dnl   OCAMLVERSION  the ocaml version number
dnl
dnl   OCAMLMKTOP    
dnl   OCAMLMKLIB    
dnl   OCAMLDOC
AC_DEFUN([AC_PROG_OCAML],
[dnl
# checking for ocamlc
AC_CHECK_PROG(OCAMLC,ocamlc,ocamlc,AC_MSG_ERROR(Cannot find ocamlc.))
OCAMLVERSION=$($OCAMLC -version)
AC_MSG_RESULT(OCaml version is $OCAMLVERSION)
OCAMLLIB=$($OCAMLC -where)
AC_MSG_RESULT(OCaml library path is $OCAMLLIB)
# checking for ocamlopt
AC_CHECK_PROG(OCAMLOPT,ocamlopt,ocamlopt)
OCAMLBEST=byte
if test -z "$OCAMLOPT"; then
	AC_MSG_WARN(Cannot find ocamlopt; bytecode compilation only.)
else
	TMPVERSION=$($OCAMLOPT -version)
	if test "$TMPVERSION" != "$OCAMLVERSION" ; then
	    AC_MSG_RESULT(versions differs from ocamlc; ocamlopt discarded.)
	    unset OCAMLOPT
	else
	    OCAMLBEST=opt
	fi
fi
# checking for ocamlc.opt
AC_CHECK_PROG(OCAMLCDOTOPT,ocamlc.opt,ocamlc.opt)
if test "$OCAMLCDOTOPT"; then
	TMPVERSION=$($OCAMLCDOTOPT -version)
	if test "$TMPVERSION" != "$OCAMLVERSION" ; then
	    AC_MSG_RESULT(versions differs from ocamlc; ocamlc.opt discarded.)
	else
	    OCAMLC=$OCAMLCDOTOPT
	fi
fi
# checking for ocamlopt.opt
if test "$OCAMLOPT" ; then
    AC_CHECK_PROG(OCAMLOPTDOTOPT,ocamlopt.opt,ocamlopt.opt)
    if test "$OCAMLOPTDOTOPT"; then
	TMPVERSION=$($OCAMLOPTDOTOPT -version)
	if test "$TMPVERSION" != "$OCAMLVERSION" ; then
	    AC_MSG_RESULT(version differs from ocamlc; ocamlopt.opt discarded.)
	else
	    OCAMLOPT=$OCAMLOPTDOTOPT
	fi
    fi
fi
# checking for ocamldep
AC_CHECK_PROG(OCAMLDEP,ocamldep,ocamldep,AC_MSG_ERROR(Cannot find ocamldep.))

#checking for ocamlmktop
AC_CHECK_PROG(OCAMLMKTOP,ocamlmktop,ocamlmktop, AC_MSG_WARN(Cannot find ocamlmktop.))
#checking for ocamlmklib
AC_CHECK_PROG(OCAMLMKLIB,ocamlmklib,ocamlmklib, AC_MSG_WARN(Cannot find ocamlmklib.))
# checking for ocamldoc
AC_CHECK_PROG(OCAMLDOC,ocamldoc,ocamldoc, AC_MSG_WARN(Cannot find ocamldoc.))

# get the C compiler used by ocamlc
if test -z "$CC" ; then
  touch conftest.c
  CC=$($OCAMLC -verbose conftest.c 2>&1 | awk '/^+/ {print $[]2 ; exit}')
  echo OCaml uses $CC to compile C files
fi

AC_SUBST(OCAMLC)
AC_SUBST(OCAMLOPT)
AC_SUBST(OCAMLDEP)
AC_SUBST(OCAMLBEST)
AC_SUBST(OCAMLVERSION)
AC_SUBST(OCAMLLIB)
AC_SUBST(OCAMLMKLIB)
AC_SUBST(OCAMLMKTOP)
AC_SUBST(OCAMLDOC)
])
dnl
dnl
dnl
dnl macro AC_PROG_OCAML_TOOLS will check OCamllex and OCamlyacc :
dnl   OCAMLLEX      "ocamllex" or "ocamllex.opt" if present
dnl   OCAMLYACC     "ocamlyac"
AC_DEFUN([AC_PROG_OCAML_TOOLS],
[dnl
# checking for ocamllex and ocamlyacc
AC_CHECK_PROG(OCAMLLEX,ocamllex,ocamllex,AC_MSG_ERROR(Cannot find ocamllex.))
if test "$OCAMLLEX"; then
    AC_CHECK_PROG(OCAMLLEXDOTOPT,ocamllex.opt,ocamllex.opt)
    if test "$OCAMLLEXDOTOPT"; then
	OCAMLLEX=$OCAMLLEXDOTOPT
    fi
else
	AC_MSG_ERROR(Cannot find ocamllex.)
fi
AC_CHECK_PROG(OCAMLYACC,ocamlyacc,ocamlyacc,AC_MSG_ERROR(Cannot find ocamlyacc.))
AC_SUBST(OCAMLLEX)
AC_SUBST(OCAMLYACC)
])
dnl
dnl
dnl
dnl AC_PROG_CAMLP4 checks for Camlp4
AC_DEFUN([AC_PROG_CAMLP4],
[dnl
AC_REQUIRE([AC_PROG_OCAML])
# checking for camlp4
AC_CHECK_PROG(CAMLP4,camlp4,camlp4)
if test "$CAMLP4"; then
	TMPVERSION=$($CAMLP4 -v 2>&1| sed -n -e 's|.*version *\(.*\)$|\1|p')
	if test "$TMPVERSION" != "$OCAMLVERSION" ; then
	    AC_MSG_ERROR(versions differs from ocamlc)
 	else
	    AC_CHECK_PROG(CAMLP4O,camlp4o,camlp4o)
	fi
fi
AC_SUBST(CAMLP4)
AC_SUBST(CAMLP4O)
])
dnl
dnl
dnl
dnl macro AC_PROG_FINDLIB will check for the presence of
dnl   ocamlfind if configure is called with --with-findlib
AC_DEFUN([AC_PROG_FINDLIB],
[dnl
AC_ARG_WITH(findlib,[  --with-findlib	  use findlib package system],
  use_findlib="$withval")
# checking for ocamlfind
if test "$use_findlib" = yes ; then 
	AC_CHECK_PROG(OCAMLFIND,ocamlfind,ocamlfind,
	  AC_MSG_ERROR(ocamlfind not found))
else
	unset OCAMLFIND
fi
AC_SUBST(OCAMLFIND)
])
dnl
dnl
dnl
dnl AC_CHECK_OCAML_PKG checks wether a findlib package is present
dnl   defines pkg_name to name
AC_DEFUN([AC_CHECK_OCAML_PKG],
[dnl
AC_REQUIRE([AC_PROG_FINDLIB])
if test "$use_findlib" = yes ; then 
	AC_MSG_CHECKING(findlib package $1)
	if $OCAMLFIND query $1 >/dev/null 2>/dev/null; then
	AC_MSG_RESULT(found)
	pkg_$1="$1"
	else
	AC_MSG_WARN(not found)
	unset pkg_$1
	fi
fi
])
dnl
dnl
dnl
dnl AC_ARG_OCAML_INSTALLDIR adds a --with-installdir option
AC_DEFUN([AC_ARG_OCAML_INSTALLDIR],
[dnl
AC_ARG_WITH(installdir,[  --with-installdir=DIR	  specify installation directory],INSTALLDIR="$withval")
if ! test "$INSTALLDIR" -o "$use_findlib" ; then
    INSTALLDIR='$(OCAMLLIB)/$(NAME)'
fi
AC_SUBST(INSTALLDIR)
])
dnl
dnl
dnl
dnl AC_CHECK_OCAML_MODULE looks for a module in a given path
dnl 1 -> name (for printing)
dnl 2 -> env var name
dnl 3 -> module to check
dnl 4 -> default dirs
AC_DEFUN([AC_CHECK_OCAML_MODULE],
[dnl
AC_MSG_CHECKING($1 directory)
cat > conftest.ml <<EOF
open $3
EOF
unset found
for $2 in $$2 $4 ; do
  if $OCAMLC -c -I "$$2" conftest.ml > /dev/null 2>&1 ; then
    found=yes
    break
  fi
done
if test "$found" ; then
  AC_MSG_RESULT($$2)
else
  AC_MSG_RESULT(not found)
  unset $2
fi
AC_SUBST($2)])
