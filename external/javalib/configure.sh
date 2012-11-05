#!/bin/bash

###
### A configuration script for Javalib
###
###     Provide a "local" configuration option
###     Detect ocamlfind
###     Determine whether camlzip and ptrees need to be make'd
###     Check the presence of unix, str, extlib
###     Check for recode
###     Set the debug flag
###     Select the camlp4o executable
###     Infer the destdir value from the localdest flag
###     Infer the ocamlopt value from the debug flag
###     Write the variables to the Makefile.config file
###     
###     
### Copyright (c)2010 Florent Kirchner
### Copyright (c)2010, 2012 Vincent Monfort
### 
### This program is free software: you can redistribute it and/or
### modify it under the terms of the GNU Lesser General Public License
### as published by the Free Software Foundation, either version 3 of
### the License, or (at your option) any later version.
### 
### This program is distributed in the hope that it will be useful, but
### WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
### Lesser General Public License for more details.
### 
### You should have received a copy of the GNU Lesser General Public
### License along with this program.  If not, see 
### <http://www.gnu.org/licenses/>.
### 
### This file: began on         march-18-2010,
###            last updated on  june-20-2012.
###


# The directory for local installations. Leave it empty if the install goes
# global.
LOCALDEST=
# The destdir argument to "ocamlfind install" (depends on LOCALDEST)
DESTDIR=
# The ocamlpath variable for the compiler to locate the locally-installed
# packages (depends on LOCALDEST)
OCAMLPATH=
# The packages that need to be made in addition to Savalib / Sawja
MAKEDEP=
# The packages that need to be made in addition to Savalib / Sawja
MAKEDEPREMOVE=
# The path to ocamlfind
FINDER=`which ocamlfind`
# The debug flag
DEBUG=yes
# The shared option flag
SHARED=
# The ocamlopt flags (depends on DEBUG)
OPT_FLAGS=
# The ocamlc flags (depends on DEBUG)
FLAGS="-w Aer"
# The flag signalling the absence of ZLIB (used to warn before camlzip compiles)
ZLIBFLAG=
# Do version check for packages 
VCHECK="true"

# Differentiated error numbers make for easier bug hunting. Hopefully we won't
# have to use them.
E_MAKERERROR=83
E_SCRIPTERROR=84


#
# The msg recursive function takes care of the pretty-printing.
# It uses "fmt" to stick to 75 characters columns.
#
function msg() 
{
  if [ $# -eq 2 ]; then
    if [ $1 = "err" ]; then
      echo ""
      echo "! configure error: $2." | fmt >&2
      exit $E_MAKERERROR
    elif [ $1 = "inf" ]; then
      echo "* $2." | fmt
      return 0
    elif [ $1 = "maj" ]; then
        echo ""
	echo "! $2." | fmt
        echo ""
	return 0
    fi
  elif [ $# -eq 3 ]; then
    if [ $1 = "ser" ]; then
      echo ""
      echo "! script error ($2): $3. Please file a bug." | fmt >&2
      exit $E_SCRIPTERROR
    fi
    msg "ser" "msg" "unexpected message type"
  else
    msg "ser" "msg" "incorrect number of message arguments"
  fi
}


#
# The push function takes an atom and a variable that contains a list, and
# performs the corresponding push.
#
# For instance, if LIST=bar\ baz, then after 'push foo LIST', LIST=foo\ bar\ baz.
#
function push ()
{
    cmd="$2=(\${$2[@]} $1)"
    eval $cmd
    return 0
}

#
# Compare version numbers: return 10 if ==, 11 if v1 > v2 and 9 if v1 < v2
#
function do_version_check() {

   [ "$1" == "$2" ] && return 10

   ver1front=`echo $1 | cut -d "." -f -1`
   ver1back=`echo $1 | cut -d "." -f 2-`

   ver2front=`echo $2 | cut -d "." -f -1`
   ver2back=`echo $2 | cut -d "." -f 2-`

   if [ "$ver1front" != "$1" ] || [ "$ver2front" != "$2" ]; then
       [ "$ver1front" -gt "$ver2front" ] && return 11
       [ "$ver1front" -lt "$ver2front" ] && return 9

       [ "$ver1front" == "$1" ] || [ -z "$ver1back" ] && ver1back=0
       [ "$ver2front" == "$2" ] || [ -z "$ver2back" ] && ver2back=0
       do_version_check "$ver1back" "$ver2back"
       return $?
   else
           [ "$1" -gt "$2" ] && return 11 || return 9
   fi
}


#
# Macro function to print a usage message.
#
function print_usage()
{
  echo -e "
Javalib configure.sh
Usage: `basename $0` [-l [PATH|default]] [-d [yes|no|prof]] [-h]
Options:
  -l PATH \t Perform a local installation at PATH.
  -d FLAG \t Use the debug flag when compiling (default: yes).
  -v  \t\t Deactivate version check for ocaml packages.
  -h  \t\t Print this message and exit."
}
#  -s  \t\t Complile a dynamically loadable plugin (cmxs).


#
# The option parsing function. Uses getopts, a bash built-in function.
#

while getopts "d:l:vh" opt
do
  case $opt in 
    h   ) print_usage
          exit 0;;
    v   ) VCHECK="false";;
    d   ) DEBUG=$OPTARG;;
    l   ) case "$OPTARG" in
            default)    tmpdest="`pwd`/lib";;
            *)          tmpdest="$OPTARG";;
          esac
          LOCALDEST=`(cd $tmpdest && pwd) 2>/dev/null`
          if [ $? != 0 ]; then
            msg "inf" "Local installation, but directory $tmpdest was not found"
            echo -n "  Creating directory... "
            #mkdir -p $tmpdest/stublibs
            mkdir -p $tmpdest
            echo "done."
            LOCALDEST=`(cd $tmpdest && pwd)` # This one can't fail!
          fi
          msg "inf" "Local installation, at $LOCALDEST"
          # For the rest of this configure, set OCAMLPATH to $LOCALDEST
          # NB: only children of this script are in the scope of 'export'.
          export OCAMLPATH=$LOCALDEST;;
    *   ) msg "err" "unrecognized option '$OPTARG'. Type '`basename $0` -h' to list available options";;
  esac
done

#    s   ) SHARED="javalib.cmxs"
#           msg "inf" "Plugin version of javalib will be generated at compilation (ocamlopt -shared)";;


shift $(($OPTIND - 1))

case $DEBUG in
    yes | YES | y | Y) OPT_FLAGS="-g" FLAGS="-g $FLAGS"
	msg "inf" "Debug flag set to yes";;
    prof | PROF | p | P) OPT_FLAGS="-g -p -noassert -ccopt -O3" FLAGS="-g $FLAGS"
	msg "inf" "Debug flag set to prof";;
    no | NO | n | N) OPT_FLAGS="-g -noassert -ccopt -O3" FLAGS="-g -noassert $FLAGS"
	msg "inf" "Debug flag set to no";;
    *)    msg "err" "debug option $DEBUG is not recognized"
esac

#
# Check Ocamlfind, print the global installation directory if relevant.
#
if [ $FINDER ]; then
  msg "inf" "Ocamlfind found at $FINDER"
else
  msg "err" "ocamlfind not found. Ocamlfind is part of the Findlib package management library, and is required to install Javalib/Sawja.

Use your system's software packaging tools to install Findlib, or download it from:
http://www.camlcity.org/archive/programming/findlib.html"
fi

if [ -z $LOCALDEST ]; then 
  msg "inf" "System-wide installation, in `$FINDER printconf destdir`" 
fi

#
# Check Ocaml version and add the correct flag in function
#
V=`$FINDER ocamlc -version`
OCAML_VERSION=${V:0:4}

if [ -z $OCAML_VERSION ] || [[ "$OCAML_VERSION" < "3.11" ]]; then
  FLAGS="$FLAGS -dtypes"
else
  FLAGS="$FLAGS -annot"
fi

      
#
# Check Camlzip, Ptrees, and Extlib. Set them to compile if necessary.
#

declare packages=(zip ptrees extlib)
declare versions=("1.05" "1.3" "1.5.1")

for (( i=0 ; i<3 ; i++ )) 
do
    pkg=${packages[i]}
    location=`$FINDER query $pkg 2>/dev/null`
    if [ $location ]; then
	aversion=`$FINDER query $pkg -format %v`
	rversion=${versions[i]}
	do_version_check $aversion $rversion
	if [ $? -eq 9 ] && [ $VCHECK = "true" ]; 
	then
	    msg "maj" "Package $pkg old version found ($location) in version $aversion (< $rversion needed), will need to be compiled and installed."
	    push "$pkg" MAKEDEP
	    push true MAKEDEPREMOVE
	else
	    msg "inf" "Package $pkg v$aversion found at $location"
	fi
    else 
	msg "inf" "Package $pkg not found, will need to be compiled"
	push "$pkg" MAKEDEP
	push false MAKEDEPREMOVE
    fi
done

#
# Check for zlib, set flag if not found
#
gcc -lz 2>&1 | grep "undefined reference to .main." > /dev/null
ZLIBFLAG=$?

#
# Check Camlp4, Unix, and Str
#
for pkg in unix str camlp4; do
location=`$FINDER query $pkg 2> /dev/null`
if [ $location ]; then
  msg "inf" "Package $pkg found at $location"
else 
  msg "err" "Package $pkg not found"
fi
done

#
# Infer the value of the DESTDIR and OPT_FLAGS variables
#
if [ -n "$LOCALDEST" ]; then
  DESTDIR="-destdir $LOCALDEST"
fi

#
# Output variables to the Makefile.config file
# TODO: move the Makefile.config.example out of the way (in src?)
#
makeconfig=`pwd`/Makefile.config
makeconfigtemplate=`pwd`/Makefile.config.example
# Copy the Makefile.config from template and add a warning header
msg "inf" "Writing $makeconfig"
echo "  Creating from $makeconfigtemplate."
# Header
echo "# WARNING: this file was automatically generated by `basename $0`." > $makeconfig
echo "# Edit at your own risk." >> $makeconfig
echo -n "  ."
# Configuration variables
echo "" >> $makeconfig
echo "# Variables detected at configure-time" >> $makeconfig
for var in FLAGS OPT_FLAGS LOCALDEST MAKEDEP FINDER DEBUG SHARED; do
  echo "$var=${!var}" >> $makeconfig
done
echo -n "."
# The rest from template
echo "" >> $makeconfig
echo "# Variables from template at: " >> $makeconfig
echo "# $makeconfigtemplate" >> $makeconfig
cat $makeconfigtemplate >> $makeconfig
echo -n "."
echo " done."


#
# Tell the user what to do next:
# - if MAKEDEP is non-empty, then compile and install the dependencies.
# - else compile and install Javalib 
#

if [ "$MAKEDEP" ]; then
  echo ""
  echo "WHAT'S NEXT: the following packages need to be compiled and installed:" | fmt
  echo "    $MAKEDEP"
  echo "In short, you will need to execute the following commands:" | fmt
  for (( i=0 ; i<${#MAKEDEP[@]} ; i++ )) do
  dep=${MAKEDEP[i]}
  remove=${MAKEDEPREMOVE[i]}
    # If $dep is camlzip, check the libz.so presence.
    if [ $dep = "camlzip" ] && [ $ZLIBFLAG -ne 0 ]; then
      echo " !! install the development package of 'zlib' library on your system. Then:"
    fi
    # Use sudo only if it's a nonlocal installation.
    if [ "$LOCALDEST" ]; then
	sudo=""
    else
	sudo="sudo "
    fi
    if ${remove} ; then
	echo "   make $dep && ${sudo}make remove$dep && ${sudo}make install$dep"
    else
	echo "   make $dep && ${sudo}make install$dep"
    fi
  done
  if [ "$LOCALDEST" ]; then
    echo "These packages will be installed in:" | fmt
    echo "    $LOCALDEST"
  fi
  echo "Once the packages have been installed, rerun your `basename $0` command to update the Javalib Makefiles." | fmt
else
    JAVALIB=`$FINDER query javalib 2>/dev/null`
    ALR_INST=$?
    echo ""
    echo "WHAT'S NEXT: all dependencies are satisfied."
    if [ $ALR_INST = 0 ]; then
	echo " A version of Javalib is already installed."
	echo " Compile, remove and install Javalib with the following commands:" | fmt
    else echo " Compile and install Javalib with the following commands:" | fmt
    fi   
    if [ "$LOCALDEST" ]; then
	if [ $ALR_INST = 0 ]; then
	    echo "    make && make remove install"
	else echo "    make && make install"
	fi   
    else
	if [ $ALR_INST = 0 ]; then
	    echo "    make && sudo make remove install"
	else echo "    make && sudo make install"
	fi   
    fi
fi
echo ""
echo "More details can be found in the installation documentation (INSTALL or http://javalib.gforge.inria.fr/javalib-doc.html)." | fmt

exit 0
