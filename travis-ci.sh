#!/bin/sh

# See https://travis-ci.org/facebook/pfff

#------------------------------
# Install ocaml
#------------------------------

# Using apt-get is very very slow but only at the beginning apparently.
# Once it has downloaded the ocaml package, travis will cache it
# somewhere and different runs of the sandbox can reuse this cache.
# Note that the default ocaml version is pretty old though (3.12.1).

#sudo apt-get update -qq
#todo: too old, does not have cmt_format which is needed by lang_ml
#less: could remove the dependency in configure?
#sudo apt-get install -qq ocaml
#sudo apt-get install -qq ocaml-compiler-libs
#sudo apt-get install -qq ocaml-native-compilers

#alternative: special packages for recent version of ocaml and opam, see:
# http://anil.recoil.org/2013/09/30/travis-and-ocaml.html

#------------------------------
# Install OPAM
#------------------------------

# install opam, does take quite some time (=~ 10min)
wget https://raw.githubusercontent.com/ocaml/opam/master/shell/opam_installer.sh
# redirecting to >/dev/null is not always good because travis
# has a 10min timeout on any program that don't output
# something on the console
yes | sh ./opam_installer.sh /usr/local/bin

eval `opam config env`

# install packages from opam:
#  pfff is battery included so no need for external packages
# camlp4 is now outside the ocaml distribution
opam install -y camlp4

#------------------------------
# Compile Pfff
#------------------------------

#todo: make configure detect the use of ocamlc.opt
#less: or use source env.sh?
./configure
make depend
make

#------------------------------
# Run tests
#------------------------------

# some tests are using hg or swipl
sudo apt-get install -qq mercurial
sudo apt-get install -qq swi-prolog

# a partial copy of env.sh
# set PFFF_HOME for many tests to be able to find their data
export PFFF_HOME=`pwd`

# for removing some warnings when unit testing git
git config --global user.email "you@example.com"
git config --global user.name "Pad"
# same but for mercurial
printf '[ui]\nusername = Pad<you@example.com>\n' > ~/.hgrc

make test
