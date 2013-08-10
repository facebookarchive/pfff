#!/bin/sh

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

#------------------------------
# Install OPAM
#------------------------------

# install opam, does take quite some time (=~ 10min)
wget http://www.ocamlpro.com/pub/opam_installer.sh
# redirecting to >/dev/null is not always good because travis
# has a 10min timeout on any program that don't output
# something on the console
yes | sh ./opam_installer.sh /usr/local/bin > /dev/null

# install packages from opam
# ??

eval `opam config env`

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
#todo: need swipl, need hg
#make test
