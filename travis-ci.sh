#!/bin/sh

#------------------------------
# Install ocaml
#------------------------------

# Using apt-get is very very slow but only at the beginning apparently.
# Once it has downloaded the ocaml package, travis will cache it
# somewhere and different runs of the sandbox can reuse this cache.
# Note that the default ocaml version is pretty old though (3.12.1). 

#sudo apt-get update -qq
sudo apt-get install -qq ocaml
sudo apt-get install -qq ocaml-compiler-libs
sudo apt-get install -qq ocaml-native-compilers

#------------------------------
# Install OPAM
#------------------------------

# install opam, does not take too much time
#wget http://www.ocamlpro.com/pub/opam_installer.sh
# can't redirect to >/dev/null because travis
# put 10min timeout on any program that don't output
# something on the console
#yes | sh ./opam_installer.sh /usr/local/bin > /dev/null

# install packages from opam
# ??

#eval `opam config env`

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
#todo: need swipl, and fix scheck regressions, also pbs with vcs tests
#make test
