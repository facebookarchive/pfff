#!/bin/sh

#------------------------------
# install ocaml
#------------------------------

# Using apt-get is very very slow but only at the beginning apparently.
# Once it has downloaded the ocaml package, travis will cache it
# somewhere and different runs of the sandbox can reuse this cache.
# Note that the default version is pretty old though (ocaml 3.12). 

#sudo apt-get update -qq
sudo apt-get install -qq ocaml
sudo apt-get install -qq ocaml-compiler-libs

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

./configure
make depend
source env.sh
make

#------------------------------
# Run tests
#------------------------------
make test
