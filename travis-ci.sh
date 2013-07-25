#!/bin/sh

#------------------------------
# install ocaml
#------------------------------

# This is too slow, the bandwidth is extremly limited
#sudo apt-get update -qq
#sudo apt-get install -qq ocaml

# install opam, take less time
wget http://www.ocamlpro.com/pub/opam_installer.sh
# can't redirect to >/dev/null because travis
# put 10min timeout on any program that don't output
# something on the console
yes | sh ./opam_installer.sh /usr/local/bin > /dev/null

# install packages from opam
# ??

eval `opam config env`

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
