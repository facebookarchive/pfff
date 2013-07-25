#!/bin/sh

#------------------------------
# install ocaml
#------------------------------

# This is too slow, the bandwidth is extremly limited
#sudo apt-get update -qq
#sudo apt-get install ocaml-nox

# install opam, take less time
wget http://www.ocamlpro.com/pub/opam_installer.sh
yes | sh ./opam_installer.sh /usr/local/bin > /dev/null

# install packages from opam
# ??

eval `opam config env`

#------------------------------
# Compile Pfff
#------------------------------

./configure
make depend
make

#------------------------------
# Run tests
#------------------------------
