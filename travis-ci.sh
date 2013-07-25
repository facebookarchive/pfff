#!/bin/sh

# install ocaml
#too slow, bandwidth is extremly limited
#sudo apt-get update -qq
#sudo apt-get install ocaml-nox
wget http://www.ocamlpro.com/pub/opam_installer.sh
yes | sh ./opam_installer.sh /usr/local/bin > /dev/null

# install opam
# TODO

# install packages from opam
# TODO

eval `opam config env`

# compile & run tests
./configure
make


