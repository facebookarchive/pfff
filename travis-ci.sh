#!/bin/sh

# install ocaml
#too slow, bandwidth is extremly limited
#sudo apt-get update -qq
#sudo apt-get install ocaml-nox
wget http://www.ocamlpro.com/pub/opam_installer.sh
echo 'y' | sh ./opam_installer.sh /usr/local/bin

# install opam
# TODO

# install packages from opam
# TODO


# compile & run tests
./configure
make


