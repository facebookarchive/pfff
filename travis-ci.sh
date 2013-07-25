#!/bin/sh

# install ocaml from apt
#sudo apt-get update -qq
sudo apt-get install ocaml-nox

# install opam
# TODO

# install packages from opam
# TODO


# compile & run tests
./configure
make


