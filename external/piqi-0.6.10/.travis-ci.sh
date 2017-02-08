#!/bin/sh

set -ex


case $TRAVIS_OS_NAME in
    osx)
        # build dependencies
        brew install opam

        # optional dependencies for running tests
        brew install protobuf

        echo OCaml version
        ocaml -version

        echo OPAM versions
        opam --version
        opam --git-version

        export OPAMYES=1
        rm -rf ~/.opam

        opam init
        eval `opam config env`
        opam install ocamlfind camlp4
        ;;

    *)
        # build dependencies
        sudo apt-get install ocaml-nox camlp4-extra ocaml-findlib

        # optional dependencies for running tests and building docs
        sudo apt-get install libprotoc-dev protobuf-compiler pandoc

        echo OCaml version
        ocaml -version
        ;;
esac


./configure
make deps
make

make -C tests

make -C doc html #test


# ex: sw=4 ts=4 et
