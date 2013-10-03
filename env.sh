#!!!!You need to source me with "source env.sh" from the right directory!!!!
if [ ! -r main.ml ]
    then echo "There is no main.ml here.
Are you sure you ran this script from the source directory of pfff?
";
fi

# To compile the source, using pad installation.
echo setting OPAM
eval `~pad/packages/Linux/bin/opam config env`

# for faster compiler
echo setting OPTBIN
export OPTBIN=.opt

# -bin-annot is for codegraph, it needs ocaml 4.00,
# -absname is for tuareg mode under my mac, it also needs ocaml 4.00
export OCAMLCFLAGS_EXTRA="-bin-annot -absname"

# for exception stack traces
echo setting OCAMLRUNPARAM
export OCAMLRUNPARAM="b"

# To run, to find the data/ config files, and to run the tests, 
# to find the tests/ files.
echo setting PFFF_HOME
export PFFF_HOME=`pwd`
