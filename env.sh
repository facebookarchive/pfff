#!!!!You need to source me with "source env.sh" from the _RIGHT_ directory!!!!
if [ ! -r main.ml ]
    then echo "There is no main.ml here. 
Are you sure you run this script from the source directory ?
";
fi

# To compile the source, using pad installation.
echo setting PATH
export PATH=/home/pad/packages/Linux/bin:$PATH
echo setting LD_LIBRARY_PATH
export LD_LIBRARY_PATH=/home/pad/packages/Linux/lib:$LD_LIBRARY_PATH

# for faster compiler
echo setting OPTBIN
export OPTBIN=.opt

# -bin-annot is for codegraph, needs ocaml 4.00, 
# -absname is for tuareg mode under my mac, also need ocaml 4.00
export OCAMLCFLAGS_EXTRA=-bin-annot -absname

# for exception stack traces
echo setting OCAMLRUNPARAM
export OCAMLRUNPARAM="b"

# To run. To find the config/ files.
echo setting PFFF_HOME
export PFFF_HOME=`pwd`
