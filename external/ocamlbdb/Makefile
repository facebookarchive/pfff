TOP=../../
##############################################################################

# CONFIG
BDB_INCLUDE=/usr/include
BDB_LIBS=/usr/lib
# for Mac OS, TODO do via configure
#BDB_INCLUDE=/opt/local/include/db46
#BDB_LIBS=/opt/local/lib/db46
-include $(TOP)/Makefile.config
OCAML_LIBS=`ocamlc -where`

# added -fPIC otherwise get some 'relocation R_X86_64_32 ...' error
CFLAGS=-g -fPIC
#CFLAGS=-O2 -Werror-implicit-function-declaration
#-DDEBUG for debug output


##############################################################################
all: bdb.cma 
all.opt: bdb.cmxa
opt: all.opt

bdb.ml: bdb.src bdberrs.src
	cat bdb.src | perl extract_caml.pl > bdb.ml
	cpp -include $(BDB_INCLUDE)/db.h bdberrs.src | perl beginend.pl >> bdb.ml

bdb_stubs.c: bdb.src
	cp bdb.src bdb_stubs.c


bdb_stubs.o: bdb_stubs.c
	gcc $(CFLAGS) -I$(BDB_INCLUDE) -I$(OCAML_LIBS) -c bdb_stubs.c

bdb.cmo: bdb.ml
	ocamlc -bin-annot -c bdb.ml

bdb.cmx: bdb.ml
	ocamlopt -c bdb.ml

# use ocamlmklib
bdb.cma: bdb.cmo bdb.cmx bdb_stubs.o
	ocamlmklib $^ -o bdb -ldb -L$(BDB_LIBS) 


clean::
	rm -f dllbdb.so libbdb.a


#libcamlbdb.a: bdb_stubs.o 
#	rm -rf libcamlbdb.a
#	ar rc libcamlbdb.a bdb_stubs.o
#	ranlib libcamlbdb.a 

#bdb.cma: bdb.cmo libcamlbdb.a
#	ocamlc -a -o bdb.cma -custom bdb.cmo \
#		-cclib -lcamlbdb -cclib -ldb -ccopt "-L ."
#
#bdb.cmxa: bdb.cmx libcamlbdb.a
#	ocamlopt -a -o bdb.cmxa bdb.cmx \
#		-cclib -lcamlbdb -cclib -ldb -ccopt "-L ."


clean::
	rm -f bdb.cma bdb.cmxa libcamlbdb.a bdb_stubs.o bdbtop bdb_stubs.c \
	bdb_stubs.a bdb.a bdb.ml *.cmo *.cmi *.cmx *.o bdb.o bdb.cmx 

distclean:

##############################################################################
# PAD
##############################################################################
bdbo.cmo: bdbo.ml
	ocamlc -c -I .. bdbo.ml

bdbo.cmx: bdbo.ml
	ocamlopt bdbo.ml -I .. 

test.exe: test.ml bdb.src bdb_stubs.c
	ocamlc -c test.ml
	ocamlc bdb.cma unix.cma -ccopt "-L$(BDB_LIBS)" -o $@ test.cmo

test2.exe: test.ml bdb.src bdb_stubs.c
	ocamlopt -c -thread test.ml
	ocamlopt -thread bdb.cmxa -ccopt "-L$(BDB_LIBS)" unix.cmxa -o $@ test.cmx

clean2: 
	rm -f test test.cm* test.o

depend::
