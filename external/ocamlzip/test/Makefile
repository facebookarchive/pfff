all: minizip minigzip

minigzip: ../zip.cma minigzip.ml
	ocamlc -ccopt -g -g -I .. -o minigzip ../zip.cma minigzip.ml

minizip: ../zip.cma minizip.ml
	ocamlc -ccopt -g -g -I .. -o minizip unix.cma ../zip.cma minizip.ml

testzlib: ../zip.cma testzlib.ml
	ocamlc -g -I .. -o testzlib ../zip.cma testzlib.ml

clean:
	rm -f *.cm*
	rm -f minigzip minizip testzlib
