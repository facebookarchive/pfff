(* ********************************************************************** *)
(* Building and Installing a CPAN Module *)
(* ********************************************************************** *)
let pleac_Building_and_Installing_a_CPAN_Module () = 
  (* Installing a module from The Caml Hump differs from project to
     project, since it is not as standardized as CPAN. However, in most
     cases, "make" and "make install" do what you expect. Here's how to
     install easy-format, which can be found on the Hump at the following
     URL: http://caml.inria.fr/cgi-bin/hump.en.cgi?contrib=651 *)
  
  $ tar xzf easy-format.tar.gz
  $ cd easy-format
  $ make
  ocamlc -c easy_format.mli
  ocamlc -c -dtypes easy_format.ml
  touch bytecode
  ocamlc -c easy_format.mli
  ocamlopt -c -dtypes easy_format.ml
  touch nativecode
  
  $ sudo make install
  [sudo] password for root: ........
  echo "version = \"1.0.0\"" > META; cat META.tpl >> META
  INSTALL_FILES="META easy_format.cmi easy_format.mli"; \
                  if test -f bytecode; then \
                    INSTALL_FILES="$INSTALL_FILES easy_format.cmo "; \
                  fi; \
                  if test -f nativecode; then \
                    INSTALL_FILES="$INSTALL_FILES easy_format.cmx easy_format.o"; \
                  fi; \
                  ocamlfind install easy-format $INSTALL_FILES
  Installed /usr/local/lib/ocaml/3.10.2/easy-format/easy_format.o
  Installed /usr/local/lib/ocaml/3.10.2/easy-format/easy_format.cmx
  Installed /usr/local/lib/ocaml/3.10.2/easy-format/easy_format.cmo
  Installed /usr/local/lib/ocaml/3.10.2/easy-format/easy_format.mli
  Installed /usr/local/lib/ocaml/3.10.2/easy-format/easy_format.cmi
  Installed /usr/local/lib/ocaml/3.10.2/easy-format/META
  

