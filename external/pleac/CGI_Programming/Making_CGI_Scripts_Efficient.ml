(* ********************************************************************** *)
(* Making CGI Scripts Efficient *)
(* ********************************************************************** *)
let pleac_Making_CGI_Scripts_Efficient () = 
  (* Ocamlnet provides an Apache 2 module called netcgi_apache that allows
     Netcgi scripts to run inside the Apache process. To load the module,
     put something like the following in your Apache configuration file: *)
  
  LoadModule netcgi_module /usr/lib/apache2/modules/mod_netcgi_apache.so
  NetcgiLoad pcre/pcre.cma
  NetcgiLoad netsys/netsys.cma
  NetcgiLoad netstring/netstring.cma
  NetcgiLoad str.cma
  NetcgiLoad netcgi2/netcgi.cma
  NetcgiLoad netcgi_apache/netcgi_apache.cma
  
  (* Extra libraries can be added with additional "NetcgiLoad" directives.
     The following will enable netcgi_apache for *.cma files: *)
  
  NetcgiHandler Netcgi_apache.bytecode
  AddHandler ocaml-bytecode .cma
  
  (* Or, if you prefer, you can enable netcgi_apache for a directory: *)
  
  <Location /caml-bin>
    SetHandler ocaml-bytecode
    NetcgiHandler Netcgi_apache.bytecode
    Options ExecCGI
    Allow from all
  </Location>
  
  (* Each script contains code similar to other Netcgi examples but uses
     Netcgi_apache.run to run the process. *)
  
  let process (cgi : Netcgi_apache.cgi) =
    cgi#set_header ~content_type:"text/html" ();
    (* ... *)
    cgi#out_channel#commit_work ()
  
  let () =
    let config = Netcgi.default_config in
    let buffered _ ch = new Netchannels.buffered_trans_channel ch in
    let output_type = `Transactional buffered in
    Netcgi_apache.run ~config ~output_type process
  
  (* Scripts need to be compiled into bytecode libraries before Apache can
     execute them. If you have findlib installed, you can compile them as
     follows: *)
  
  ocamlfind ocamlc -package netcgi_apache -c myscript.ml
  ocamlfind ocamlc -a -o myscript.cma myscript.cmo
  
  (* Here is a Makefile to automate the build process. *)
  
  RESULTS = myscript.cma another.cma
  PACKS = netcgi_apache,anotherlib
  
  %.cmo : %.ml
  	ocamlfind ocamlc -package $(PACKS) -c $<
  
  %.cma : %.cmo
  	ocamlfind ocamlc -a -o $@ $<
  
  all: $(RESULTS)
  
  clean:
  	rm -f *.cma *.cmi *.cmo $(RESULTS)
  

