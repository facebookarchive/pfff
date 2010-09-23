(* ********************************************************************** *)
(* Introduction *)
(* ********************************************************************** *)
let pleac_Introduction () = 
  (* If you've never seen a URL before, here are a few examples. *)
  http://caml.inria.fr/
  http://www.ocaml-tutorial.org/
  http://en.wikipedia.org/wiki/Ocaml
  http://pleac.sourceforge.net/pleac_ocaml/index.html
  
  (* The URL for a form submission using the GET method will contain a
     query string (the sequence of characters after the '?') with named
     parameters of the form: key1=value1&key2=value2&... *)
  http://caml.inria.fr/cgi-bin/search.en.cgi?corpus=hump&words=cgi
  
  (* The URL for a form submission using POST will not usually contain
     a query string, so it will appear cleaner. *)
  http://caml.inria.fr/cgi-bin/hump.cgi
  
  (* GET requests are assumed to be "idempotent", meaning they can be
     requested many times without any different effect than if they were
     only requested once. This has the practical difference of making
     GET requests easy to cache, and POST requests nearly impossible
     (since there is no guarantee that a POST is non-destructive). It
     is considered best practice to use POST, not GET, for side-effecting
     operations such as deleting or modifying a record. *)
  

