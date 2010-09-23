(* ********************************************************************** *)
(* Putting Commas in Numbers *)
(* ********************************************************************** *)
let pleac_Putting_Commas_in_Numbers () = 
  (* This example requires the PCRE library, available at:
     http://www.ocaml.info/home/ocaml_sources.html#pcre-ocaml *)
  #directory "+pcre";;
  #load "pcre.cma";;
  
  let rev_string s =
    let s' = String.copy s in
    let i = ref (String.length s - 1) in
    String.iter (fun c -> s'.[!i] <- c; decr i) s;
    s'
  
  let commify s =
    rev_string
      (Pcre.replace ~pat:"(\\d\\d\\d)(?=\\d)(?!\\d*\\.)" ~templ:"$1,"
         (rev_string s))
  
  (*-----------------------------*)
  
  (* more reasonable web counter :-) *)
  let () =
    Random.self_init ();
    let hits = Random.int32 2147483647l in
    Printf.printf "Your web page received %s accesses last month.\n"
      (commify (Int32.to_string hits))
  (* Your web page received 1,670,658,439 accesses last month. *)
  

