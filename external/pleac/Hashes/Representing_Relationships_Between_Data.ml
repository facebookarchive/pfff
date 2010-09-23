(* ********************************************************************** *)
(* Representing Relationships Between Data *)
(* ********************************************************************** *)
let pleac_Representing_Relationships_Between_Data () = 
  (*-----------------------------*)
  
  open Printf;;
  open Hashtbl;;
  
  let father = assoc_list2hashtbl
    [ "Cain", "Adam";
      "Abel", "Adam";
      "Seth", "Adam";
      "Enoch", "Cain";
      "Irad", "Enoch";
      "Mehujael", "Irad";
      "Methusael", "Mehujael";
      "Lamech", "Methusael";
      "Jabal", "Lamech";
      "Jubal", "Lamech";
      "Tubalcain", "Lamech";
      "Enos", "Seth"] ;;
  (*-----------------------------*)
  (* recursively print all parents of a given name *)
  let rec parents s =
    printf "%s " s;
    if mem father s then
      parents (find father s)
    else
      printf "\n"
  in
    iter_lines parents stdin
  ;;
  (*-----------------------------*)
  let children = hashtbl_reverse_multi father in
  iter_lines 
    (fun line ->
      List.iter (printf "%s ") (find_all children line);
      print_newline()
    )
    stdin;
  ;;
  (*-----------------------------*)
  (* build an hash that map filename to list of included file *)
  open Hashtbl;;
  open Str;;
  
  let includes = create (List.length files);;
  let includeRE = regexp "^#include <\([a-zA-Z0-9.]+\)>";;
  let isincludeline l = string_match includeRE l 0;;
  let getincludes fname =
    let includelines =
      List.filter isincludeline (readlines (open_in fname))
    in
    List.map (replace_first includeRE "\1") includelines
  ;;
  List.iter (fun fname -> add includes fname (getincludes fname)) files;;
  
  (*-----------------------------*)
  (* build a list of files that does not include system headers *)
  let hasnoinclude fname = (find includes fname = []) in
  List.filter hasnoinclude (uniq (hashtbl_keys includes));;
  
  (*-----------------------------*)

