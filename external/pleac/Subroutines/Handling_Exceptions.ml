(* ********************************************************************** *)
(* Handling Exceptions *)
(* ********************************************************************** *)
let pleac_Handling_Exceptions () = 
  (* To handle exceptions, which are thrown with the raise keword, wrap the
   * possibly exceptional call in a try ... with block.  You only need to do this
   * where appropriate *)
  
  let slurp_to_list filename =
    (* Note, if filename does not exist in the current directory, it will raise a
     * Sys_error exception *)
    let ic = open_in filename and
    l = ref [] in
    let rec loop () =
      let line = input_line ic in
      l := line::!l;
      loop () in
    try loop () with End_of_file -> close_in ic; List.rev !l;;
  
  let lfind name l =
    (* Note, if no elements in the list satisfy the predicate, List.find will
     * raise the Not_found exception *)
    List.find (fun x -> Str.string_match (Str.regexp ("$" ^ name)) x 0) l;;
  
  let findSmurfette =
    try
      print_endline (lfind "Smurfette" (slurp_to_list "smurfs"))
    with
      Sys_error s -> prerr_endline ("Dammit! - " ^ s)
    | Not_found -> prerr_endline "Hmmm... Smurfette is not in smurfs";;
  

