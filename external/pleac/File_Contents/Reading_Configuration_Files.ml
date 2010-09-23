(* ********************************************************************** *)
(* Reading Configuration Files *)
(* ********************************************************************** *)
let pleac_Reading_Configuration_Files () = 
  #load "str.cma";;
  
  let user_preferences = Hashtbl.create 0
  
  let () =
    let comments = Str.regexp "#.*" in
    let leading_white = Str.regexp "^[ \t]+" in
    let trailing_white = Str.regexp "[ \t]+$" in
    let equals_delim = Str.regexp "[ \t]*=[ \t]*" in
    Stream.iter
      (fun s ->
         let s = Str.replace_first comments "" s in
         let s = Str.replace_first leading_white "" s in
         let s = Str.replace_first trailing_white "" s in
         (* anything left? *)
         if String.length s > 0 then
           match Str.bounded_split_delim equals_delim s 2 with
             | [var; value] -> Hashtbl.replace user_preferences var value
             | _ -> failwith s)
      (* defined in this chapter's introduction *)
      (line_stream_of_channel config)
  
  (*-----------------------------*)
  
  (* load variables from ocaml source - toplevel scripts only *)
  #use ".progrc";;
  

