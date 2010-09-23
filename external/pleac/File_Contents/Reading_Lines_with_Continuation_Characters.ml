(* ********************************************************************** *)
(* Reading Lines with Continuation Characters *)
(* ********************************************************************** *)
let pleac_Reading_Lines_with_Continuation_Characters () = 
  let () =
    let buffer = Buffer.create 16 in
    let rec loop () =
      let line = input_line chan in
      if line <> "" && line.[String.length line - 1] = '\\'
      then (Buffer.add_string
              buffer (String.sub line 0 (String.length line - 1));
            loop ())
      else Buffer.add_string buffer line;
      let line = Buffer.contents buffer in
      Buffer.clear buffer;
      (* process full record in line here *)
      loop () in
    try loop () with End_of_file -> ()
  

