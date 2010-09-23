(* ********************************************************************** *)
(* Modifying a File in Place with Temporary File *)
(* ********************************************************************** *)
let pleac_Modifying_a_File_in_Place_with_Temporary_File () = 
  (* Modify a file in place. *)
  let modify func old new' =
    let old_in = open_in old in
    let new_out = open_out new' in
    begin
      try
        while true do
          let line = input_line old_in in
          func new_out line
        done
      with End_of_file -> ()
    end;
    close_in old_in;
    close_out new_out;
    Sys.rename old (old ^ ".orig");
    Sys.rename new' old
  
  (* Insert lines at line 20. *)
  let () =
    let count = ref 0 in
    modify
      (fun out line ->
         incr count;
         if !count = 20
         then (output_string out "Extra line 1\n";
               output_string out "Extra line 2\n");
         output_string out line;
         output_string out "\n")
      old new'
  
  (* Delete lines 20..30. *)
  let () =
    let count = ref 0 in
    modify
      (fun out line ->
         incr count;
         if !count < 20 || !count > 30
         then (output_string out line;
               output_string out "\n"))
      old new'
  

