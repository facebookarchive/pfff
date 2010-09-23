(* ********************************************************************** *)
(* Modifying a File in Place Without a Temporary File *)
(* ********************************************************************** *)
let pleac_Modifying_a_File_in_Place_Without_a_Temporary_File () = 
  #load "str.cma";;
  #load "unix.cma";;
  
  (* Modify a file in place. *)
  let modify func file =
    let in' = open_in file in
    let lines = ref [] in
    begin
      try
        while true do
          let line = input_line in' in
          lines := func line :: !lines
        done
      with End_of_file -> ()
    end;
    close_in in';
    let lines = List.rev !lines in
    let out = open_out file in
    List.iter
      (fun line ->
         output_string out line;
         output_string out "\n")
      lines;
    close_out out
  
  (* Replace DATE with the current date. *)
  let () =
    let tm = Unix.localtime (Unix.time ()) in
    let date = Printf.sprintf "%02d/%02d/%04d"
      (tm.Unix.tm_mon + 1)
      tm.Unix.tm_mday
      (tm.Unix.tm_year + 1900) in
    modify
      (Str.global_replace (Str.regexp "DATE") date)
      infile
  

