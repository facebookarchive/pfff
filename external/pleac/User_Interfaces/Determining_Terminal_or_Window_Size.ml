(* ********************************************************************** *)
(* Determining Terminal or Window Size *)
(* ********************************************************************** *)
let pleac_Determining_Terminal_or_Window_Size () = 
  #load "unix.cma";;
  
  (* UNIX only, due to "stty". *)
  let get_terminal_size () =
    let in_channel = Unix.open_process_in "stty size" in
    try
      begin
        try
          Scanf.fscanf in_channel "%d %d"
            (fun rows cols ->
               ignore (Unix.close_process_in in_channel);
               (rows, cols))
        with End_of_file ->
          ignore (Unix.close_process_in in_channel);
          (0, 0)
      end
    with e ->
      ignore (Unix.close_process_in in_channel);
      raise e
  
  (* Display a textual bar chart as wide as the console. *)
  let () =
    let (height, width) = get_terminal_size () in
    if width < 10
    then (prerr_endline "You must have at least 10 characters";
          exit 255);
    let max_value = List.fold_left max 0.0 values in
    let ratio = (float width -. 10.0) /. max_value in
    List.iter
      (fun value ->
         Printf.printf "%8.1f %s\n"
           value
           (String.make (int_of_float (ratio *. value)) '*'))
      values
  

