(* ********************************************************************** *)
(* Gathering Output from a Program *)
(* ********************************************************************** *)
let pleac_Gathering_Output_from_a_Program () = 
  (* Process support is mostly in the "unix" library. *)
  #load "unix.cma";;
  
  (* Run a command and return its results as a string. *)
  let read_process command =
    let buffer_size = 2048 in
    let buffer = Buffer.create buffer_size in
    let string = String.create buffer_size in
    let in_channel = Unix.open_process_in command in
    let chars_read = ref 1 in
    while !chars_read <> 0 do
      chars_read := input in_channel string 0 buffer_size;
      Buffer.add_substring buffer string 0 !chars_read
    done;
    ignore (Unix.close_process_in in_channel);
    Buffer.contents buffer
  
  (* Run a command and return its results as a list of strings,
     one per line. *)
  let read_process_lines command =
    let lines = ref [] in
    let in_channel = Unix.open_process_in command in
    begin
      try
        while true do
          lines := input_line in_channel :: !lines
        done;
      with End_of_file ->
        ignore (Unix.close_process_in in_channel)
    end;
    List.rev !lines
  
  (* Example: *)
  let output_string = read_process "program args"
  let output_lines = read_process_lines "program args"
  
  (*-----------------------------*)
  
  (* Create a pipe for the subprocess output. *)
  let readme, writeme = Unix.pipe ()
  
  (* Launch the program, redirecting its stdout to the pipe.
     By calling Unix.create_process, we can avoid running the
     command through the shell. *)
  let () =
    let pid = Unix.create_process
      program [| program; arg1; arg2 |]
      Unix.stdin writeme Unix.stderr in
    Unix.close writeme;
    let in_channel = Unix.in_channel_of_descr readme in
    let lines = ref [] in
    begin
      try
        while true do
          lines := input_line in_channel :: !lines
        done
      with End_of_file -> ()
    end;
    Unix.close readme;
    List.iter print_endline (List.rev !lines)
  

