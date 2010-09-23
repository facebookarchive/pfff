(* ********************************************************************** *)
(* Reading a Particular Line in a File *)
(* ********************************************************************** *)
let pleac_Reading_a_Particular_Line_in_a_File () = 
  (* Read lines until the desired line number is found. *)
  let () =
    let line = ref "" in
    for i = 1 to desired_line_number do line := input_line handle done;
    print_endline !line
  
  (* Read lines into an array. *)
  let () =
    let lines = ref [] in
    (try while true do lines := input_line handle :: !lines done
     with End_of_file -> ());
    let lines = Array.of_list (List.rev !lines) in
    let line = lines.(desired_line_number) in
    print_endline line
  
  (* Build an index file containing line offsets. *)
  let build_index data_file index_file =
    set_binary_mode_out index_file true;
    let offset = ref 0 in
    try
      while true do
        ignore (input_line data_file);
        output_binary_int index_file !offset;
        offset := pos_in data_file
      done
    with End_of_file ->
      flush index_file
  
  (* Read a line using the index file. *)
  let line_with_index data_file index_file line_number =
    set_binary_mode_in index_file true;
    let size = 4 in
    let i_offset = size * (line_number - 1) in
    seek_in index_file i_offset;
    let d_offset = input_binary_int index_file in
    seek_in data_file d_offset;
    input_line data_file
  
  (*-----------------------------*)
  
  #!/usr/bin/ocaml
  (* print_line-v1 - linear style *)
  
  let () =
    if Array.length Sys.argv <> 3
    then (prerr_endline "usage: print_line FILENAME LINE_NUMBER"; exit 255);
  
    let filename = Sys.argv.(1) in
    let line_number = int_of_string Sys.argv.(2) in
    let infile =
      try open_in filename
      with Sys_error e -> (prerr_endline e; exit 255) in
    let line = ref "" in
    begin
      try
        for i = 1 to line_number do line := input_line infile done
      with End_of_file ->
        Printf.eprintf "Didn't find line %d in %s\n" line_number filename;
        exit 255
    end;
    print_endline !line
  
  (*-----------------------------*)
  
  #!/usr/bin/ocaml
  (* print_line-v2 - index style *)
  #load "unix.cma";;
  (* build_index and line_with_index from above *)
  let () =
    if Array.length Sys.argv <> 3
    then (prerr_endline "usage: print_line FILENAME LINE_NUMBER"; exit 255);
  
    let filename = Sys.argv.(1) in
    let line_number = int_of_string Sys.argv.(2) in
    let orig =
      try open_in filename
      with Sys_error e -> (prerr_endline e; exit 255) in
  
    (* open the index and build it if necessary *)
    (* there's a race condition here: two copies of this *)
    (* program can notice there's no index for the file and *)
    (* try to build one.  This would be easily solved with *)
    (* locking *)
    let indexname = filename ^ ".index" in
    let idx = Unix.openfile indexname [Unix.O_CREAT; Unix.O_RDWR] 0o666 in
    build_index orig (Unix.out_channel_of_descr idx);
  
    let line =
      try
        line_with_index orig (Unix.in_channel_of_descr idx) line_number
      with End_of_file ->
        Printf.eprintf "Didn't find line %d in %s\n" line_number filename;
        exit 255 in
    print_endline line
  

