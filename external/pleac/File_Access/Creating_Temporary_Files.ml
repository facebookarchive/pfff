(* ********************************************************************** *)
(* Creating Temporary Files *)
(* ********************************************************************** *)
let pleac_Creating_Temporary_Files () = 
  (* Open a new temporary file for writing. Filename.open_temp_file
     safeguards against race conditions and returns both the filename
     and an output channel. *)
  let name, out_channel = Filename.open_temp_file "prefix-" ".suffix"
  
  (* Install an at_exit handler to remove the temporary file when this
     program exits. *)
  let () = at_exit (fun () -> Sys.remove name)
  
  (*-----------------------------*)
  
  #load "unix.cma";;
  
  let () =
    (* Open a temporary file for reading and writing. *)
    let name = Filename.temp_file "prefix-" ".suffix" in
    let descr = Unix.openfile name [Unix.O_RDWR] 0o600 in
  
    (* Write ten lines of output. *)
    let out_channel = Unix.out_channel_of_descr descr in
    for i = 1 to 10 do
      Printf.fprintf out_channel "%d\n" i
    done;
    flush out_channel;
  
    (* Seek to the beginning and read the lines back in. *)
    let in_channel = Unix.in_channel_of_descr descr in
    seek_in in_channel 0;
    print_endline "Tmp file has:";
    let rec loop () =
      print_endline (input_line in_channel);
      loop () in
    try loop() with End_of_file -> ();
  
    (* Close the underlying file descriptor and remove the file. *)
    Unix.close descr;
    Sys.remove name
  

