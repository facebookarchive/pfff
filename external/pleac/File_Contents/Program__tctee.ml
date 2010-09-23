(* ********************************************************************** *)
(* Program: tctee *)
(* ********************************************************************** *)
let pleac_Program__tctee () = 
  #!/usr/bin/ocaml
  (* tctee - clone that groks process tees *)
  #load "unix.cma";;
  
  let ignore_ints = ref false
  let append      = ref false
  let unbuffer    = ref false
  let nostdout    = ref false
  let names       = ref []
  
  let () =
    Arg.parse
      [
        "-a", Arg.Set append,      "Append to output files";
        "-i", Arg.Set ignore_ints, "Ignore interrupts";
        "-u", Arg.Set unbuffer,    "Unbuffered output";
        "-n", Arg.Set nostdout,    "No standard output";
      ]
      (fun name -> names := name :: !names)
      (Printf.sprintf "Usage: %s [-a] [-i] [-u] [-n] [filenames] ..."
         Sys.argv.(0));
    names := List.rev !names
  
  let fhs = Hashtbl.create 0
  let status = ref 0
  
  let () =
    if not !nostdout then
      (* always go to stdout *)
      Hashtbl.replace fhs stdout "standard output";
  
    if !ignore_ints
    then
      List.iter
        (fun signal -> Sys.set_signal signal Sys.Signal_ignore)
        [Sys.sigint; Sys.sigterm; Sys.sighup; Sys.sigquit];
  
    List.iter
      (fun name ->
         if name.[0] = '|'
         then
           Hashtbl.replace fhs
             (Unix.open_process_out
                (String.sub name 1 (String.length name - 1)))
             name
         else
           begin
             let mode =
               if !append
               then [Open_wronly; Open_creat; Open_append]
               else [Open_wronly; Open_creat; Open_trunc] in
             try Hashtbl.replace fhs (open_out_gen mode 0o666 name) name
             with Sys_error e ->
               Printf.eprintf "%s: couldn't open %s: %s\n%!"
                 Sys.argv.(0) name e;
               incr status
           end)
      !names;
  
    begin
      try
        while true do
          let line = input_line stdin in
          Hashtbl.iter
            (fun fh name ->
               try
                 output_string fh line;
                 output_string fh "\n";
                 if !unbuffer then flush fh
               with Sys_error e ->
                 Printf.eprintf "%s: couldn't write to %s: %s\n%!"
                   Sys.argv.(0) name e;
                 incr status)
            fhs
        done
      with End_of_file -> ()
    end;
  
    Hashtbl.iter
      (fun fh name ->
         let close =
           if name.[0] = '|'
           then fun p -> ignore (Unix.close_process_out p)
           else close_out in
         try close fh
         with Sys_error e ->
           Printf.eprintf "%s: couldn't close %s: %s\n%!"
             Sys.argv.(0) name e;
           incr status)
      fhs;
  
    exit !status
  

