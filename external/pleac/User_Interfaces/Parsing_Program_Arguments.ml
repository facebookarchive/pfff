(* ********************************************************************** *)
(* Parsing Program Arguments *)
(* ********************************************************************** *)
let pleac_Parsing_Program_Arguments () = 
  let verbose = ref false
  let debug = ref false
  let output = ref ""
  
  let () =
    Arg.parse
      [
        "-v", Arg.Set verbose, "Verbose mode";
        "-D", Arg.Set debug, "Debug mode";
        "-o", Arg.Set_string output, "Specify output file";
      ]
      (fun s ->
         raise (Arg.Bad (Printf.sprintf "unexpected argument `%s'" s)))
      (Printf.sprintf "Usage: %s [-v] [-d] [-o file]" Sys.argv.(0))
  
  let () =
    if !verbose then print_endline "Verbose mode";
    if !debug then print_endline "Debug mode";
    if !output <> "" then print_endline ("Writing output to " ^ !output);
  

