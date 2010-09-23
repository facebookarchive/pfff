(* ********************************************************************** *)
(* Trailing a Growing File *)
(* ********************************************************************** *)
let pleac_Trailing_a_Growing_File () = 
  #load "unix.cma";;
  
  let sometime = 1
  
  let () =
    let chan = open_in file in
    while Sys.file_exists file do
      (try
         let line = input_line chan in
         (* ... *)
         ()
       with End_of_file ->
         Unix.sleep sometime)
    done;
    close_in chan
  

