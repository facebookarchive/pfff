(* ********************************************************************** *)
(* Controlling Input and Output of Another Program *)
(* ********************************************************************** *)
let pleac_Controlling_Input_and_Output_of_Another_Program () = 
  #load "unix.cma";;
  
  let () =
    let (readme, writeme) = Unix.open_process program in
    output_string writeme "here's your input\n";
    close_out writeme;
    let output = input_line readme in
    ignore (Unix.close_process (readme, writeme))
  

