(* ********************************************************************** *)
(* Reading a File Backwards by Line or Paragraph *)
(* ********************************************************************** *)
let pleac_Reading_a_File_Backwards_by_Line_or_Paragraph () = 
  let lines = ref []
  let () =
    try
      while true do
        lines := input_line chan :: !lines
      done
    with End_of_file -> ()
  let () =
    List.iter
      (fun line ->
         (* do something with line *)
         ())
      !lines
  

