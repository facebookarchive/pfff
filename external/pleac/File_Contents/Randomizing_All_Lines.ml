(* ********************************************************************** *)
(* Randomizing All Lines *)
(* ********************************************************************** *)
let pleac_Randomizing_All_Lines () = 
  (* assumes the fisher_yates_shuffle function from Chapter 4 *)
  let shuffle list =
    let array = Array.of_list list in
    fisher_yates_shuffle array;
    Array.to_list array
  
  let () =
    Random.self_init ();
    let lines = ref [] in
    (try
       while true do
         lines := (input_line input) :: !lines
       done
     with End_of_file -> ());
    let reordered = shuffle !lines in
    List.iter
      (fun line ->
         output_string output line;
         output_char output '\n')
      reordered
  

