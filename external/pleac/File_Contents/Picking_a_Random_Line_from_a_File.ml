(* ********************************************************************** *)
(* Picking a Random Line from a File *)
(* ********************************************************************** *)
let pleac_Picking_a_Random_Line_from_a_File () = 
  let () =
    Random.self_init ();
    let count = ref 1 in
    let line = ref "" in
    try
      while true do
        let next = input_line stdin in
        if Random.int !count < 1 then line := next;
        incr count
      done
    with End_of_file ->
      (* !line is the random line *)
      ()
  

