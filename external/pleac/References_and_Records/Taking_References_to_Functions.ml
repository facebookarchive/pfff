(* ********************************************************************** *)
(* Taking References to Functions *)
(* ********************************************************************** *)
let pleac_Taking_References_to_Functions () = 
  (* Create a reference to a function *)
  let fref = ref func
  let fref = ref (fun () -> (* ... *) ())
  
  (* Call the referent function *)
  let () = !fref ()
  
  (* Create a reference to an association list with function values. *)
  let commands = ref []
  let () =
    let ( => ) name func = commands := (name, func) :: !commands in
    (
      "happy" => joy;
      "sad"   => sullen;
      "done"  => (fun () -> print_endline "See ya!"; exit 0);
      "mad"   => angry;
    )
  
  let () =
    while true do
      print_string "How are you? ";
      let string = read_line () in
      try
        let command = List.assoc string !commands in
        command ()
      with Not_found ->
        Printf.printf "No such command: %s\n" string
    done
  
  (* Use closures to generate functions that count. *)
  
  let counter_maker () =
    let start = ref 0 in
    fun () ->                             (* this is a closure *)
      let result = !start in              (* lexical from enclosing scope *)
      incr start; result
  
  let counter1 = counter_maker ()
  let counter2 = counter_maker ()
  
  let () =
    for i = 0 to 4 do
      Printf.printf "%d\n" (counter1 ())
    done;
    Printf.printf "%d %d\n" (counter1 ()) (counter2 ())
    (*
      0
      1
      2
      3
      4
      5 0
    *)
  
  (* Use closures to generate functions that keep track of time.
     Note that this example does not need references, since
     since functions are just ordinary values in OCaml. *)
  
  #load "unix.cma";;
  
  let timestamp () =
    let start_time = Unix.time () in
    fun () -> int_of_float (Unix.time () -. start_time)
  
  let () =
    let early = timestamp () in
    Unix.sleep 20;
    let later = timestamp () in
    Unix.sleep 10;
    Printf.printf "It's been %d seconds since early.\n" (early ());
    Printf.printf "It's been %d seconds since later.\n" (later ());
    (*
      It's been 30 seconds since early.
      It's been 10 seconds since later.
    *)
  

