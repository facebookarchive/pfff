(* ********************************************************************** *)
(* Implementing a Circular List *)
(* ********************************************************************** *)
let pleac_Implementing_a_Circular_List () = 
  
  (* To get a true circular list, one can use the let rec construct *)
  
  let rec processes = 1::2::3::4::5::processes;;
  while true do
    let process::processes = process in
    printf "Handling process %d\n" process;
    Unix.sleep 2;
  done;;
  
  (* or one can use these somewhat inefficient functions to simulate the Perl
   * examples *)
  
  let popleft l =
    match l with
      [] -> raise Not_found
    | h::t -> h,(t @ [h]);;
  
  let popright l =
    match List.rev l with
      [] -> raise Not_found
    | h::t -> h,(h::(List.rev t));;
  
  let processes = ref [1;2;3;4;5];;
  while true do
    let process,np = popleft !processes in
    processes := np;
    printf "Handling process %d\n" process;
    flush_all ();
    Unix.sleep 1;
  done;;
  

