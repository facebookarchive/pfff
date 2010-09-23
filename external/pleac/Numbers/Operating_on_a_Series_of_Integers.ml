(* ********************************************************************** *)
(* Operating on a Series of Integers *)
(* ********************************************************************** *)
let pleac_Operating_on_a_Series_of_Integers () = 
  (*-----------------------------*)
  (* The boring way is to use a for loop... *)
  for i = low to high do
    (* Do your stuff *)
    (* Note, if what you want to do in the loop does not have have type unit, you
       need to wrap it with ignore, e.g. ignore (2 * i) *)
  done
  
  (* Or you skip the syntactic sugar and write it recursively yourself *)
  let rec loop low high f =
    if low > high then
      ()
    else
      begin
        ignore (f low);
        loop (succ low) high f
      end;;
  
  (* and now with stepsize different from 1 *)
  let rec loopStep low high step f =
    if low > high then
      ()
    else
      begin
        ignore (f low);
        loopStep (low + step) high f
      end;;
  
      
  (* Or, if you don't mind wasting space, you can use the useful iter functions
   *)
  (* Array based *)
  let makeArraySequence lo hi =
    Array.init (hi - lo + 1) (fun i -> i + lo);;
  Array.iter ( your function here ) (makeArraySequence lo hi);;
    
  (* List based *)
  let makeListSequence lo hi = 
    let rec msHelper lo hi l =
      match (a - b) with
      0 -> b::l
  	| _ -> msHelper a (b-1) (b::l) in
    msHelper lo hi [];;
  List.iter ( your function here ) (makeListSequence lo hi);;
  (*-----------------------------*)
  printf "Infancy is: ";
  for i = 0 to 2 do
    printf "%d " i
  done;;
  
  print_newline();;
  
  printf "Toddling is: ";
  loop 3 4 (fun i -> printf "%d " i);;
  
  print_newline ();;
  
  printf "Childhood is: ";
  Array.iter (fun i -> printf "%d " i) (makeArraySequence 5 12);;
  
  print_newline();;
  
  (*
   * Infancy is: 0 1 2
   * Toddling is: 3 4
   * Childhood is: 5 6 7 8 9 10 11 12
   *) 
  (*-----------------------------*)
  

