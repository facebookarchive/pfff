(* ********************************************************************** *)
(* Testing for the Presence of a Key in a Hash *)
(* ********************************************************************** *)
let pleac_Testing_for_the_Presence_of_a_Key_in_a_Hash () = 
  (*-----------------------------*)
  (* does %HASH have a value for $KEY ?  *)
  if (Hashtbl.mem hash key) then
    (* it exists *)
  else
    (* id doesn't exists *)
   ;;
  (*-----------------------------*)
  (* food_color defined per the introduction *)
  List.iter (fun name ->
    let kind = if Hashtbl.mem food_color name then "food" else "drink" in
    printf "%s is a %s.\n" name kind
  ) ["Banana"; "Martini"] ;;
  (*
  > Banana is a food.
  > Martini is a drink.
  *)
  (*-----------------------------*)
  (* there's no such thing called "undef", "nil" or "null" in Caml
     if you really want such a value, use type "option" as shown below *)
  let age = assoc_list2hashtbl 
      [ "Toddler", 3 ; "Unborn", 0 ] ;;
  (*> val age : (string, int) Hashtbl.t = <abstr> *)
  
  List.iter (fun thing ->
    printf "%s: %s\n" thing
      (try match Hashtbl.find age thing with
      | 0 -> "Exists"
      | _ -> "Exists NonNull"
      with Not_found -> "")
  ) ["Toddler" ; "Unborn" ; "Phantasm" ; "Relic" ]
  
  let age = assoc_list2hashtbl 
      [ "Toddler", Some 3 ; "Unborn", Some 0 ; "Phantasm", None ] ;;
  (*> val age : (string, int option) Hashtbl.t = <abstr> *)
  
  List.iter (fun thing ->
    printf "%s: %s\n" thing
      (try match Hashtbl.find age thing with
      | None -> "Exists"
      | Some 0 -> "Exists Defined"
      | Some _ -> "Exists Defined NonNull"
      with Not_found -> "")
  ) ["Toddler" ; "Unborn" ; "Phantasm" ; "Relic" ]
  (*
  > Toddler: Exists Defined NonNull
  > Unborn: Exists Defined
  > Phantasm: Exists
  > Relic: 
  *)
  (*-----------------------------*)
  let size = Hashtbl.create 20 in
  List.iter (fun f -> 
    if not (Hashtbl.mem size f) then
      Hashtbl.replace size f (Unix.stat f).Unix.st_size;
  ) (readlines stdin);
  (*-----------------------------*)
  (* here is a more complete solution which does stat 2 times the same file (to
  be mimic perl's version) *)
  let size = Hashtbl.create 20 in
  List.iter (fun f -> 
    if not (Hashtbl.mem size f) then
      Hashtbl.replace size f (try Some (Unix.stat f).Unix.st_size with _ -> None)
  ) (readlines stdin);
  
  

