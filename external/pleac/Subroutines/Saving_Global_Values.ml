(* ********************************************************************** *)
(* Saving Global Values *)
(* ********************************************************************** *)
let pleac_Saving_Global_Values () = 
  (* To do this in OCaml -- which doesn't like global state in the first place --
   * you need to manually store the old value and replace it before exiting the
   * block *)
  
  let age = ref 18;;
  if condition then
    (
      let org_age = !age in
      age := 23;
      func ();
      age := org_age
    );;
  
  (* for local handles, just create a new channel inside your block *)
  let get_motd () =
    let motd = open_in "/etc/motd" in
    let retval = 
      ... in
    close_in motd;
    retval;;
  

