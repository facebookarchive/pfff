(* ********************************************************************** *)
(* Using Random-Access I/O *)
(* ********************************************************************** *)
let pleac_Using_Random_Access_I_O () = 
  let () =
    let address = recsize * recno in
    seek_in fh address;
    really_input fh buffer 0 recsize
  
  (*-----------------------------*)
  
  let () =
    let address = recsize * (recno - 1) in
    (* ... *)
    ()
  

