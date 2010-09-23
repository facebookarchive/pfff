(* ********************************************************************** *)
(* Hashing References *)
(* ********************************************************************** *)
let pleac_Hashing_References () = 
  (*-----------------------------*)
  open Unix;;
  open Printf;;
  
  let filenames = ["/etc/printcap"; "/vmlinuz"; "/bin/cat"] in
  let openfiles = Hashtbl.create 3 in
  print_newline();
  List.iter
    (fun fname ->
      printf "%s is %d bytes long.\n"
        fname
        (stat fname).st_size
    )
    filenames
  ;;
    
  (*-----------------------------*)

