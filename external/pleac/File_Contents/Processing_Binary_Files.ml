(* ********************************************************************** *)
(* Processing Binary Files *)
(* ********************************************************************** *)
let pleac_Processing_Binary_Files () = 
  set_binary_mode_in in_channel true
  set_binary_mode_out out_channel true
  
  (*-----------------------------*)
  
  let () =
    let gifname = "picture.gif" in
    let gif = open_in gifname in
    set_binary_mode_in gif true;
    (* now DOS won't mangle binary input from "gif" *)
    set_binary_mode_out stdout true;
    (* now DOS won't mangle binary output to "stdout" *)
    let buff = String.make 8192 '\000' in
    let len = ref (-1) in
    while !len <> 0 do
      len := input gif buff 0 8192;
      output stdout buff 0 !len
    done
  

