(* ********************************************************************** *)
(* Converting HTML to ASCII *)
(* ********************************************************************** *)
let pleac_Converting_HTML_to_ASCII () = 
  #load "unix.cma";;
  
  let slurp_channel channel =
    let buffer_size = 4096 in
    let buffer = Buffer.create buffer_size in
    let string = String.create buffer_size in
    let chars_read = ref 1 in
    while !chars_read <> 0 do
      chars_read := input channel string 0 buffer_size;
      Buffer.add_substring buffer string 0 !chars_read
    done;
    Buffer.contents buffer
  
  let () =
    let process = Unix.open_process_in ("lynx -dump " ^ filename) in
    let ascii = slurp_channel process in
    ignore (Unix.close_process_in process);
    (* ... *)
  

