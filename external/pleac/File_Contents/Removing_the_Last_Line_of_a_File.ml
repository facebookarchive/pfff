(* ********************************************************************** *)
(* Removing the Last Line of a File *)
(* ********************************************************************** *)
let pleac_Removing_the_Last_Line_of_a_File () = 
  #load "unix.cma";;
  
  let () =
    let descr = Unix.openfile file [Unix.O_RDWR] 0o666 in
    let in_channel = Unix.in_channel_of_descr descr in
    let position = ref 0 in
    let last_position = ref 0 in
    begin
      try
        while true do
          ignore (input_line in_channel);
          last_position := !position;
          position := pos_in in_channel;
        done
      with End_of_file -> ()
    end;
    Unix.ftruncate descr !last_position;
    Unix.close descr
  

