(* ********************************************************************** *)
(* Copying or Moving a File *)
(* ********************************************************************** *)
let pleac_Copying_or_Moving_a_File () = 
  (*-----------------------------*)
  
  (* Note : this doesn't use the unix library, only the standard one *)
  
  let copy oldfile newfile =
    let infile = open_in oldfile
    and outfile = open_out newfile
    and blksize = 16384 in
    let buf = String.create blksize in
    let rec real_copy () =
      let byte_read = input infile buf 0 blksize in
      if byte_read <> 0 then
        begin
  	(* Handle partialle write : nothing to do *)
  	output outfile buf 0 byte_read;
  	real_copy ()
        end in
    real_copy ();
    close_in infile;
    close_out outfile;;
  
  (*-----------------------------*)
  Sys.command ("cp " ^ oldfile ^ " " ^ newfile)	(* Unix *)
  Sys.command (String.concat " " ["copy";oldfile;newfile]) (* Dos *)
  
  (*-----------------------------*)
  
  Unix.copy "datafile.dat" "datafile.bak";;
  
  Sys.rename "datafile.dat" "datafile.bak";;
  
  (*-----------------------------*)
  

