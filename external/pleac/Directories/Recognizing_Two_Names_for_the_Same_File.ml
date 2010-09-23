(* ********************************************************************** *)
(* Recognizing Two Names for the Same File *)
(* ********************************************************************** *)
let pleac_Recognizing_Two_Names_for_the_Same_File () = 
  #load "unix.cma";;
  
  (* Count the number of times a (dev, ino) pair is seen. *)
  let seen = Hashtbl.create 0
  let do_my_thing filename =
    let {Unix.st_dev=dev; st_ino=ino} = Unix.stat filename in
    Hashtbl.replace seen (dev, ino)
      (try Hashtbl.find seen (dev, ino) + 1
       with Not_found -> 1);
    if Hashtbl.find seen (dev, ino) = 1
    then
      begin
        (* do something with filename because we haven't
           seen it before. *)
      end
  
  (*-----------------------------*)
  
  (* Maintain a list of files for each (dev, ino) pair. *)
  let seen = Hashtbl.create 0
  let () =
    List.iter
      (fun filename ->
         let {Unix.st_dev=dev; st_ino=ino} = Unix.stat filename in
         Hashtbl.replace seen (dev, ino)
           (try filename :: Hashtbl.find seen (dev, ino)
            with Not_found -> [filename]))
      files
  let () =
    Hashtbl.iter
      (fun (dev, ino) filenames ->
         Printf.printf "(%d, %d) => [%s]\n"
           dev ino (String.concat ", " filenames))
      seen
  

