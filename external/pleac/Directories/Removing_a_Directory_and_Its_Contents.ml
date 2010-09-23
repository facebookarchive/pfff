(* ********************************************************************** *)
(* Removing a Directory and Its Contents *)
(* ********************************************************************** *)
let pleac_Removing_a_Directory_and_Its_Contents () = 
  (* rmtree - remove whole directory trees like rm -r *)
  #load "unix.cma";;
  
  let rec finddepth f roots =
    Array.iter
      (fun root ->
         (match Unix.lstat root with
            | {Unix.st_kind=Unix.S_DIR} ->
                finddepth f
                  (Array.map (Filename.concat root) (Sys.readdir root))
            | _ -> ());
         f root)
      roots
  
  let zap path =
    match Unix.lstat path with
      | {Unix.st_kind=Unix.S_DIR} ->
          Printf.printf "rmdir %s\n%!" path;
          Unix.rmdir path
      | _ ->
          Printf.printf "unlink %s\n%!" path;
          Unix.unlink path
  
  let () =
    if Array.length Sys.argv < 2
    then (Printf.eprintf "usage: %s dir ..\n" Sys.argv.(0); exit 1);
    finddepth zap (Array.sub Sys.argv 1 (Array.length Sys.argv - 1))
  

