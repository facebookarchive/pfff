(* ********************************************************************** *)
(* Executing Commands Without Shell Escapes *)
(* ********************************************************************** *)
let pleac_Executing_Commands_Without_Shell_Escapes () = 
  (* UNSAFE *)
  let status =
    Unix.system
      (command ^ " " ^ input ^ " " ^ String.concat " " files)
  
  (* safer *)
  let pid =
    Unix.create_process command (Array.of_list ([command; input] @ files))
      Unix.stdin Unix.stdout Unix.stderr
  let _, status = Unix.waitpid [] pid
  

