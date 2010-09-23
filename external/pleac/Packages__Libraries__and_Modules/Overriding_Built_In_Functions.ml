(* ********************************************************************** *)
(* Overriding Built-In Functions *)
(* ********************************************************************** *)
let pleac_Overriding_Built_In_Functions () = 
  #load "unix.cma";;
  
  (* The Unix module returns the time as a float. Using a local module
     definition and an "include", we can override this function to return
     an int32 instead. (This is a bit silly, but it illustrates the basic
     technique. *)
  module Unix = struct
    include Unix
    let time () = Int32.of_float (time ())
  end
  
  (* Use the locally modified Unix.time function. *)
  let () =
    let start = Unix.time () in
    while true do
      Printf.printf "%ld\n" (Int32.sub (Unix.time ()) start)
    done
  
  (* Operators can also be locally modified. Here, we'll temporarily
     define '-' as int32 subtraction. *)
  let () =
    let ( - ) = Int32.sub in
    let start = Unix.time () in
    while true do
      Printf.printf "%ld\n" (Unix.time () - start)
    done
  

