(* ********************************************************************** *)
(* Reporting Errors and Warnings Like Built-Ins *)
(* ********************************************************************** *)
let pleac_Reporting_Errors_and_Warnings_Like_Built_Ins () = 
  (* There are two built-in functions that raise standard exceptions.
     Many standard library functions use these. "invalid_arg" raises
     an Invalid_argument exception, which takes a string parameter: *)
  let even_only n =
    if n land 1 <> 0 (* one way to test *)
    then invalid_arg (string_of_int n);
    (* ... *)
    ()
  
  (* "failwith" raises a Failure exception, which also takes a string
     parameter (though it is typically used to identify the name of
     the function as opposed to the argument). *)
  let even_only n =
    if n mod 2 <> 0 (* here's another *)
    then failwith "even_only";
    (* ... *)
    ()
  
  (* In most cases, it is preferable to define your own exceptions. *)
  exception Not_even of int
  let even_only n =
    if n land 1 <> 0 then raise (Not_even n);
    (* ... *)
    ()
  
  (* OCaml does not provide a facility for emitting warnings. You can
     write to stderr, which may be an acceptable substitute. *)
  let even_only n =
    let n =
      if n land 1 <> 0 (* test whether odd number *)
      then (Printf.eprintf "%d is not even, continuing\n%!" n; n + 1)
      else n in
    (* ... *)
    ()
  

