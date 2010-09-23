(* ********************************************************************** *)
(* Printing Data Structures *)
(* ********************************************************************** *)
let pleac_Printing_Data_Structures () = 
  (* If you are in the OCaml toplevel, simply enter an expression to
     view its type and value. *)
  # let reference = ref ( [ "foo", "bar" ],
                          3,
                          fun () -> print_endline "hello, world" );;
  val reference : ((string * string) list * int * (unit -> unit)) ref =
    {contents = ([("foo", "bar")], 3, <fun>)}
  
  (* From within your own programs, use the Std.print and Std.dump
     functions from the Extlib library, available at
     http://ocaml-lib.sourceforge.net/ *)
  # Std.print reference;;
  (([("foo", "bar")], 3, <closure>))
  - : unit = ()
  # Std.dump reference;;
  - : string = "(([(\"foo\", \"bar\")], 3, <closure>))"
  

