(* ********************************************************************** *)
(* Controlling Another Program with Expect *)
(* ********************************************************************** *)
let pleac_Controlling_Another_Program_with_Expect () = 
  (* Use perl4caml to integrate OCaml with Perl:
     http://merjis.com/developers/perl4caml *)
  #directory "+perl";;
  #load "perl4caml.cma";;
  
  (* Wrap the needed functionality from CPAN's Expect module: *)
  module Expect = struct
    open Perl
    let _ = eval "use Expect"
  
    exception Error of string
  
    type match_pattern = Ex of string | Re of string
  
    class expect () = object (self)
      val sv = call_class_method "Expect" "new" []
  
      method log_stdout =
        bool_of_sv (call_method sv "log_stdout" [])
  
      method set_log_stdout bool =
        ignore (call_method sv "log_stdout" [sv_of_bool bool])
  
      method spawn command parameters =
        let result =
          call_method sv "spawn"
            (sv_of_string command :: List.map sv_of_string parameters) in
        if not (bool_of_sv result)
        then raise (Error (string_of_sv (eval "$!")))
  
      method expect timeout match_patterns =
        let svs_of_pattern = function
          | Ex s -> [sv_of_string "-ex"; sv_of_string s]
          | Re s -> [sv_of_string "-re"; sv_of_string s] in
        let timeout =
          match timeout with
            | Some i -> sv_of_int i
            | None -> sv_undef () in
        let result =
          call_method sv "expect"
            (timeout ::
               List.flatten (List.map svs_of_pattern match_patterns)) in
        if sv_is_undef result
        then None
        else Some (int_of_sv result - 1)
  
      method send string =
        ignore (call_method sv "send" [sv_of_string string])
  
      method soft_close () =
        ignore (call_method sv "soft_close" [])
  
      method hard_close () =
        ignore (call_method sv "hard_close" [])
    end
  
    let spawn command parameters =
      let exp = new expect () in
      exp#spawn command parameters;
      exp
  end
  
  (* start the program *)
  let command =
    try Expect.spawn "program to run" ["arg 1"; "arg 2"]
    with Expect.Error e ->
      Printf.eprintf "Couldn't start program: %s\n%!" e;
      exit 1
  
  let () =
    (* prevent the program's output from being shown on our stdout *)
    command#set_log_stdout false;
  
    (* wait 10 seconds for "login:" to appear *)
    if command#expect (Some 10) [Expect.Ex "login"] = None
    then failwith "timed out";
  
    (* wait 20 seconds for something that matches /[Pp]assword: ?/ *)
    if command#expect (Some 20) [Expect.Re "[Pp]assword: ?"] = None
    then failwith "timed out";
  
    (* wait forever for "invalid" to appear *)
    if command#expect None [Expect.Ex "invalid"] = None
    then failwith "error occurred; the program probably went away";
  
    (* send "Hello, world" and a carriage return to the program *)
    command#send "Hello, world\r";
  
    (* if the program will terminate by itself, finish up with *)
    command#soft_close ();
  
    (* if the program must be explicitly killed, finish up with *)
    command#hard_close ()
  
  (* wait for multiple strings *)
  let () =
    match command#expect (Some 30)
      [Expect.Ex "invalid"; Expect.Ex "succes";
       Expect.Ex "error"; Expect.Ex "boom"] with
        | Some which ->
            (* found one of those strings *)
            ()
        | None ->
            ()
  

