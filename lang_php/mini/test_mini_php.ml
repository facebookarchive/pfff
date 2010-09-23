open Common

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_sexp_mini_php file = 
  let asts = Parse_mini_php.parse file in
  (* let _ast = Type_annoter.annotate_program !Type_annoter.initial_env ast *) 
  let s = Sexp_mini_php.string_of_program asts in
  pr2 s;
  ()


let test_typing_mini_php file = 
  let asts = Parse_mini_php.parse file in
  asts +> List.iter Typing_mini_php.typing;
  ()

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
  "-sexp_mini", "   <file>", 
    Common.mk_action_1_arg test_sexp_mini_php;

  (* just an alias for -sexp_php *)
  "-dump_mini", "   <file>", 
    Common.mk_action_1_arg test_sexp_mini_php;

  (* just an alias for -sexp_php *)
  "-typing_mini", "   <file>", 
    Common.mk_action_1_arg test_typing_mini_php;

]
