open Common

open Ast_ml
module Ast = Ast_ml
module V = Visitor_ml

open OUnit

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

let unittest =
  "parsing_ml" >::: [
    (* Check that the visitor implementation correctly visit all AST 
     * subelements, even when they are deep inside the AST tree (e.g. 
     * sub-sub expressions inside parenthesis).
     *)
    "visitor" >:: (fun () ->
      Common.with_tmp_file ~ext:".ml" ~str:
"open Foo1
module A = Foo2
"      (fun file ->
         let ast = Parse_ml.parse_program file in
         let cnt = ref 0 in
         let visitor = V.mk_visitor { V.default_visitor with
           V.kmodule_expr = (fun (k, _) x ->
             (match x with
             | ModuleName ((qu, name)) ->
                 incr cnt
             | _ -> ()
             );
             k x
           );
           V.kitem = (fun (k, _) x ->
             (match x with
             | Open (_tok, (qu, name)) ->
                 incr cnt;
             | _ -> ()
             );
             k x
           );
           V.kqualifier = (fun (k, _) xs ->
             (match xs with 
             | [Name (s, _), _tok] ->
                 pr2 s;
                 incr cnt;
             | _ ->
                 ()
             );
           );
         }
         in
         visitor (Program ast);
         assert_equal 2 !cnt;
         ()
      )
    );
  ]

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)
let actions () = [
    "-unittest_parsing_ml", "   ", 
    Common.mk_action_0_arg (fun () -> OUnit.run_test_tt unittest |> ignore);
]
