open Common

open Ast_js
module Ast = Ast_js
module V = Visitor_js
module M = Map_js

open OUnit

module Flag = Flag_parsing_js

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

let unittest =
  "parsing_js" >::: [

    "the javascript AST mapper" >:: (fun () ->
      let js_ex = "foo(42, 101);" in
      Common.with_tmp_file ~str:js_ex ~ext:".js" (fun file ->
        let prog = Parse_js.parse_program file in
        let map_visitor = M.mk_visitor { M.default_visitor with
          M.kexpr = (fun (k, _) x ->
            match x with
            | L (Num (s, tok)), typ ->
                let i = s_to_i s in
                L (Num (i_to_s (i+1), Ast.fakeInfo())), typ
            | _ -> k x
          );
        }
        in
        let transformed_ast = map_visitor (Program prog) in

        let integers = 
          V.do_visit_with_ref (fun aref -> { V.default_visitor with
            V.kexpr = (fun (k, _) x ->
              match Ast.untype x with
              | L (Num (s, tok)) -> 
                  Common.push2 (s_to_i s) aref
              | _ -> k x
            );
          }) transformed_ast in
        assert_equal
          ~msg:"it should increment all the integers in the program"
          [43; 102] integers;
      )
    );
  ]


(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)
let actions () = [
    "-unittest_parsing_js", "   ", 
    Common.mk_action_0_arg (fun () -> OUnit.run_test_tt unittest |> ignore);
]
