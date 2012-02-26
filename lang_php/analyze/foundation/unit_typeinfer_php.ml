open Common

open OUnit

module Ast = Ast_php_simple
module Env = Env_typing_php
module Infer  = Typing_php
module InferH = Typing_helpers_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let normalize t =
  let env = Env_typing_php.make_env () in
  InferH.Normalize.normalize env t

let to_string env t =
  let buf = Buffer.create 256 in
  let o   = Buffer.add_string buf in
  let ()  = InferH.Print.show_type env o t in
  Buffer.contents buf

let rec last = function
  | [x] -> x 
  | _ :: rl -> last rl 
  | _ -> assert false

let get_signature fun_ =
  let source_file = Parse_php.tmp_php_file_from_string fun_ in
  let env = Env_typing_php.make_env () in
  let env = { env with Env_typing_php.verbose = false } in
  Infer.Builtins.make env;
  let ast = Parse_php.parse_program source_file in
  let ast = Ast_php_simple_build.program ast in
  Infer.decls env ast;
  match last ast with
  | Ast.FuncDef fd ->
      Infer.func_def env fd;
      normalize (InferH.GEnv.get_fun env (Ast.unwrap fd.Ast.f_name))
  | Ast.ClassDef cd ->
      Infer.class_def env cd;
      normalize (InferH.GEnv.get_class env (Ast.unwrap cd.Ast.c_name))
  | _ -> assert false

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)
let unittest =
  let n = normalize in
  "type_inference" >::: (
    [

    "basic" >:: (fun () ->
      let t sig_ def = assert_equal sig_ (get_signature def) in
      t (n Env.(fun_ [int] int)) "function f($x) { return $x + 1; }";
      t (n Env.(fun_ [bool] bool)) "function f($x) { return $x === true; }";
    );
    "trait" >:: (fun () ->
      let file = "
trait T1 { public function foo() { return 0; } }
trait T2 { public function bar() { return 0; } }
class A { use T1, T2; }
function f() { $a = new A(); return $a->foo(); }
    " in
    assert_equal (n Env.(fun_ [] int)) (get_signature file)
    );
  ]
  )

