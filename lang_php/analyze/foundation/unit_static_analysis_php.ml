
open Common

open OUnit

open Env_interpreter_php
module Env = Env_interpreter_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* dup: with unit_analyze_php.ml *)
let tmp_php_file_from_string s =
  let tmp_file = Common.new_temp_file "test" ".php" in
  Common.write_file ~file:tmp_file ("<?php\n" ^ s);
  tmp_file

let heap_of_program_at_checkpoint content =
  let tmp_file = tmp_php_file_from_string content in
  let ast = Parse_php.parse_program tmp_file in
  let ast = Ast_php_simple_build.program ast in
  let heap = Env_interpreter_php.empty_heap in
  let juju_db = Env_interpreter_php.juju_db_of_files [tmp_file] in
  let db = Env_interpreter_php.code_database_of_juju_db juju_db in
  let env = Env_interpreter_php.empty_env db tmp_file in
  Abstract_interpreter_php.extract_paths := false;
  let _heap = Abstract_interpreter_php.program env heap ast in
  match !Abstract_interpreter_php._checkpoint_heap with
  | None -> failwith "use checkpoint() in your unit test"
  | Some x -> x

let rec chain_ptrs heap v =
  match v with
  | Vptr n ->
      Vptr n::(chain_ptrs heap (IMap.find n heap.ptrs))
  | Vref aset ->
      let n = ISet.choose aset in
      Vref aset::(chain_ptrs heap (Vptr n))
  | x -> [x]

let value_of_var s vars heap =
  let v = SMap.find s vars in
  match v with
  | Vptr n ->
      chain_ptrs heap v
  | _ -> assert_failure "variable is not a Vptr"

let info heap v = Env.string_of_value heap (List.hd v)

(*****************************************************************************)
(* Abstract interpreter *)
(*****************************************************************************)
let abstract_interpreter_unittest =
  "abstract interpreter" >::: [

    "basic" >:: (fun () ->
      let file ="
$x = 42;
checkpoint(); // x:42
" in
      let (heap, vars) = heap_of_program_at_checkpoint file in
      match value_of_var "$x" vars heap with
      (* variables in PHP are pointers to a pointer to a value ... *)
      | [Vptr n1; Vptr n2; Vint 42] -> ()
      | v -> assert_failure ("wrong value for $x: " ^ info heap v)
    );

    "aliasing" >:: (fun () ->
      let file ="
$x = 42;
$y =& $x;
checkpoint();
" in
      let (heap, vars) = heap_of_program_at_checkpoint file in
      let x = value_of_var "$x" vars heap in
      let y = value_of_var "$y" vars heap in
      match x, y with
      | [Vptr ix1; Vref _set; Vptr ix2; Vint 42],
        [Vptr iy1; Vref _set2; Vptr iy2; Vint 42]
        ->
          assert_equal ix2 iy2;
          assert_bool "variables should have different original pointers"
            (ix1 <> iy1)

      | _ -> assert_failure (spf "wrong value for $x: %s, $y = %s "
                               (info heap x) (info heap y))
    );

    "abstraction when if" >:: (fun () ->
      let file ="
$x = 1;
$y = true; // TODO? could statically detect it's always $x = 2;
if($y) { $x = 2;} else { $x = 3; }
checkpoint(); // x: int
" in
      let (heap, vars) = heap_of_program_at_checkpoint file in
      match value_of_var "$x" vars heap with
      | [Vptr n1; Vptr n2; Vabstr Tint] -> ()
      | v -> assert_failure ("wrong value for $x: " ^ info heap v)
    );

    "union types" >:: (fun () ->
      let file ="
$x = null;
$y = true;
if($y) { $x = 2;} else { $x = 3; }
checkpoint(); // x: null | int
" in
      let (heap, vars) = heap_of_program_at_checkpoint file in
      match value_of_var "$x" vars heap with
      | [Vptr n1; Vptr n2; Vsum [Vnull; Vabstr Tint]] -> ()
      | v -> assert_failure ("wrong value for $x: " ^ info heap v)
    );

    "simple dataflow" >:: (fun () ->
      let file ="
$x = 2;
$x = 3;
$y = $x;
checkpoint(); // y:int
" in
      let (heap, vars) = heap_of_program_at_checkpoint file in
      match value_of_var "$x" vars heap with
      | [Vptr n1; Vptr n2; Vabstr Tint] -> ()
      | v -> assert_failure ("wrong value for $y: " ^ info heap v)
    );

    "interprocedural dataflow" >:: (fun () ->
      let file ="
$x = 2;
function foo($a) { return $a + 1; }
$y = foo($x);
checkpoint(); // y: int
" in
      let (heap, vars) = heap_of_program_at_checkpoint file in
      match value_of_var "$y" vars heap with
      | [Vptr n1; Vptr n2; Vabstr Tint] -> ()
      | v -> assert_failure ("wrong value for $y: " ^ info heap v)
    );

    "semantic lookup static method" >:: (fun () ->

      let file ="
$x = 2;
class A { static function foo($a) { return $a + 1; } }
class B extends A { }
$y = B::foo($x);
checkpoint(); // y::int
" in
      let (heap, vars) = heap_of_program_at_checkpoint file in
      match value_of_var "$y" vars heap with
      | [Vptr n1; Vptr n2; Vabstr Tint] -> ()
      | v -> assert_failure ("wrong value for $y: " ^ info heap v)
    );

    "semantic lookup self in parent" >:: (fun () ->

      let file ="
class A {
  static function foo() { return self::bar(); }
  static function bar() { return 1+1; }
}

class B extends A {
 static function bar() { return false || false; }
  }
$x = B::foo();
$y = B::bar();
checkpoint(); // x: int, y: bool
" in
      let (heap, vars) = heap_of_program_at_checkpoint file in
      (match value_of_var "$x" vars heap with
      | [Vptr n1; Vptr n2; Vabstr Tint] -> ()
      | v -> assert_failure ("wrong value for $x: " ^ info heap v)
      );
      (match value_of_var "$y" vars heap with
      | [Vptr n1; Vptr n2; Vabstr Tbool] -> ()
      | v -> assert_failure ("wrong value for $y: " ^ info heap v)
      );
    );

    "semantic lookup method" >:: (fun () ->

      let file ="
$x = 2;
class A { function foo($a) { return $a + 1; } }
class B extends A { }
$o = new B();
$y = $o->foo($x);
checkpoint(); // y: int
" in
      let (heap, vars) = heap_of_program_at_checkpoint file in
      match value_of_var "$y" vars heap with
      | [Vptr n1; Vptr n2; Vabstr Tint] -> ()
      | v -> assert_failure ("wrong value for $y: " ^ info heap v)
    );
  ]

(*****************************************************************************)
(* Tainting analysis *)
(*****************************************************************************)

(*****************************************************************************)
(* Type inference *)
(*****************************************************************************)

(*****************************************************************************)
(* Final suite *)
(*****************************************************************************)
let unittest =
  "static_analysis_php" >::: [
    abstract_interpreter_unittest;
  ]

(*****************************************************************************)
(* Main entry for args *)
(*****************************************************************************)
