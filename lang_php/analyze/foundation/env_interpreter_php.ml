(* Julien Verlaguet, Yoann Padioleau
 *
 * Copyright (C) 2011, 2012 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
 * license.txt for more details.
 *)
open Common

module Ast = Ast_php_simple

module ISet = Set.Make (Int)
module IMap = Map.Make (Int)
module SSet = Set.Make (String)
module SMap = Map.Make (String)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Main types and data structures used by the PHP abstract interpreter:
 * The "environment" and "heap".
 * 
 * In the abstract interpreter, all variables are pointers to pointers
 * of values. So with '$x = 42;' we got $x = &2{&1{42}}.
 * In 'env.vars' we got "$x" = Vptr 2
 * and in the 'heap' we then got [2 -> Vptr 1; 1 -> Vint 42]
 * meaning that $x is a variable with address 2, where the content
 * of this cell is a pointer to address 1, where the content of
 * this cell is the value 42. This is consistent with how Zend
 * manages values and variables at runtime (the "zval").
 * 
 * References:
 *  - todo: there was a doc about zvalues
 *  - todo: POPL paper on PHP copy-on-write semantic?
 *
 * Why this model? Why not having variables contain directly values,
 * that is $x = {42} without any Vptr? Because dealing with lvalues in the
 * interpreter would then be tedious. For instance with '$x = array(1,2,3);'
 * How would you handle '$x[0] = 1;' without any Vptr and a heap?
 * You would need to use ocaml references to mimic those Vptr.
 * 
 * Why not just variables be pointer to values then? Because
 * of references. With this code:
 * 
 *   $x = 2;
 *   var_dump($x);
 *   $y =& $x;
 *   var_dump($x);
 * 
 * We will have at the first var_dump: $x = &2{&1{2}}
 * and at the second var_dump: $x = &2{&REF 1{2}}, $y = &4{&REF 1{2}}
 * See the code of assign in the interpreter for more information.
 * 
 * 
 * Retrospecively, was it good to try to manage references correctly?
 * After all, many other things are not that well handled in the interpreter
 * or imprecise or passed-over silently, e.g. many functions/classes not
 * found, traits not handled, other constructs not handled, choosing
 * arbitrarily one object in a Vsum when there could actually be many
 * different classes, not really doing a fixpoint on loops, etc.
 * 
 * juju: yes, maybe it was not a good idea to focus so much on references.
 *  Moreover it slows down things a lot.
 * 
 * One advantage though of managing references correctly is that it forces
 * to really understand how Zend PHP internally works (zvalues).
 * Note that HPHP may do certain optimizations so those pointers of
 * pointers may actually not exist for many local variables when
 * we statically know they are never "referenced".
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* In practice a huge codebase contains multiple times the same
 * function name or class, but here we will just return one
 * of those possible entities :( At least we detect and warn 
 * about dupes when building the code database (see database_juju_php.ml).
 *)
type code_database = {
  funs:      string -> Ast.func_def;
  classes:   string -> Ast.class_def;
  constants: string -> Ast.constant_def;
}

type value =
  (* Precise value. Especially useful for Vstring for interprocedural
   * analysis as people use strings to represent functions or classnames
   * (they are not first-class citizens in PHP). Maybe also useful
   * for Vint when use ints for array indices and we want the precise
   * value in the array.
   *)
  | Vbool   of bool
  | Vint    of int
  | Vfloat  of float
  | Vstring of string

  (* We converge quickly to a very abstract value. Values are either
   * a single precise value (e.g. 42), or a type (e.g. int). No
   * intermediate range for instance.
   *)
  | Vabstr  of type_

  (* could be useful for nullpointer analysis *)
  | Vnull

  (* a pointer is an int address in the heap *)
  | Vptr of int
  (* todo: extract Vptr out of 'value', to have clear different types.
   * | Vptr1    of ptr1
   * | Vptr2    of ptr2
   * where ptr1 and ptr2 are abstract types, with a module with a
   * set_ptr1 taking only a value, and set_ptr2 take only a Vptr
   *)
  (* pad: because of some imprecision, we actually have a set of addresses ? *)
  | Vref    of ISet.t

  (* try to differentiate the different (abusive) usage of PHP arrays *)
  | Vrecord of value SMap.t
  | Varray  of value list
  | Vmap of value * value

  (* pad: The integer key of the IMap below is a unique identifier for a 
   * method (the name is not enough). The first 'value' is for '$this'. The
   * Imap is a set of methods because when we unify objects,
   * we merge methods and remember all possible values for this method.
   * See sum_call in the interpreter and call_methods.
   * 
   * What is the value of $x given: 
   *   class A { public function foo() { } }
   *   $x = new A();
   * It should be:
   * $x = &2{REF 1{Vobject (["foo" -> Vmethod (&2, [0x42-> (<foo closure>)])])}}
   *)
  | Vmethod of value * (env -> heap -> Ast.expr list -> heap * value) IMap.t
  (* We would need a Vfun too if we were handling closures. But for
   * regular function calls, we just handle 'Call (Id "...")' specially
   * in the interpreter (but we don't for 'Call (Obj_get ...)', hence
   * this intermediate Vmethod value above).
   *)

  | Vobject of value SMap.t

  (* Union of possible types/values, ex: null | object, bool | string, etc.
   * This could grow a lot so the abstract interpreter need to turn
   * that into a Vany at some point ???
   *)
  | Vsum    of value list

  (* tainting analysis for security *)
  | Vtaint of string

  (* usually used when we don't handle certain constructs or when
   * the code is too dynamic
   *)
  | Vany

  and type_ =
    | Tint
    | Tbool
    | Tfloat
    | Tstring
    (* useful for tainting analysis again *)
    | Txhp

(* this could be one field of env too, close to .vars and .globals *)
and heap = {
  (* a heap maps addresses to values (which can also be addresses (Vptr)) *)
  ptrs: value IMap.t;
}

(* mutually recursive types because Vmethod above needs an env *)
and env = {
  db: code_database;

  (* local variables and parameters (will be pointer to pointer to values) *)
  vars    : value SMap.t ref;
  (* globals and static variables (prefixed with a "<function>**" *)
  globals : value SMap.t ref;

  (* for debugging, to print for instance in which file we have XSS  *)
  file    : string ref;
  (* current function processed, used for handling static variables,
   * to add fake "<function>**$var" in globals *)
  cfun    : string;
  (* call stack used for debugging when found an XSS hole and used also
   * for callgraph generation.
   * todo: could be put in the env too, next to 'stack' and 'safe'? take
   * care of save_excursion though.
   *)
  path: Callgraph_php2.node list ref;
  (* number of recursive calls to a function f. if > 2 then stop, 
   * for fixpoint. 
   *)
  stack   : int SMap.t;

  (* opti: cache of already processed functions safe for tainting *)
  safe    : value SMap.t ref;
}

(* The code of the abstract interpreter and tainting analysis used to be
 * in the same file, but it's better to separate concerns, hence this module
 * which contains hooks that a tainting analysis can fill in.
 *)
module type TAINT =
  sig
    val taint_mode : bool ref

    (* main entry point to warn when found XSS *)
    val check_danger :  env -> heap ->
      string -> Ast_php.info option -> Callgraph_php2.node list (* path *) -> 
      value -> unit

    val taint_expr : env -> heap ->
      (env -> heap -> Ast_php_simple.expr -> heap * value) *
      (env -> heap -> Ast_php_simple.expr -> heap * bool * value) *
      (env -> heap -> value -> 'a * 'b) *
      ('b -> env -> 'a -> Ast_php_simple.expr list -> heap * value) *
      (env -> heap -> value -> Ast_php_simple.expr list -> heap * value) *
      (env -> heap -> bool -> value -> value -> heap * value)
      ->
      Callgraph_php2.node list ->
      Ast_php_simple.expr -> 
      heap * value

    val binary_concat : env -> heap ->
      value -> value -> 
      Callgraph_php2.node list (* path *) -> 
      value

    val fold_slist : value list -> value

    val when_call_not_found : heap -> value list -> value

    module GetTaint :
      sig
        exception Found of string list
        val list : ('a -> unit) -> 'a list -> unit
        val value : heap -> value -> string list option
      end
  end

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let empty_heap = {
  ptrs  = IMap.empty;
}

let empty_env db file =
  let globals = ref SMap.empty in
  { db = db;
    file = ref file;
    globals = globals;
    (* Why use same ref? because when we process toplevel statements,
     * the vars are the globals.
     *)
    vars = globals; 
    cfun = "*TOPLEVEL*";
    stack   = SMap.empty ;
    safe = ref SMap.empty;
    path = ref [];
  }

(*****************************************************************************)
(* String-of like *)
(*****************************************************************************)

let rec list o f l =
  match l with
  | [] -> ()
  | [x] -> f o x
  | x :: rl -> f o x; o ", "; list o f rl

let rec value ptrs o x =
  match x with
  | Vany -> o "Any"
  | Vnull -> o "Null"
  | Vtaint s -> o "PARAM:"; o s
  | Vabstr ty -> type_ o ty

  | Vbool b -> o (string_of_bool b)
  | Vint n -> o (string_of_int n)
  | Vfloat f -> o (string_of_float f)
  | Vstring s -> o "'"; o s; o "'"

  | Vref s ->
      o "&REF ";
      let l = ISet.elements s in
      list o (fun o x -> o (string_of_int x)) l;
      let n = ISet.choose s in
      (try
          o "{";
          value (IMap.remove n ptrs) o (IMap.find n ptrs);
          o "}"
      with Not_found -> o "rec"
      )
  | Vptr n when IMap.mem n ptrs ->
      o "&";
      o (string_of_int n);
      o "{";
      value (IMap.remove n ptrs) o (IMap.find n ptrs);
      o "}"
  (* pad: when this happens??? *)
  | Vptr _ -> o "rec"
  | Vrecord m ->
      let vl = SMap.fold (fun x y acc -> (x, y) :: acc) m [] in
      o "record(";
      list o (fun o (x, v) -> o "'"; o x ; o "' => "; value ptrs o v) vl;
      o ")"
  | Vobject m ->
      let vl = SMap.fold (fun x y acc -> (x, y) :: acc) m [] in
      o "object(";
      list o (fun o (x, v) -> o "'"; o x ; o "' => "; value ptrs o v) vl;
      o ")"
  | Varray vl ->
      o "array(";
      list o (value ptrs) (List.rev vl);
      o ")";
  | Vmap (v1, v2) ->
      o "[";
      value ptrs o v1;
      o " => ";
      value ptrs o v2;
      o "]"
  | Vmethod (v, _imap) -> o "method("; value ptrs o v; o ")"
  | Vsum vl ->
      o "choice(";
      list o (value ptrs) vl;
      o ")"

and type_ o x =
  match x with
  | Tint -> o "int"
  | Tbool -> o "bool"
  | Tfloat -> o "float"
  | Tstring -> o "string"
  | Txhp -> o "xhp"

(*****************************************************************************)
(* Printing *)
(*****************************************************************************)

and print_locals_and_globals o env heap =
  SMap.iter (fun s v ->
    o "|"; o s ; o " = "; value heap.ptrs o v; o "\n"
  ) !(env.vars);
  SMap.iter (fun s v ->
    o "|"; o "GLOBAL "; o s ; o " = "; value heap.ptrs o v; o "\n"
  ) !(env.globals)

(*****************************************************************************)
(* Debugging *)
(*****************************************************************************)

let string_of_value heap v =
  Common.with_open_stringbuf (fun (_pr, buf) ->
    let pr s = Buffer.add_string buf s in
    value heap.ptrs pr v
  )
