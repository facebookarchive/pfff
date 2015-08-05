(* Yoann Padioleau
 *
 * Copyright (C) 2013 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * When searching for or refactoring code, regexps are good enough most of 
 * the time; tools such as 'grep' or 'sed' are great. But certain regexps
 * are tedious to write when one needs to handle variations in spacing,
 * the possibilty to have comments in the middle of the code you
 * are looking for, or newlines. Things are even more complicated when
 * you want to handle nested parenthesized expressions or statements. This is
 * because regexps can't count. For instance how would you
 * remove a namespace in C++? You would like to write a transformation
 * like:
 * 
 *  - namespace my_namespace {
 *    ...
 *  - }
 * 
 * but regexps can't do that[1].
 * 
 * The alternative is then to use more precise tools such as 'sgrep'
 * or 'spatch'. But implementing sgrep/spatch in the usual way 
 * for a new language, by matching AST against AST, can be really tedious. 
 * The AST can be big and even if we can auto generate most of the
 * boilerplate code, this still takes quite some effort (see lang_php/matcher).
 * 
 * Moreover, in my experience matching AST against AST lacks
 * flexibility sometimes. For instance many people want to use 'sgrep' to
 * find a method foo and so do "sgrep -e 'foo(...)'" but
 * because the matching is done at the AST level, 'foo(...)' is
 * parsed as a function call, not a method call, and so it will
 * not work. But people expect it to work because it works
 * with regexps. So 'sgrep' for PHP currently forces people to write this
 * pattern '$V->foo(...)'.
 * In the same way a pattern like '1' was originally matching
 * only expressions, but was not matching static constants because
 * again it was a different AST constructor. Actually many
 * of the extensions and bugfixes in sgrep_php/spatch_php in 
 * the last year has been related to this lack of flexibility
 * because the AST was too precise.
 * 
 * Enter Ast_fuzzy, a way to factorize most of the needs of
 * 'sgrep' and 'spatch' over different programming languages,
 * while being more flexible in some ways than having a precise AST.
 * It fills a niche between regexps and very-precise ASTs.
 * 
 * In Ast_fuzzy we just want to keep the parenthesized information
 * from the code, and abstract away spacing, the main things that 
 * regexps have troubles with, and then let people match over this
 * parenthesized cleaned-up tree in a flexible way.
 * 
 * related:
 *  - xpath? but do programming languages need the full power of xpath?
 *    usually an AST just have 3 different kinds of nodes, Defs, Stmts,
 *    and Exprs.
 * 
 * See also lang_cpp/parsing_cpp/test_parsing_cpp and its parse_cpp_fuzzy()
 * and dump_cpp_fuzzy() functions. Most of the code related to Ast_fuzzy
 * is in matcher/ and called from 'sgrep' and 'spatch'.
 * For 'sgrep' and 'spatch' examples, see unit_matcher.ml as well as
 * tests/cpp/sgrep/ and tests/cpp/spatch/
 * 
 * notes:
 *  [1] Actually Perl regexps are more powerful so one can do for instance:
 *  echo 'something< namespace<x<y<z,t>>>, other >' | 
 *    perl -pe 's/namespace(<(?:[^<>]|(?1))*>)/foo/'
 *  => 'something< foo, other >'
 *  but it's arguably more complicated than the proposed spatch above.
 * 
 * todo:
 *  - handle infix operators: parse them not as a sequence
 *    but as a tree as we want for instance '$X->foo()' to match
 *    whole expression like 'this->bar()->foo()', or we want
 *    '$X' to match '1+1' (and not only in Parens context)
 *  - same for function calls? so maybe we need to transform our
 *    original program in a lisp like AST where things are more uniform
 *  - how to handle isomorphisms like 'order of attributes don't matter'
 *    as in XHP? or class that can be mentioned anywhere in the arguments
 *    to implements? or how can we make 'class X { ... }' to also match
 *    'class X extends whatever { ... }'? or have public/static to
 *    be optional?
 *    Use regexp over trees? Use isomorphisms file as in coccinelle?
 *    Have special mark about optional things in ast_fuzzy? 
 *    Derives such information from the grammar?
 *  - want powerful queries like 
 *      'class X { ... function(...) { ... foo() ... } ... }
 *    so sgrep powerful for microlevel queries, and prolog for macrolevel
 *    queries. Xpath? Css selector?
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type tok = Parse_info.info
type 'a wrap = 'a * tok

type tree =
  | Braces of tok * trees * tok
  (* todo: comma *)
  | Parens of tok * (trees, tok (* comma*)) Common.either list * tok
  | Angle  of tok * trees * tok

  (* note that gcc allows $ in identifiers, so using $ for metavariables
   * means we will not be able to match such identifiers. No big deal.
   *)
  | Metavar of string wrap
  (* note that "..." are allowed in many languages, so using "..."
   * to represent a list of anything means we will not be able to
   * match specifically "...".
   *)
  | Dots of tok

  | Tok of string wrap
and trees = tree list
 (* with tarzan *)

let is_metavar s =
  s =~ "^\\$.*"

(*****************************************************************************)
(* Visitor *)
(*****************************************************************************)

type visitor_out = trees -> unit

type visitor_in = {
  ktree: (tree -> unit) * visitor_out -> tree -> unit;
  ktrees: (trees -> unit) * visitor_out -> trees -> unit;
  ktok: (tok -> unit) * visitor_out -> tok -> unit;
}

let (default_visitor : visitor_in) = 
  { ktree = (fun (k, _) x -> k x);
    ktok  = (fun (k, _) x -> k x);
    ktrees = (fun (k, _) x -> k x);
  }

let (mk_visitor: visitor_in -> visitor_out) = fun vin ->

  let rec v_tree x =
    let k x = match x with
      | Braces ((v1, v2, v3)) ->
        let _v1 = v_tok v1 and _v2 = v_trees v2 and _v3 = v_tok v3 in ()
      | Parens ((v1, v2, v3)) ->
        let _v1 = v_tok v1
        and _v2 = Ocaml.v_list (Ocaml.v_either v_trees v_tok) v2
        and _v3 = v_tok v3
      in ()

      | Angle ((v1, v2, v3)) ->
        let _v1 = v_tok v1 and _v2 = v_trees v2 and _v3 = v_tok v3 in ()
      | Metavar v1 -> let _v1 = v_wrap v1 in ()
      | Dots v1 -> let _v1 = v_tok v1 in ()
      | Tok v1 -> let _v1 = v_wrap v1 in ()
    in
    vin.ktree (k, all_functions) x
 and v_trees a = 
    let k xs =
      match xs with
      | [] -> ()
      | x::xs ->
        v_tree x;
        v_trees xs;
    in
    vin.ktrees (k, all_functions) a

 and v_wrap (_s, x) = v_tok x
        
 and v_tok x =
    let k _x = () in
    vin.ktok (k, all_functions) x

  and all_functions x = v_trees x in
  all_functions

(*****************************************************************************)
(* Map *)
(*****************************************************************************)

type map_visitor = {
  mtok: (tok -> tok) -> tok -> tok;
}

let (mk_mapper: map_visitor -> (trees -> trees)) = fun hook ->
  let rec map_tree =
    function
    | Braces ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_trees v2
      and v3 = map_tok v3
      in Braces ((v1, v2, v3))
    | Parens ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = List.map (Ocaml.map_of_either map_trees map_tok) v2
      and v3 = map_tok v3
      in Parens ((v1, v2, v3))
    | Angle ((v1, v2, v3)) ->
      let v1 = map_tok v1
      and v2 = map_trees v2
      and v3 = map_tok v3
      in Angle ((v1, v2, v3))
  | Metavar v1 -> let v1 = map_wrap v1 in Metavar ((v1))
  | Dots v1 -> let v1 = map_tok v1 in Dots ((v1))
  | Tok v1 -> let v1 = map_wrap v1 in Tok ((v1))
  and map_trees v = List.map map_tree v
  and map_tok v = 
    let k v = v in
    hook.mtok k v
  and map_wrap (s, t) = (s, map_tok t)
  in
  map_trees

(*****************************************************************************)
(* Extractor *)
(*****************************************************************************)

let (toks_of_trees: trees -> Parse_info.info list) = fun trees ->
  let globals = ref [] in
  let hooks = { default_visitor with
    ktok = (fun (_k, _) i -> Common.push i globals)
  } in
  begin
    let vout = mk_visitor hooks in
    vout trees;
    List.rev !globals
  end

(*****************************************************************************)
(* Abstract position *)
(*****************************************************************************)

let abstract_position_trees trees = 
  let hooks = { 
    mtok = (fun (_k) i -> 
      { i with Parse_info.token = Parse_info.Ab }
    )
  } in
  let mapper = mk_mapper hooks in
  mapper trees

(*****************************************************************************)
(* Vof *)
(*****************************************************************************)

let vof_token t =
  Ocaml.VString (Parse_info.str_of_info t)
  (* Parse_info.vof_token t*)

let rec vof_multi_grouped =
  function
  | Braces ((v1, v2, v3)) ->
      let v1 = vof_token v1
      and v2 = Ocaml.vof_list vof_multi_grouped v2
      and v3 = vof_token v3
      in Ocaml.VSum (("Braces", [ v1; v2; v3 ]))
  | Parens ((v1, v2, v3)) ->
      let v1 = vof_token v1
      and v2 = Ocaml.vof_list (Ocaml.vof_either vof_trees vof_token) v2
      and v3 = vof_token v3
      in Ocaml.VSum (("Parens", [ v1; v2; v3 ]))
  | Angle ((v1, v2, v3)) ->
      let v1 = vof_token v1
      and v2 = Ocaml.vof_list vof_multi_grouped v2
      and v3 = vof_token v3
      in Ocaml.VSum (("Angle", [ v1; v2; v3 ]))
  | Metavar v1 -> let v1 = vof_wrap v1 in Ocaml.VSum (("Metavar", [ v1 ]))
  | Dots v1 -> let v1 = vof_token v1 in Ocaml.VSum (("Dots", [ v1 ]))
  | Tok v1 -> let v1 = vof_wrap v1 in Ocaml.VSum (("Tok", [ v1 ]))
and vof_wrap (s, _x) = Ocaml.VString s
and vof_trees xs =
  Ocaml.VList (xs +> List.map vof_multi_grouped)
