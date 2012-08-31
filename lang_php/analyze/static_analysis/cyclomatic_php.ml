(*s: cyclomatic_php.ml *)
(*s: Facebook copyright *)
(* Yoann Padioleau
 * 
 * Copyright (C) 2009, 2010, 2011 Facebook
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
(*e: Facebook copyright *)
open Common

open Ast_php
module Ast = Ast_php
module V = Visitor_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * References:
 *  - A Complexity Measure, by  Thomas J. McCabe, IEEE Transactions on
 *    software engineering, Dec 1976
 *  - http://en.wikipedia.org/wiki/Cyclomatic_complexity
 * 
 * From the wikipedia page:
 * "Cyclomatic complexity (or conditional complexity) is a software metric
 * (measurement). It was developed by Thomas J. McCabe, Sr. in 1976 and
 * is used to indicate the complexity of a program. It directly measures
 * the number of linearly independent paths through a program's source
 * code. 
 * 
 * The complexity is then defined as
 * 
 * M = E - N + 2P
 * 
 * where
 * 
 * M = cyclomatic complexity
 * E = the number of edges of the graph
 * N = the number of nodes of the graph
 * P = the number of connected components 
 * 
 * ...
 * For a single program (or subroutine or method), P is always equal to 1.
 * "
 * 
 * The goal of having a set of "linear independent paths" 
 * is to have a set of "foundational paths" from which all other paths
 * can be described (by a linear combination of those foundational paths).
 * 
 * Note that the cyclomatic complexity does not care about
 * the complexity of expressions or function calls. For instance
 * 'if(e1 || e1 || e3) { ...} will have the same cyclomatic complexity 
 * as 'if(e1) { ... }' because they have a similar CFG.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type selection = 
  | Threshold of int
  | Topn of int


(*****************************************************************************)
(* Main algorithm *)
(*****************************************************************************)

let cyclomatic_complexity_flow ?(verbose=false) flow = 

  let n = flow#nb_nodes in
  let e = flow#nb_edges in
  let p = 1 in

  let m = e - n +  2 * p  in

  if verbose 
  then pr2 (spf "N = %d, E = %d" n e);

  m

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let string_of_bad_cyclo name cyclo = 
  let info = Ast.info_of_name name in
  spf "cyclo for %s at %s:%d = %d" 
    (Ast.name name) 
    (Ast.file_of_info info)
    (Ast.line_of_info info)
    cyclo

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let cyclomatic_complexity_func ?verbose func =
  let flow = Controlflow_build_php.cfg_of_func func in
  cyclomatic_complexity_flow flow

let cyclomatic_complexity_file file = 
  let (ast2, _stat) = Parse_php.parse file in
  let ast = Parse_php.program_of_program2 ast2 in

  let res = ref [] in

  let hooks = { V.default_visitor with
    V.kfunc_def = (fun (k, _) def ->
      (* the function can have nested funcs *)
      k def;
      let flow = Controlflow_build_php.cfg_of_func def in
      let cyclo = cyclomatic_complexity_flow flow in
      Common.push2  (def.f_name, cyclo) res;
       );
    V.kexpr = (fun (k, _) x ->
      match x with
      (* don't go inside lambdas *)
      | Lambda _ ->
          ()
      | _ -> k x
    );
  }
  in
  (try 
      (V.mk_visitor hooks) (Program ast);
    with
    Controlflow_build_php.Error err ->
      Controlflow_build_php.report_error err
  );
  !res

(*****************************************************************************)
(* Finding bad code *)
(*****************************************************************************)

let (code_with_bad_cyclomatic: selection -> Common.filename list -> unit) =
 fun selection files ->
   
   let hscore = Hashtbl.create 101 in

   (* populating hscore *)
   Flag_parsing_php.show_parsing_error := true;
   files +> List.iter (fun file ->
     pr2 (spf "processing: %s" file);

     let cyclos = cyclomatic_complexity_file file in
     cyclos +> List.iter (fun (name, cyclo) ->
       Hashtbl.add hscore name cyclo;
       pr2 (string_of_bad_cyclo name cyclo);
     );
   );

   (* rank *)
   let xs = Common.hash_to_list hscore in
   let bad = 
     match selection with
     | Threshold n ->
         xs +> List.filter (fun (name, score) -> 
           score >= n
         ) +> Common.sort_by_val_highfirst
     | Topn n -> 
         xs +> Common.sort_by_val_highfirst +> 
           Common.take_safe n
   in
   pr2_xxxxxxxxxxxxxxxxx ();
   bad +> List.iter (fun (name, score) -> 
     pr2 (string_of_bad_cyclo name score)
   )

(*e: cyclomatic_php.ml *)
