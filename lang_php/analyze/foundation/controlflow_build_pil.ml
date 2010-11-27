(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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

open Pil

module Ast = Pil
module F = Controlflow_pil

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* This is a ugly copy paste of controlflow_build_php.ml. We should be able to
 * factorize code or functorize or whatever to avoid code duplication
 * but for now I want to get a working data flow analysis as fast
 * as possible. Moreover Linus approves copy paste!
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* an int representing the index of a node in the graph *)
type nodei = Ograph_extended.nodei 

(* Information passed recursively in cfg_stmt or cfg_stmt_list below.
 * The graph g is mutable, so most of the work is done by side effects on it.
 * No need to return a new state.
 *)
type state = {
  g: F.flow;

  (* When there is a 'return' we need to know the exit node to link to *)
  exiti: nodei;

  (* Sometimes when there is a 'continue' or 'break' we must know where 
   * to jump and so we must know the node index for the end of the loop.
   * The same kind of information is needed for 'switch' or 'try/throw'.
   * 
   * Because loops can be inside switch or try, and vice versa, you need
   * a stack of context.
   *)
  ctx: context Common.stack;
}
 and context = 
  | NoCtx
  | LoopCtx   of nodei (* head *) * nodei (* end *)
(*
  | SwitchCtx of nodei (* end *)
  | TryCtx    of nodei (* the first catch *)
*)

type error = 
  | NoEnclosingLoop of Ast_php.info
  | DynamicBreak    of Ast_php.info

exception Error of error

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let add_arc (starti, nodei) g = 
  g#add_arc ((starti, nodei), F.Direct)
 
let add_arc_opt (starti_opt, nodei) g = 
  starti_opt |> Common.do_option (fun starti -> 
    g#add_arc ((starti, nodei), F.Direct)
  )

(*****************************************************************************)
(* Algorithm *)
(*****************************************************************************)

(*
 * The CFG building algorithm works by iteratively visiting the 
 * statements in the AST of a function. At each statement,
 * the cfg_stmt function is called, and passed the index of the
 * previous node (if there is one), and returns the index of 
 * the created node (if there is one).
 * 
 * history:
 * 
 * ver1: old code was returning a nodei, but break has no end, so
 * cfg_stmt should return a nodei option.
 * 
 * ver2: old code was taking a nodei, but should also take a nodei
 * option. There can be deadcode in the function.
 * 
 * subtle: try/throw. The current algo is not very precise, but 
 * it's probably good enough for many analysis.
 * 
 * ver3: adapt from working on Ast_php to working on Pil.
 *)
let rec (cfg_stmt: state -> nodei option -> stmt -> nodei option) = 
 fun state previ stmt ->

   match stmt with
   | Echo x ->
       let newi = state.g#add_node { F.n = F.Echo x; } in
       state.g |> add_arc_opt (previ, newi);
       Some newi
   | Instr x -> 
       let newi = state.g#add_node { F.n = F.Instr x; } in
       state.g |> add_arc_opt (previ, newi);
       Some newi

   | TodoStmt info ->
       let newi = state.g#add_node { F.n = F.TodoNode (Some info); } in
       state.g |> add_arc_opt (previ, newi);
       Some newi

   | EmptyStmt ->
       previ

   | While (e, stmt) ->
     (* previ -> newi ---> newfakethen -> ... -> finalthen -
      *             |---|-----------------------------------|
      *                 |-> newfakelse 
      *)
       let node = F.WhileHeader e in

       let newi = state.g#add_node { F.n = node } in
       state.g |> add_arc_opt (previ, newi);

       let newfakethen = state.g#add_node { F.n = F.TrueNode; } in
       let newfakeelse = state.g#add_node { F.n = F.FalseNode; } in
       state.g |> add_arc (newi, newfakethen);
       state.g |> add_arc (newi, newfakeelse);

       let state = { state with
         ctx = LoopCtx (newi, newfakeelse)::state.ctx;
       }
       in
       let finalthen = 
         cfg_stmt state (Some newfakethen) stmt
       in
       state.g |> add_arc_opt (finalthen, newi);
       Some newfakeelse


   | Block xs -> 
       cfg_stmt_list state previ xs

   | Return e ->
       let newi = state.g#add_node { F.n = F.Return e } in
       state.g |> add_arc_opt (previ, newi);
       state.g |> add_arc (newi, state.exiti);

       (* the next statement if there is one will not be linked to
        * this new node *)
       None

   | If (e, st_then, st_else) ->
     (* previ  -> newi --->   newfakethen -> ... -> finalthen --> lasti
      *                  |                                      |
      *                  |->   newfakeelse -> ... -> finalelse -|
      * 
      *)
       let newi = state.g#add_node { F.n = F.IfHeader (e); } in
       state.g |> add_arc_opt (previ, newi);
       
       let newfakethen = state.g#add_node { F.n = F.TrueNode; } in
       let newfakeelse = state.g#add_node { F.n = F.FalseNode; } in
       state.g |> add_arc (newi, newfakethen);
       state.g |> add_arc (newi, newfakeelse);

       let finalthen = cfg_stmt state (Some newfakethen) st_then in
       let finalelse = cfg_stmt state (Some newfakeelse) st_else in

       (match finalthen, finalelse with
       | None, None ->
           (* probably a return in both branches *)
           None
       | Some nodei, None
       | None, Some nodei ->
           Some nodei
       | Some n1, Some n2 ->
           let lasti = state.g#add_node { F.n = F.Join } in
           state.g |> add_arc (n1, lasti);
           state.g |> add_arc (n2, lasti);
           Some lasti
       )
       
        
   | (Try (_, _)|Throw _|Continue _|Break _)
       ->
       let newi = state.g#add_node { F.n = F.TodoNode None; } in
       state.g |> add_arc_opt (previ, newi);
       Some newi

and cfg_stmt_list state previ xs =
  xs +> List.fold_left (fun previ stmt -> 
    cfg_stmt state previ stmt
  ) previ

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let (control_flow_graph_of_stmts: stmt list -> F.flow) = fun xs ->

  (* yes, I sometimes use objects, and even mutable objects in OCaml ... *)
  let g = new Ograph_extended.ograph_mutable in

  let enteri = g#add_node { F.n = F.Enter; } in
  let exiti = g#add_node { F.n = F.Exit; } in

  let state = {
    g = g;
    exiti = exiti;
    ctx = [NoCtx]; (* could also remove NoCtx and use an empty list *)
  }
  in
  let last_node_opt = 
    cfg_stmt_list state (Some enteri) xs
  in
  (* maybe the body does not contain a single 'return', so by default
   * connect last stmt to the exit node
   *)
  g |> add_arc_opt (last_node_opt, exiti);
  g




let cfg_of_stmts = control_flow_graph_of_stmts

(*****************************************************************************)
(* Error management *)
(*****************************************************************************)

let string_of_error err =
  let spos info = 
    let info = Ast_php.parse_info_of_info info in
    (* emacs compile-mode compatible output *)
    spf "%s:%d:%d: " 
      info.Parse_info.file info.Parse_info.line info.Parse_info.column
  in

  match err with
  | NoEnclosingLoop info ->
      spos info ^ "FLOW: no enclosing loop found for break or continue"
  | DynamicBreak info ->
      spos info ^ "FLOW: dynamic break/continue are not supported"


let info_of_error error =
  match error with
  | NoEnclosingLoop info
  | DynamicBreak info
      -> Some info

let (report_error : error -> unit) = fun err ->
  pr2 (string_of_error err)
