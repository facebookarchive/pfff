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

module PP = Pretty_print_pil

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* This is a ugly copy paste of controlflow_php.ml. We should be able to
 * factorize code or functorize or whatever to avoid code duplication
 * but for now I want to get a working data flow analysis as fast
 * as possible. Moreover Linus approves copy paste!
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type node = {
  (* For now we just have node_kind, but later if we want to do some data-flow
   * analysis or use temporal logic, we may want to add extra information
   * in each CFG nodes. We could also record such extra
   * information in an external table that maps Ograph_extended.nodei,
   * that is nodeid, to some information.
   *)
  n: node_kind;
}
 and node_kind =

  (* special fake cfg nodes *)
  | Enter
  | Exit

  (* An alternative is to store such information in the edges, but
   * experience shows it's easier to encode it via regular nodes
   *)
  | TrueNode
  | FalseNode

  | IfHeader of expr
  | WhileHeader of expr

  | Return of expr option

  | Jump

  | TryHeader
  | Throw

  | Echo of expr list
  | Instr of instr

  | TodoNode of Parse_info.info option

  | Join
 (* with tarzan *)

(* For now there is just one kind of edge. Later we may have more,
 * see the ShadowNode idea of Julia Lawall.
 *)
type edge = Direct

type flow = (node, edge) Ograph_extended.ograph_mutable

type nodei = Ograph_extended.nodei

(* for the visitor *)
type any =
  | Lvalue of lvalue
  | Expr of expr
  | Instr2 of instr
  | Stmt of stmt
  | Node of node

  | StmtList of stmt list

  | Toplevel of toplevel
  | Program of program

(*****************************************************************************)
(* String of *)
(*****************************************************************************)

let short_string_of_node node =
  match node.n with
  | Enter -> "<enter>"
  | Exit -> "<exit>"

  | FalseNode -> "<false>"
  | TrueNode -> "<true>"

  | Echo xs ->
      "echo " ^ (xs +> List.map PP.string_of_expr +> Common.join ", ")
  | Instr i ->
      PP.string_of_instr i

  | WhileHeader e ->
      "while " ^ PP.string_of_expr e

  | Return e ->
      "return " ^
      (match e with
      | None -> ""
      | Some e -> PP.string_of_expr e
      )
  | IfHeader e ->
      "if " ^ PP.string_of_expr e

  | Join -> "<join>"

  | TodoNode _ -> "<TodoStmt>"

  | (Throw|TryHeader|Jump)
    -> raise Todo

(*****************************************************************************)
(* Accessors *)
(*****************************************************************************)


let (first_node : flow -> Ograph_extended.nodei) = fun flow ->
  fst (List.find (fun (_, nk) -> nk.n == Enter) flow#nodes#tolist)

let (mk_node: node_kind -> node) = fun nk ->
  raise Todo

(* using internally graphviz dot and ghostview on X11 *)
let (display_flow: flow -> unit) = fun flow ->
  flow +> Ograph_extended.print_ograph_mutable_generic
    ~s_of_node:(fun (nodei, node) ->
      let s = short_string_of_node node in
      (* 'dot' does not like quote *)
      String.escaped s, None, None
    )
