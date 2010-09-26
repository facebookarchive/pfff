(* Copyright (C) 2008 Yoann Padioleau
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)

open Common

module Ast_c = Ast_java
module Visitor_c = Visitor_java

(*****************************************************************************)
(* Extract infos *)
(*****************************************************************************)

let extract_info_visitor recursor x = 
  let globals = ref [] in
  let visitor = 
    {
      Visitor_c.default_visitor_s with
        Visitor_c.kinfo_s = (fun (k, _) i -> 
          Common.push2 i globals;
          i
        )
    } in
  begin
    ignore(recursor visitor x);
    !globals
  end

let ii_of_stmt = extract_info_visitor Visitor_c.stmt
let ii_of_ini  = extract_info_visitor Visitor_c.init

let ii_of_decls = extract_info_visitor Visitor_c.decls
let ii_of_modifiers = extract_info_visitor Visitor_c.modifiers

(*****************************************************************************)
let max_min_ii_by_pos xs = 
  match xs with
  | [] -> failwith "empty list, max_min_ii_by_pos"
  | [x] -> (x, x)
  | x::xs -> 
      let pos_leq p1 p2 = (Ast_c.compare_pos p1 p2) = (-1) in
      xs +> List.fold_left (fun (maxii,minii) e -> 
        let maxii' = if pos_leq maxii e then e else maxii in
        let minii' = if pos_leq e minii then e else minii in
        maxii', minii'
      ) (x,x)

(*
let info_to_fixpos ii =
  match Ast_c.pinfo_of_info ii with
    Ast_c.OriginTok pi -> Ast_cocci.Real pi.Common.charpos
  | Ast_c.ExpandedTok (_,(pi,offset)) ->
      Ast_cocci.Virt (pi.Common.charpos,offset)
  | Ast_c.FakeTok (_,(pi,offset)) ->
      Ast_cocci.Virt (pi.Common.charpos,offset)
  | Ast_c.AbstractLineTok pi -> failwith "unexpected abstract"
*)
  
let max_min_by_pos xs = 
  let (i1, i2) = max_min_ii_by_pos xs in
  (Ast_c.pos_of_info i1, Ast_c.pos_of_info i2)



