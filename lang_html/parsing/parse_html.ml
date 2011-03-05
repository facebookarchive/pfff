(* Yoann Padioleau
 *
 * Copyright (C) 2001-2006 Patrick Doane and Gerd Stolpmann
 * Copyright (C) 2011 Facebook
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

open Ast_html
(* todo: remove *)
open Parser_html

open Dtd

module Ast = Ast_html
module Flag = Flag_parsing_html
module TH   = Token_helpers_html

module T = Parser_html

module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * src: most of the code in this file comes from ocamlnet/netstring/.
 * The original CVS ID is:
 * $Id: nethtml.ml 1296 2009-11-18 13:27:41Z ChriS $
 * I've extended it mainly to add position information. I've also
 * moved stuff in dtd.ml and removed the encode/decode and xmap stuff.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
exception Parse_error of Parse_info.info

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)

(* 
 * For many languages I have tokens() and parse() functions, but for
 * HTML because the lexical rules are so tied to the parsing rules,
 * this module does not provide a tokens() function.
 *)

(*****************************************************************************)
(* Ocamlnet parser *)
(*****************************************************************************)

(* a small wrapper over ocamlnet *)
let (parse_simple_tree: Ast_html.html_raw -> Ast_html.html_tree2) = 
 fun (Ast.HtmlRaw raw) -> 
  let ch = new Netchannels.input_string raw in
  Nethtml.parse 
    ~return_declarations:true 
    ~return_pis:true
    ~return_comments:true
    ch

(*****************************************************************************)
(* Parsing helpers *)
(*****************************************************************************)
exception End_of_scan

let rec parse_special name buf =
  (* Parse until </name> *)
  match Lexer_html.scan_special buf with
  | Lelementend (_tok, n) ->
      if String.lowercase n = name 
      then ""
      else "</" ^ n ^ parse_special name buf
  | EOF _ -> raise End_of_scan
  | Cdata (_tok, s) -> s ^ parse_special name buf
  | _ ->
      (* Illegal *)
      parse_special name buf

let rec skip_element buf =
  (* Skip until ">" (or "/>") *)
  match Lexer_html.scan_element buf with
  | Relement _ | Relement_empty _ ->  ()
  | EOF _ -> raise End_of_scan
  | _ -> skip_element buf


let rec next_no_space p_string buf =
  (* p_string: whether string literals in quotation marks are allowed *)
  let tok =
    if p_string 
    then Lexer_html.scan_element_after_Is buf
    else Lexer_html.scan_element buf in
  match tok with
  | Space _ -> next_no_space p_string buf
  | t -> t


(*****************************************************************************)
(* Misc helpers *)
(*****************************************************************************)

let model_of ~dtd_hash element_name =
  if element_name = "" 
  then (Everywhere, Any)
  else
    try 
      (match Hashtbl.find dtd_hash element_name with 
      | (eclass, Sub_exclusions(_,m)) -> eclass, m
      | m -> m
      )
    with Not_found -> (Everywhere, Any)

let exclusions_of ~dtd_hash element_name =
  if element_name = "" 
  then []
  else
    try
      (match Hashtbl.find dtd_hash element_name with
      | (eclass, Sub_exclusions(l,_)) -> l
      | _ -> []
      )
    with Not_found -> []

let is_possible_subelement 
 ~dtd_hash parent_element parent_exclusions sub_element =
  let (sub_class, _) = model_of ~dtd_hash sub_element in
  let rec eval m =
    match m with
    | Inline2     -> sub_class = Inline
    | Block2      -> sub_class = Block  || sub_class = Essential_block
    | Flow       -> 
        sub_class = Inline || sub_class = Block || sub_class = Essential_block
    | Elements l -> List.mem sub_element l
    | Any        -> true
    | Or(m1,m2)  -> eval m1 || eval m2
    | Except(m1,m2) -> eval m1 && not (eval m2)
    | Empty      -> false
    | Special    -> false
    | Sub_exclusions(_,_) -> assert false
  in
  (sub_class = Everywhere) || 
  (
    (not (StringSet.mem sub_element parent_exclusions)) &&
      let (_, parent_model) = model_of ~dtd_hash parent_element in
      eval parent_model
  )

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

exception Found

(* *)
let parse2 file =
 Common.with_open_infile file (fun chan -> 
  let buf = Lexing.from_channel chan in

  let dtd = Dtd.html40_dtd in
  let dtd_hash = Common.hash_of_list dtd in

  let current_name = ref "" in
  let current_atts = ref [] in
  let current_subs = ref [] in
  let current_excl = ref StringSet.empty in      (* current exclusions *)

  let stack = Stack.create() in

  let unwind_stack sub_name =
    (* If the current element is not a possible parent element for sub_name,
     * search the parent element in the stack.
     * Either the new current element is the parent, or there was no
     * possible parent. In the latter case, the current element is the
     * same element as before.
     *)
    let backup = Stack.create() in
    let backup_name = !current_name in
    let backup_atts = !current_atts in
    let backup_subs = !current_subs in
    let backup_excl = !current_excl in
    try
      while not (is_possible_subelement 
                    ~dtd_hash !current_name !current_excl sub_name) do
        (* Maybe we are not allowed to end the current element: *)
        let (current_class, _) = model_of ~dtd_hash !current_name in
        if current_class = Essential_block then raise Stack.Empty;
        (* End the current element and remove it from the stack: *)
        let grant_parent = Stack.pop stack in
        Stack.push grant_parent backup;        (* Save it; may we need it *)
        let (gp_name, gp_atts, gp_subs, gp_excl) = grant_parent in
        (* If gp_name is an essential element, we are not allowed to close
         * it implicitly, even if that violates the DTD.
         *)
        let current = Element (!current_name, !current_atts, 
                               List.rev !current_subs) in
        current_name := gp_name;
        current_atts := gp_atts;
        current_excl := gp_excl;
        current_subs := current :: gp_subs
      done;
    with Stack.Empty ->
      (* It did not work! Push everything back to the stack, and
       * resume the old state.
       *)
      while Stack.length backup > 0 do
        Stack.push (Stack.pop backup) stack
      done;
      current_name := backup_name;
      current_atts := backup_atts;
      current_subs := backup_subs;
      current_excl := backup_excl
  in

  let parse_atts() =

    let rec parse_atts_lookahead next =
      match next with
      | Relement _  -> ( [], false )
      | Relement_empty _  -> ( [], true )
      | Name (_tok, n) ->
          (match next_no_space false buf with
          | Is _ ->
              (match next_no_space true buf with
              | Name (_tok, v) ->
                  let toks, is_empty =
                    parse_atts_lookahead (next_no_space false buf) in
                  ( (String.lowercase n, v) :: toks, is_empty )
              | Literal (_tok, v) ->
                  let toks, is_empty =
                    parse_atts_lookahead (next_no_space false buf) in
                  ( (String.lowercase n,v) :: toks, is_empty )
              | EOF _ ->
                  raise End_of_scan
              | Relement _ ->
                  (* Illegal *)
                  ( [], false )
              | Relement_empty _ ->
                  (* Illegal *)
                  ( [], true )
              | _ ->
                  (* Illegal *)
                  parse_atts_lookahead (next_no_space false buf)
              )
          | EOF _ ->
              raise End_of_scan
          | Relement _ ->
              (* <tag name> <==> <tag name="name"> *)
              ( [ String.lowercase n, String.lowercase n ], false)
          | Relement_empty _ ->
              (* <tag name> <==> <tag name="name"> *)
              ( [ String.lowercase n, String.lowercase n ], true)
          | next' ->
              (* assume <tag name ... > <==> <tag name="name" ...> *)
              let toks, is_empty = 
                parse_atts_lookahead next' in
              ( ( String.lowercase n, String.lowercase n ) :: toks,
              is_empty)
          )
      | EOF _ ->
          raise End_of_scan
      | _ ->
          (* Illegal *)
          parse_atts_lookahead (next_no_space false buf)
    in
    parse_atts_lookahead (next_no_space false buf)
  in

  let rec parse_next() =
    let t = Lexer_html.scan_document buf in
    match t with
    | TComment info ->
        let comment = PI.str_of_info info in
        Common.push2 (Element("--",["contents",comment],[])) current_subs;
        parse_next()
    | TDoctype info ->
        let decl = PI.str_of_info info in
        Common.push2 (Element("!",["contents",decl],[])) current_subs;
        parse_next()
    | TPi info ->
        let pi = PI.str_of_info info in
        Common.push2 (Element("?",["contents",pi],[])) current_subs;
        parse_next()

    | Lelement (_tok, name) ->
        let name = String.lowercase name in
        let (_, model) = model_of ~dtd_hash name in
        (match model with
        | Empty ->
          let atts, _ = parse_atts() in
          unwind_stack name;
          current_subs := (Element(name, atts, [])) :: !current_subs;
          parse_next()
        | Special ->
            let atts, is_empty = parse_atts() in
            unwind_stack name;
            let data = 
              if is_empty then 
                ""
              else (
                let d = parse_special name buf in
                (* Read until ">" *)
                skip_element buf;
                d
              ) in
            current_subs := (Element(name, atts, [Data data])) :: !current_subs;
            parse_next()
        | _ ->
            let atts, is_empty = parse_atts() in
            (* Unwind the stack until we find an element which can be
             * the parent of the new element:
             *)
            unwind_stack name;
            if is_empty then (
              (* Simple case *)
              current_subs := (Element(name, atts, [])) :: !current_subs;
            )
            else (
              (* Push the current element on the stack, and this element
               * becomes the new current element:
               *)
              let new_excl = exclusions_of ~dtd_hash name in
              Stack.push 
                (!current_name, 
                !current_atts, !current_subs, !current_excl)
                stack;
              current_name := name;
              current_atts := atts;
              current_subs := [];
              List.iter
                (fun xel -> current_excl := 
                  StringSet.add xel !current_excl)
                new_excl;
            );
            parse_next()
        )
    | Cdata (_tok, data) ->
        current_subs := (Data data) :: !current_subs;
        parse_next()
    | Lelementend (_tok, name) ->
        let name = String.lowercase name in
        (* Read until ">" *)
        skip_element buf;
        (* Search the element to close on the stack: *)
        let found = 
          (name = !current_name) ||
            try
              Stack.iter
                (fun (old_name, _, _, _) ->
                  if name = old_name then raise Found;
                  match model_of ~dtd_hash old_name with
                  Essential_block, _ -> raise Not_found;
                    (* Don't close essential blocks implicitly *)
                  | _ -> ())
                stack;
              false
            with
            | Found -> true
            | Not_found -> false
        in
        (* If not found, the end tag is wrong. Simply ignore it. *)
        if not found then
          parse_next()
        else begin
          (* If found: Remove the elements from the stack, and append
           * them to the previous element as sub elements
           *)
          while !current_name <> name do
            let old_name, old_atts, old_subs, old_excl = Stack.pop stack in
            current_subs := (Element (!current_name, !current_atts,
                                     List.rev !current_subs)) :: old_subs;
            current_name := old_name;
            current_atts := old_atts;
            current_excl := old_excl
          done;
          (* Remove one more element: the element containing the element
           * currently being closed.
           *)
          let old_name, old_atts, old_subs, old_excl = Stack.pop stack in
          current_subs := (Element (!current_name, !current_atts,
                                   List.rev !current_subs)) :: old_subs;
          current_name := old_name;
          current_atts := old_atts;
          current_excl := old_excl;
          (* Go on *)
          parse_next()
        end
    | EOF _ ->
        raise End_of_scan
    | _ ->
        parse_next()
  in

  let xs = 
    try
      parse_next();  (* never returns. Will get a warning X *)
      assert false
    with End_of_scan ->
      (* Close all remaining elements: *)
      while Stack.length stack > 0 do
        let old_name, old_atts, old_subs, old_excl = Stack.pop stack in
        current_subs := Element (!current_name,
                                !current_atts,
                                List.rev !current_subs) :: old_subs;
        current_name := old_name;
        current_atts := old_atts;
        current_excl := old_excl
      done;
      List.rev !current_subs
  in
  Element ("__root__", [], xs)
 )

let parse a = 
  Common.profile_code "Parse_html.parse" (fun () -> parse2 a)

