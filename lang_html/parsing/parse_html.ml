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
 * I've also simplified the code, factorized things.
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

(* p_string: whether string literals in quotation marks are allowed *)
let rec skip_space p_string buf =
  let tok =
    if p_string 
    then Lexer_html.scan_element_after_Eq buf
    else Lexer_html.scan_element buf in
  match tok with
  | T.Space _ -> skip_space p_string buf
  | t -> t

(* skip until ">" (or "/>") *)
let rec skip_element buf =
  let tok = Lexer_html.scan_element buf in
  match tok with
  | T.Relement _ 
  | T.Relement_empty _ 
    ->  ()
  | T.EOF _ -> raise End_of_scan
  | _ -> skip_element buf


let parse_atts buf =
 
  let rec parse_atts_lookahead next =
    match next with
    | T.Relement _  -> ( [], false )
    | T.Relement_empty _  -> ( [], true )
    | T.Name (_tok, n) ->
        (match skip_space false buf with
        | T.Eq _ ->
            (match skip_space true buf with
            | T.Name (_tok, v) ->
                let toks, is_empty =
                  parse_atts_lookahead (skip_space false buf) in
                ( (String.lowercase n, v) :: toks, is_empty )
            | T.Literal (_tok, v) ->
                let toks, is_empty =
                  parse_atts_lookahead (skip_space false buf) in
                ( (String.lowercase n,v) :: toks, is_empty )
            | T.EOF _ ->
                raise End_of_scan
            | T.Relement _ ->
                (* Illegal *)
                ( [], false )
            | T.Relement_empty _ ->
                (* Illegal *)
                ( [], true )
            | _ ->
                (* Illegal *)
                parse_atts_lookahead (skip_space false buf)
            )
        | T.EOF _ ->
            raise End_of_scan
        | T.Relement _ ->
            (* <tag name> <==> <tag name="name"> *)
            ( [ String.lowercase n, String.lowercase n ], false)
        | T.Relement_empty _ ->
            (* <tag name> <==> <tag name="name"> *)
            ( [ String.lowercase n, String.lowercase n ], true)
        | next' ->
            (* assume <tag name ... > <==> <tag name="name" ...> *)
            let toks, is_empty = 
              parse_atts_lookahead next' in
            ( ( String.lowercase n, String.lowercase n ) :: toks,
            is_empty)
        )
    | T.EOF _ ->
        raise End_of_scan
    | _ ->
        (* Illegal *)
        parse_atts_lookahead (skip_space false buf)
  in
  parse_atts_lookahead (skip_space false buf)

(* called for 'Special, not is_empty' tag categories *)
let rec parse_special name buf =
  (* Parse until </name> *)
  match Lexer_html.scan_special buf with
  | T.Lelementend (_tok, n) ->
      if String.lowercase n = name 
      then ""
      else "</" ^ n ^ parse_special name buf
  | T.EOF _ -> raise End_of_scan
  | T.Cdata (_tok, s) -> s ^ parse_special name buf
  | _ ->
      (* Illegal *)
      parse_special name buf

(*****************************************************************************)
(* Misc helpers *)
(*****************************************************************************)

let model_of ~dtd_hash element_name =
  if element_name = "" 
  then (Dtd.Everywhere, Dtd.Any)
  else
    try 
      (match Hashtbl.find dtd_hash element_name with 
      | (eclass, Dtd.Sub_exclusions(_,m)) -> eclass, m
      | m -> m
      )
    with Not_found -> (Dtd.Everywhere, Dtd.Any)

let exclusions_of ~dtd_hash element_name =
  if element_name = "" 
  then []
  else
    try
      (match Hashtbl.find dtd_hash element_name with
      | (eclass, Dtd.Sub_exclusions(l,_)) -> l
      | _ -> []
      )
    with Not_found -> []

let is_possible_subelement 
 ~dtd_hash parent_element parent_exclusions sub_element =
  let (sub_class, _) = model_of ~dtd_hash sub_element in
  let rec eval m =
    match m with
    | Dtd.Inline2     -> sub_class = Dtd.Inline
    | Dtd.Block2      -> 
        sub_class = Dtd.Block  || 
        sub_class = Dtd.Essential_block
    | Dtd.Flow       -> 
        sub_class = Dtd.Inline || 
        sub_class = Dtd.Block  || 
        sub_class = Dtd.Essential_block
    | Dtd.Elements l -> List.mem sub_element l
    | Dtd.Any        -> true
    | Dtd.Or (m1,m2)  -> eval m1 || eval m2
    | Dtd.Except (m1,m2) -> eval m1 && not (eval m2)
    | Dtd.Empty      -> false
    | Dtd.Special    -> false
    | Dtd.Sub_exclusions(_,_) -> assert false
  in
  (sub_class = Dtd.Everywhere) || 
  (
    (not (StringSet.mem sub_element parent_exclusions)) &&
      let (_, parent_model) = model_of ~dtd_hash parent_element in
      eval parent_model
  )

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

exception Found

type element_state = {
  name: Ast.tag;
  atts: (Ast.attr_name * Ast.attr_value) list;
  subs: Ast.html_tree list;

  excl: StringSet.t;
}

let parse2 file =
 Common.with_open_infile file (fun chan -> 
  let buf = Lexing.from_channel chan in

  let dtd = Dtd.html40_dtd in
  let dtd_hash = Common.hash_of_list dtd in

  let current = 
    ref { name = ""; atts = []; subs = []; excl = StringSet.empty} in

  let stack = Stack.create() in

  (* If the current element is not a possible parent element for sub_name,
   * search the parent element in the stack.
   * Either the new current element is the parent, or there was no
   * possible parent. In the latter case, the current element is the
   * same element as before.
   *)
  let unwind_stack sub_name =
    let backup = Stack.create() in
    let backup_el = !current in
    try
      while not (is_possible_subelement 
                    ~dtd_hash !current.name !current.excl sub_name) do

        (* Maybe we are not allowed to end the current element: *)
        let (current_class, _) = model_of ~dtd_hash !current.name in
        if current_class = Dtd.Essential_block 
        then raise Stack.Empty;

        (* End the current element and remove it from the stack: *)
        let grant_parent = Stack.pop stack in
        (* Save it; may we need it *)
        Stack.push grant_parent backup;  

        (* If gp_name is an essential element, we are not allowed to close
         * it implicitly, even if that violates the DTD.
         *)
        current := { grant_parent with subs = 
            Ast.Element (!current.name, !current.atts,  
                        List.rev !current.subs) :: grant_parent.subs;
        }
      done;
    with Stack.Empty ->
      (* It did not work! Push everything back to the stack, and
       * resume the old state.
       *)
      while Stack.length backup > 0 do
        Stack.push (Stack.pop backup) stack
      done;
      current := backup_el;
  in


  let rec parse_next() =
    let t = Lexer_html.scan_document buf in
    match t with
    | T.TComment info ->
        current := { !current with subs = 
          (Ast.Element("--",["contents",PI.str_of_info info],[]))::!current.subs
        };
        parse_next()
    | T.TDoctype info ->
        current := { !current with subs =
          (Ast.Element("!",["contents",PI.str_of_info info],[]))::!current.subs;
        };
        parse_next()
    | T.TPi info ->
        current := { !current with subs =
          (Ast.Element("?",["contents",PI.str_of_info info],[]))::!current.subs;
        };
        parse_next()

    | T.Lelement (_tok, name) ->
        let name = String.lowercase name in
        let (_, model) = model_of ~dtd_hash name in
        (match model with
        | Dtd.Empty ->
          let atts, _ = parse_atts buf in
          unwind_stack name;
          current := { !current with subs =
              (Ast.Element(name, atts, [])) :: !current.subs;
          };
          parse_next()
        | Dtd.Special ->
            let atts, is_empty = parse_atts buf in
            unwind_stack name;
            let data = 
              if is_empty 
              then ""
              else begin
                let d = parse_special name buf in
                (* Read until ">" *)
                skip_element buf;
                d
              end
            in
            current := { !current with subs = 
                (Ast.Element(name, atts, [Ast.Data data])) :: !current.subs;
            };
            parse_next()
        | _ ->
            let atts, is_empty = parse_atts buf in
            (* Unwind the stack until we find an element which can be
             * the parent of the new element:
             *)
            unwind_stack name;
            if is_empty then 
              (* Simple case *)
              current := { !current with
                subs = (Ast.Element(name, atts, [])) :: !current.subs;
              }
            else begin
              (* Push the current element on the stack, and this element
               * becomes the new current element:
               *)
              let new_excl = exclusions_of ~dtd_hash name in
              Stack.push !current stack;
              current := { 
                name = name;
                atts = atts;
                subs = [];
                excl = StringSet.union (StringSet.of_list new_excl) 
                  !current.excl;
              };
            end;
            parse_next()
        )
    | T.Cdata (_tok, data) ->
        current := { !current with subs =
            (Ast.Data data) :: !current.subs;
        };
        parse_next()
    | T.Lelementend (_tok, name) ->
        let name = String.lowercase name in
        (* Read until ">" *)
        skip_element buf;
        (* Search the element to close on the stack: *)
        let found = 
          (name = !current.name) ||
            try
              Stack.iter
                (fun { name = old_name} ->
                  if name = old_name 
                  then raise Found;
                  match model_of ~dtd_hash old_name with
                  |  Dtd.Essential_block, _ -> raise Not_found;
                    (* Don't close essential blocks implicitly *)
                  | _ -> ()
                )
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
          while !current.name <> name do
            let old_el = Stack.pop stack in
            current := { old_el with subs =
                (Ast.Element (!current.name, !current.atts,
                             List.rev !current.subs)) :: old_el.subs;
            };
          done;
          (* Remove one more element: the element containing the element
           * currently being closed.
           *)
          let old_el = Stack.pop stack in
          current := { old_el with subs =
              (Ast.Element (!current.name, !current.atts,
                           List.rev !current.subs)) :: old_el.subs;
          };
          (* Go on *)
          parse_next()
        end
    | T.EOF _ ->
        raise End_of_scan
    | (  T.Other _| T.Literal _| T.Eq _
       | T.Name _| T.Space _| T.Relement_empty _| T.Relement _)
        -> 
        (* pad: ???? *)
        parse_next ()
  in

  let xs = 
    try
      parse_next();  (* never returns. Will get a warning X *)
      assert false
    with End_of_scan ->
      (* Close all remaining elements: *)
      while Stack.length stack > 0 do
        let old_el = Stack.pop stack in
        current := { old_el with subs =
          Ast.Element (!current.name, !current.atts, 
                      List.rev !current.subs) :: old_el.subs;
        };
      done;
      List.rev !current.subs
  in
  (* TODO *)
  let tokens = [] in
  Ast.Element ("__root__", [], xs), tokens
 )

let parse a = 
  Common.profile_code "Parse_html.parse" (fun () -> parse2 a)

