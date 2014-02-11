(* Yoann Padioleau
 *
 * Copyright (C) 2002-2008 Yoann Padioleau
 * Copyright (C) 2014 Facebook
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

open Parser_cpp
open Token_views_cpp

module TH = Token_helpers_cpp
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let is_braceised = function
  | Braceised   _ -> true
  | BToken _ -> false

(*****************************************************************************)
(* Context *)
(*****************************************************************************)
(* 
 * Most of the important contexts are introduced via some '{' '}'. To
 * disambiguate is it often enough to just look at a few tokens before the
 * '{'.
 * 
 * TODO harder now that have c++, can have function inside struct so need
 * handle all together. 
 * 
 * TODO So change token but do not recurse in
 * nested Braceised. maybe do via accumulator, don't use iter_token_brace.
 * 
 * TODO Also need remove the qualifier as they make
 * the sequence pattern matching more difficult.
 *)

let rec set_in_function_tag xs = 
 (* could try: ) { } but it can be the ) of a if or while, so 
  * better to base the heuristic on the position in column zero.
  * Note that some struct or enum or init put also their { in first column
  * but set_in_other will overwrite the previous InFunction tag.
  *)
 let rec aux xs = 
  match xs with
  | [] -> ()
(*TODOC++ext: now can have some const or throw between 
  => do a view that filter them first ?
*)

  (* ) { and the closing } is in column zero, then certainly a function *)
(*TODO1 col 0 not valid anymore with c++ nestedness of method *)
  | BToken ({t=TCPar _;_})::(Braceised (body, tok1, Some tok2))::xs 
      when tok1.col <> 0 && tok2.col = 0 -> 
      body +> List.iter (iter_token_brace (fun tok -> 
        tok.where <- InFunction::tok.where;
      ));
      aux xs

  | (BToken x)::xs -> aux xs

(*TODO1 not valid anymore with c++ nestedness of method *)
  | (Braceised (body, tok1, Some tok2))::xs 
      when tok1.col = 0 && tok2.col = 0 -> 
      body +> List.iter (iter_token_brace (fun tok -> 
        tok.where <- InFunction::tok.where;
      ));
      aux xs
  | Braceised (body, tok1, tok2)::xs -> 
      aux xs
 in
 aux xs

let rec set_in_other xs = 
  match xs with 
  | [] -> ()

  (* enum x { } *)
  | BToken ({t=Tenum _;_})::BToken ({t=TIdent _;_})
    ::Braceised(body, tok1, tok2)::xs 
  | BToken ({t=Tenum _;_})
    ::Braceised(body, tok1, tok2)::xs 
    -> 
      body +> List.iter (iter_token_brace (fun tok -> 
        tok.where <- InEnum::tok.where;
      ));
      set_in_other xs

  (* struct/union/class x { } *)
  | BToken ({t=tokstruct; _})::BToken ({t= TIdent (s,_); _})
    ::Braceised(body, tok1, tok2)::xs when TH.is_classkey_keyword tokstruct -> 
      body +> List.iter (iter_token_brace (fun tok -> 
        tok.where <- (InClassStruct s)::tok.where;
      ));
      set_in_other xs

  (* struct/union/class x : ... { } *)
  | BToken ({t= tokstruct; _})::BToken ({t=TIdent _; _})
    ::BToken ({t=TCol _;_})::xs when TH.is_classkey_keyword tokstruct -> 

      (try 
        let (before, elem, after) = Common2.split_when is_braceised xs in
        (match elem with 
        | Braceised(body, tok1, tok2) -> 
            body +> List.iter (iter_token_brace (fun tok -> 
              tok.where <- InInitializer::tok.where;
            ));
            set_in_other after
        | _ -> raise Impossible
        )
      with Not_found ->
        pr2 ("PB: could not find braces after struct/union/class x : ...");
      )

  (* = { } *)
  | BToken ({t=TEq _; _})
    ::Braceised(body, tok1, tok2)::xs -> 
      body +> List.iter (iter_token_brace (fun tok -> 
        tok.where <- InInitializer::tok.where;
      ));
      set_in_other xs


  (* recurse *)
  | BToken _::xs -> set_in_other xs
  | Braceised(body, tok1, tok2)::xs -> 
      body +> List.iter set_in_other;
      set_in_other xs

(* TODO: handle C++ context for real, and Parameter, and etc *)
let set_context_tag xs = 
  begin
    (* order is important *)
    set_in_function_tag xs;
    set_in_other xs;
  end
