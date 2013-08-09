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

module G = Graph_code
module E = Database_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let pred a b c =
  Common.profile_code "G.pred" (fun () -> G.pred a b c)
let nodeinfo a b =
  Common.profile_code "G.nodeinfo" (fun () -> G.nodeinfo a b)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(* It can be difficult to trace the use of a field in languages like
 * PHP because one can do $o->fld and you don't know the type of $o
 * and so its class. But for the protected_to_private analysis,
 * it means the field is protected and so it can be used only
 * via a $this->xxx expression, which is easy to statically 
 * analyze.
 *)
let protected_to_private g =
  let found = ref false in
  g +> G.iter_nodes (fun node ->
    match node with
    | (s, E.Field) ->
      if s =$= "PlacesCheckinMoreTestCase.$method_name"
      then found := true;

      if !found then begin
      let props =
        try 
          let info = nodeinfo node g in
          info.G.props 
        with Not_found ->
          pr2 (spf "No nodeinfo for %s" (G.string_of_node node));
          [E.Privacy E.Private]
      in
      let privacy =
        props +> Common.find_some (function
        | E.Privacy x -> Some x
        | _ -> None
        )
      in
      (match privacy with
      | E.Private ->
        let users = pred node G.Use g in
        if null users
        then pr2 (spf "DEAD private field: %s" (G.string_of_node node))
      | E.Protected ->
        let parents = G.parents node g in
        if List.length parents > 1
        then begin pr2_gen node; pr2_gen parents end;
        let class_ = G.parent node g in
        if class_ =*= G.dupe 
        then pr2 (spf "Redefined field: %s" (G.string_of_node node))
        else begin
          let classname = fst class_ in
        
          let users = pred node G.Use g in
          if null users
          then pr2 (spf "DEAD protected field: %s" (G.string_of_node node))
          else 
            if users +> List.for_all (fun (s, kind) -> 
              s =~ (spf "^%s\\." classname)
            )
            then pr2 (spf "Protected to private candidate: %s"
                        (G.string_of_node node))
            else ()
        end
      | _ -> ()
      )
      end
    | _ -> ()
  )

