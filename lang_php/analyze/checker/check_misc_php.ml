(* Yoann Padioleau
 *
 * Copyright (C) 2012 Facebook
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

open Ast_php
module Ast = Ast_php
module V = Visitor_php
module E = Error_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Misc checks (use of ';' instead of ':', wrong case sensitivity for
 * 'instanceOf' for now).
 *)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let check ast = 
  let visitor = V.mk_visitor { V.default_visitor with
    V.kstmt = (fun (k, _) st ->
      (match st with
      | Switch (tok, expr, cases) ->
          (match cases with
          | CaseList (obrace, tok2, cases, cbrace) ->
              cases +> List.iter (function
              | Case (_, _, case_separator, _)
              | Default (_, case_separator, _) ->
                  (* this is more something that should be fixed by a proper
                   * grammar
                   *)
                  let str = Ast.str_of_info case_separator in
                  (match str with
                  | ":" -> ()
                  | ";" -> E.warning case_separator E.CaseWithSemiColon
                      
                  | _ -> raise Impossible
                  )
              )
          | _ -> ()
          )
      | _ -> ()
      );
      (* recurse, call continuation *)
      k st
    );
    V.kexpr = (fun (k,_) e ->
      (match e with
      (* could do the case sensitivity check on all keywords, but
       * this one in particular seems to happen a lot
       *)
      | InstanceOf (e, tok, classname) ->
          let str = Ast.str_of_info tok in
          let lower = Common.lowercase str in
          if not (str =$= lower)
          then E.warning tok E.CaseSensitivityKeyword;
          k e
      | _ -> ()
      );
      (* recurse, call continuation *)
      k e
    );
  }
  in
  visitor (Program ast)

