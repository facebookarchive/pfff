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

open Ast_php

module Flag = Flag_analyze_php
module Ast = Ast_php

module V = Visitor_php
module T = Type_php
module N = Namespace_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * For now the analysis is rudimentary. Later it could be used
 * to resolve what method is called at a method call site, for instance
 * doing very basic data-flow analysis to see what values can have
 * a certain variable.
 *)

(*****************************************************************************)
(* Analysis helper for deadcode generator *)
(*****************************************************************************)

(* 
 * A common pattern of dynamic function call is to use a prefix
 * string concatenated to a variable as in :
 * 
 *   $process_func = 'staticrs_process_'.$type;
 * 
 *   $process_func($file, $root, $supported_locales, $unlocalized_hashes,
 *       $futures, $meta_data);
 * 
 * To avoid some false positives in the deadcode analysis, it is useful
 * to detect those patterns (to not consider for instance here the 
 * staticrs_process_xxx functions as deadcode). 
 * 
 * Right now I look at all funcvar calls, and then look in the body
 * of this callsite for assignments like the one above. 
 * 
 * Normally I should only go up to see some possible
 * assignements to the var. This would require a CFG.
 * 
 *)

let finding_function_pointer_prefix dvar ast = 
  let h = Hashtbl.create 101 in
  
  let hooks = { V.default_visitor with
    V.kexpr = (fun (k,vx) e ->
      match Ast_php.untype e with
      | Assign (var, _, e2) ->
          (match Ast_php.untype var with
          | Var (name, _scope) -> 
              let dvar2 = Ast_php.dname name in
              if dvar2 = dvar then begin
                match Ast_php.untype e2 with
                (* this case is already handled by the exact string
                 * heuristic in deadcode analysis, so it's commented.
                 *
                 * | Sc (C (String (x, _))) ->
                 * Hashtbl.replace h x true;
                 *)

                | Binary ((Sc (C (String (str, _))), _t1),
                          (BinaryConcat, _),
                          e2) ->
                    Hashtbl.replace h str true;

                | _ -> ()
              end;
              k e
          | _ -> k e
          )
      | _ -> k e
    );
  } in
  let visitor = V.mk_visitor hooks in
  visitor (Toplevel ast);
  Common.hashset_to_list h

