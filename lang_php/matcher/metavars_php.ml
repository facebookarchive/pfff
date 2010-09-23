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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* todo? could want to remember the position in the pattern of the metavar
 * for error reporting ? so use a 'string Ast_php.wrap' ?
 *)
type mvar = string 

type metavars_binding = (mvar, binded_code) Common.assoc
 and binded_code =
   (* at some point we could bind class names, method names, or other
    * kinds of PHP entities
    *)
   | Expr of expr


let empty_environment = []
