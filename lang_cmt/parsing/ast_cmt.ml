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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * This is mostly a wrapper around the ocaml compiler libs and its
 * typed ocaml tree. See the typed_ml file in this directory for
 * a copy paste of what is in the ocaml compiler source (and
 * was used to generate via ocamltarzan meta_ast_cmt.ml).
 *)

(*****************************************************************************)
(* The AST *)
(*****************************************************************************)

type ast = Cmt_format.cmt_infos
