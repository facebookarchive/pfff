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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type annotation =
  | ProvidesModule of Module_js.module_
  | Other of string

(*****************************************************************************)
(* String -> annotation *)
(*****************************************************************************)
let extract_annotations str =
  raise Todo

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(* The returned parse_info is the one associated with the whole comment.
 * We use it in the tag generation. It's not very precise because
 * it's the info of the whole comment, and not just of the annotation,
 * but this should be good enough for the tags.
 *)
let annotations_of_program_with_comments asts_and_tokens =
  raise Todo
