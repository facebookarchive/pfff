(*s: editor_connection.ml *)
(*s: Facebook copyright *)
(* Yoann Padioleau
 * 
 * Copyright (C) 2010-2012 Facebook
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
(*e: Facebook copyright *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Emacs *)
(*****************************************************************************)

(*s: emacs configuration *)
(*
let emacsclient_path_mac =
  "/home/pad/Dropbox/apps/Emacs.app/Contents/MacOS/bin/emacsclient"
*)

let emacsclient_path = ref "emacsclient"

(* you need to have done a M-x server-start first *)
let run_emacsclient ~file ~line =
  Common.command2 (spf "%s -n %s" !emacsclient_path file);
  Common.command2 (spf 
    "%s -e '(with-current-buffer (window-buffer (selected-window)) (goto-line %d))'"
    !emacsclient_path line);
  ()
(*e: emacs configuration *)

(*****************************************************************************)
(* Vi *)
(*****************************************************************************)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)

(*s: open_file_in_current_editor() *)
let open_file_in_current_editor ~file ~line =
  let (Model2.Line line) = line in
  (* emacs line numbers start at 1 *)
  let line = line + 1 in
  run_emacsclient ~file ~line
(*e: open_file_in_current_editor() *)
(*e: editor_connection.ml *)
