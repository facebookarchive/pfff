(*s: editor_connection.ml *)
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Emacs *)
(*****************************************************************************)

let emacsclient_path_mac =
  "/home/pad/Dropbox/apps/Emacs.app/Contents/MacOS/bin/emacsclient"

let emacsclient_path = "emacsclient"

(* you need to have done a M-x server-start first *)
let run_emacsclient ~file ~line =
  Common.command2 (spf "%s -n %s" emacsclient_path file);
  Common.command2 (spf 
    "%s -e '(with-current-buffer (window-buffer (selected-window)) (goto-line %d))'"
    emacsclient_path line);

  ()


(*****************************************************************************)
(* Vi *)
(*****************************************************************************)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)

let open_file_in_current_editor ~file ~line =
  run_emacsclient ~file ~line
(*e: editor_connection.ml *)
