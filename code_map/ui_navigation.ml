(*s: ui_navigation.ml *)
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

open Model2
module G = Gui
module Controller = Controller2

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Navigation *)
(*****************************************************************************)

(*s: go_back *)
let go_back w = 
  (* reset also the motion notifier ? less needed because
   * the next motion will reset it
   *)
  !Controller.paint_content_maybe_refresher +> Common.do_option (fun x ->
    GMain.Idle.remove x;
  );
  let old_dw = Common2.pop2 w.dw_stack in
  w.dw <- old_dw;
 
  let path = w.dw.current_root in
  !Controller._set_title (Controller.title_of_path path);
  !Controller._refresh_da();
  ()
(*e: go_back *)

(*s: go_dirs_or_file *)
let go_dirs_or_file ?(current_grep_query=None) w paths =
  let root = Common2.common_prefix_of_files_or_dirs paths in
  pr2 (spf "zooming in %s" (Common.join "|" paths));

  (* reset the painter? not needed because will call draw below
   * which will reset it
   *)
  let dw = w.dw in
  !Controller._set_title (Controller.title_of_path root);

  Common.push w.dw w.dw_stack;
  w.dw <- 
    Model2.init_drawing 
      ~width:dw.width
      ~height:dw.height
      w.treemap_func 
      dw.layers
      paths
      w.root_orig;
  (match current_grep_query with
  | Some h -> w.dw.current_grep_query <- h;
  (* wants to propagate the query so when right-click the query
   * is still there  *)
  | None -> w.dw.current_grep_query <- dw.current_grep_query;
  );
  View_overlays.paint_initial w.dw;
  View_mainmap.paint w.dw w.model;
  !Controller._refresh_da ();
  ()
(*e: go_dirs_or_file *)

(*e: ui_navigation.ml *)
