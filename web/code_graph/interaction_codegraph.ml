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
open Common2.ArithFloatInfix
open Common_client

module M = Model_codegraph
module DM = Dependencies_matrix_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let mouseclick
  (ctx:Canvas_helpers.context) (w: Model_codegraph.world_client) dblclick
  rpc_explain_cell main_service
  (elt: Dom_html.element Js.t) (ev: Dom_html.mouseEvent Js.t) =

  let device_x, device_y = Common_client.get_position elt ev in
  pr2 (spf "mouseclick device coord: %d x %d" device_x device_y);
  let (x, y) = ctx#device_to_user ~x:device_x ~y:device_y in
  pr2 (spf "mouseclick user coord: %f, %f" x y);

  (match M.find_region_at_user_point w ~x ~y with
  | None -> Lwt.return ()
  | Some x ->
      (match x with
      | M.Row i -> 
        pr2 (spf "clicking on row i");
        let (str, _kind) = w.M.m.DM.i_to_name.(i) in
        if dblclick 
        then
          Eliom_client.exit_to ~service:main_service
            (w.M.size, (w.M.project, str)) ();
        Lwt.return ()
(*
            (match GdkEvent.get_type ev, GdkEvent.Button.button ev with
            | `TWO_BUTTON_PRESS, 1 ->
                w.path <- add_path (DM.Expand node) w.path;
            | `BUTTON_PRESS, 3 ->
                pr2 (spf "right clicking on row i");
                let node = w.m.DM.i_to_name.(i) in
                w.path <- add_path (DM.Focus (node, DM.DepsOut)) w.path;
                recompute_matrix w;
                true

            | `BUTTON_RELEASE, _ |  _ ->
                false
            )
*)
      | M.Cell (i, j) -> 
          
          pr2 (spf "clicking on cell (%d, %d)" i j);
          lwt str = rpc_explain_cell (i, j) in
(*
          let deps = 
            DM.explain_cell_list_use_edges  (i, j) w.M.m w.M.gopti in
(*
          let ncount =
            if List.length deps > 100
            then 2
            else 50
          in
          let xs = Common.take_safe 1000 deps in
          let grouped_deps = 
            Graph_code.group_edges_by_files_edges xs 
              w.model.g_deprecated in
          let str = 
            grouped_deps +> List.map (fun ((f1, f2), deps) ->
              let final_file f =
                try 
                  Common.filename_without_leading_path w.model.root f
                with Failure _ -> f
              in
              let f1 = final_file f1 in
              let f2 = final_file f2 in
              spf "%s --> %s (%d)\n" f1 f2 (List.length deps) ^
                (Common.take_safe ncount deps +> List.map (fun (n1, n2) ->
                  spf "            %s --> %s" 
                    (Graph_code.string_of_node n1)  
                    (Graph_code.string_of_node n2)
                ) +> Common.join "\n"
                )
            ) +> Common.join "\n"
          in
*)
          let str = 
            deps +> Common.take_safe 50 +> List.map (fun (n1, n2) ->
              spf "            %s --> %s" 
                (Graph_code.string_of_node n1)  
                (Graph_code.string_of_node n2)
            ) +> Common.join "\n"
          in
*)
          pr2 str;
          Dom_html.window##alert (Js.string str);
          Lwt.return ();

(*
            | `BUTTON_PRESS, 3 ->
                pr2 (spf "right clicking on cell (%d, %d)" i j);
                if i = j
                then begin
                  let node = w.m.DM.i_to_name.(j) in
                  w.path <- add_path (DM.Focus (node, DM.DepsInOut)) w.path;
                  recompute_matrix w;
                  true
                end else
                  false

            | `BUTTON_RELEASE, _ | `TWO_BUTTON_PRESS, _ | _ ->
                false
            )
*)

      | M.Column j ->
          Lwt.return ()
(*
            (match GdkEvent.get_type ev, GdkEvent.Button.button ev with
            | `TWO_BUTTON_PRESS, 1 ->
                pr2 (spf "double clicking on column j");
                let node = w.m.DM.i_to_name.(j) in
                w.path <- add_path (DM.Expand node) w.path;
                recompute_matrix w;
                true
            | `BUTTON_PRESS, 3 ->
                pr2 (spf "right clicking on column j");
                let node = w.m.DM.i_to_name.(j) in
                w.path <- add_path (DM.Focus (node, DM.DepsIn)) w.path;
                recompute_matrix w;
                true

            | _ -> false
            )
  *)
      )
  )
