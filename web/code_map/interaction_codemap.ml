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

module T = Treemap
module M = Model_codemap

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let mouseclick
  (ctx:Canvas_helpers.context) (w: Model_codemap.world_client) 
  main_service
  (elt: Dom_html.element Js.t) ev =
  let device_x, device_y = Common_client.get_position elt ev in
  pr2 (spf "mouseclick device coord: %d x %d" device_x device_y);
  let (x, y) = ctx#device_to_user ~x:device_x ~y:device_y in
  pr2 (spf "mouseclick user coord: %f, %f" x y);

  let user = { Figures.x = x; y = y } in
  let r_opt = M.find_rectangle_at_user_point w user in

  r_opt +> Common.do_option (fun (_r, _, r_englobing) ->
    let path = r_englobing.T.tr_label in
    Eliom_client.exit_to ~service:main_service
      (w.M.size, (w.M.project, path)) ();

  );

  Lwt.return ()
