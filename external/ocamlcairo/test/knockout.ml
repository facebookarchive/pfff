(**************************************************************************)
(*  cairo-ocaml -- Objective Caml bindings for Cairo                      *)
(*  Copyright © 2004-2005 Olivier Andrieu                                 *)
(*                                                                        *)
(*  This code is free software and is licensed under the terms of the     *)
(*  GNU Lesser General Public License version 2.1 (the "LGPL").           *)
(**************************************************************************)

let pi = 4. *. atan 1.
let oval_path cr xc yc xr yr =
  let m = Cairo.get_matrix cr in
  Cairo.translate cr xc yc ;
  Cairo.scale cr 1. (yr /. xr) ;
  Cairo.move_to cr xr 0. ;
  Cairo.arc cr 0. 0. xr 0. (2. *. pi) ;
  Cairo.close_path cr ;
  Cairo.set_matrix cr m




let check_size = 32
let fill_checks c x y width height =
  Cairo.save c ; begin
    let check = 
      Cairo.surface_create_similar
	(Cairo.get_target c)
	Cairo.CONTENT_COLOR (2 * check_size) (2 * check_size) in

    begin
      let f_size = float check_size in
      let cr2 = Cairo.create check in
      Cairo.set_operator cr2 Cairo.OPERATOR_SOURCE ;
      Cairo.set_source_rgb cr2 0.4 0.4 0.4 ;
      Cairo.rectangle cr2 0. 0. (2. *. f_size) (2. *. f_size) ;
      Cairo.fill cr2 ;
      
      Cairo.set_source_rgb cr2 0.7 0.7 0.7 ;
      Cairo.rectangle cr2 x y f_size f_size ;
      Cairo.fill cr2 ;
      Cairo.rectangle cr2 (x +. f_size) (y +. f_size) f_size f_size ;
      Cairo.fill cr2
    end ;

    let pattern = Cairo.Pattern.create_for_surface check in
    Cairo.Pattern.set_extend pattern Cairo.EXTEND_REPEAT ;
    Cairo.set_source c pattern ;
    Cairo.rectangle c 0. 0. (float width) (float height) ;
    Cairo.fill c end ;
  Cairo.restore c



let draw_3circles c xc yc radius alpha =
  let subradius = radius *. (2. /. 3. -. 0.1) in

  List.iter (fun (r, g, b, off) ->
    Cairo.set_source_rgba c r g b alpha ;
    oval_path c 
      (xc +. radius /. 3. *. cos (pi *. (0.5 +. off)))
      (yc -. radius /. 3. *. sin (pi *. (0.5 +. off)))
      subradius subradius ;
    Cairo.fill c)
    [ 1., 0., 0., 0. ;
      0., 1., 0., 2./.3. ;
      0., 0., 1., 4./.3. ; ]


let draw c width height =
  let radius = 0.5 *. float (min width height) -. 10. in
  let xc = float width /. 2. in
  let yc = float height /. 2. in

  let sur = Cairo.get_target c in
  let overlay = Cairo.surface_create_similar sur Cairo.CONTENT_COLOR_ALPHA width height in
  let punch = Cairo.surface_create_similar sur Cairo.CONTENT_ALPHA width height in
  let circles = Cairo.surface_create_similar sur Cairo.CONTENT_COLOR_ALPHA width height in

  fill_checks c 0. 0. width height ;

  begin
    let cr_o = Cairo.create overlay in
    Cairo.set_source_rgb cr_o 0. 0. 0. ;
    oval_path cr_o xc yc radius radius ;
    Cairo.fill cr_o ;
    begin
      let cr_p = Cairo.create punch in
      draw_3circles cr_p xc yc radius 1.
    end ;
    Cairo.set_operator cr_o Cairo.OPERATOR_DEST_OUT ;
    Cairo.set_source_surface cr_o punch 0. 0. ;
    Cairo.paint cr_o ;
    begin
      let cr_c = Cairo.create circles in
      Cairo.set_operator cr_c Cairo.OPERATOR_OVER ;
      draw_3circles cr_c xc yc radius 0.5
    end ;
    Cairo.set_operator cr_o Cairo.OPERATOR_ADD ;
    Cairo.set_source_surface cr_o circles 0. 0.;
    Cairo.paint cr_o 
  end ;
  Cairo.set_source_surface c overlay 0. 0. ;
  Cairo.paint c

let expose d_area ev =
  let c = Cairo_lablgtk.create d_area#misc#window in
  let allocation = d_area#misc#allocation in
  draw c allocation.Gtk.width allocation.Gtk.height ;
  true

let main () =
  let w = GWindow.window ~title:"Knockout Groups" ~width:400 ~height:400 () in
  ignore (w#connect#destroy GMain.quit) ;

  let d = GMisc.drawing_area ~packing:w#add () in
  d#misc#set_double_buffered false ;
  ignore (d#event#connect#expose (expose d)) ;

  w#show () ;
  GMain.main ()

let _ = 
  if not !Sys.interactive
  then main ()
