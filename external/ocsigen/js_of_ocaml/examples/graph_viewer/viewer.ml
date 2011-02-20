(* Graph viewer
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open Scene

let pi = 4. *. atan 1.

(****)

let path_extent ctx fill stroke =
  if stroke <> None then Cairo.stroke_extents ctx
  else Cairo.fill_extents ctx

let compute_extent ctx e =
  Cairo.new_path ctx;
  match e with
    Path (cmd, fill, stroke) ->
      Array.iter
        (fun c ->
           match c with
             Move_to (x, y) ->
               Cairo.move_to ctx x y
           | Curve_to (x1, y1, x2, y2, x3, y3) ->
               Cairo.curve_to ctx x1 y1 x2 y2 x3 y3)
        cmd;
      path_extent ctx fill stroke
  | Ellipse (cx, cy, rx, ry, fill, stroke) ->
      Cairo.save ctx;
      Cairo.translate ctx cx cy;
      Cairo.scale ctx rx ry;
      Cairo.arc ctx 0. 0. 1. 0. (2. *. pi);
      Cairo.restore ctx;
      path_extent ctx fill stroke
  | Polygon (points, fill, stroke) ->
      Array.iteri
        (fun i (x, y) ->
           if i = 0 then Cairo.move_to ctx x y else Cairo.line_to ctx x y)
        points;
      Cairo.close_path ctx;
      path_extent ctx fill stroke
  | Text (x, y, txt, (font, font_size), fill, stroke) ->
      Cairo.select_font_face ctx font
        Cairo.FONT_SLANT_NORMAL Cairo.FONT_WEIGHT_NORMAL;
      Cairo.set_font_size ctx font_size;
      let ext = Cairo.text_extents ctx txt in
      (x -. ext.Cairo.text_width /. 2. -. 5.,
       y +. ext.Cairo.y_bearing -. 5.,
       x +. ext.Cairo.text_width /. 2. +. 5.,
       y +. ext.Cairo.y_bearing +. ext.Cairo.text_height +. 5.)

(****)

module Common = Viewer_common.F (struct
  type font = string * float
  type color = float * float * float
  type text = string
  let white = (1., 1., 1.)

  type ctx = Cairo.t

  let save = Cairo.save
  let restore = Cairo.restore

  let scale = Cairo.scale
  let translate = Cairo.translate

  let begin_path = Cairo.new_path
  let close_path = Cairo.close_path
  let move_to = Cairo.move_to
  let line_to = Cairo.line_to
  let curve_to = Cairo.curve_to
  let arc = Cairo.arc
  let rectangle = Cairo.rectangle

  let fill ctx (r, g, b) =
    Cairo.set_source_rgb ctx r g b; Cairo.fill_preserve ctx
  let stroke ctx (r, g, b) =
    Cairo.set_source_rgb ctx r g b; Cairo.stroke_preserve ctx
  let clip = Cairo.clip

  let perform_draw ctx fill_color stroke_color =
    begin match fill_color with
      Some c -> fill ctx c
    | None   -> ()
    end;
    begin match stroke_color with
      Some c -> stroke ctx c
    | None   -> ()
    end

  let draw_text ctx x y txt (font, font_size) fill stroke =
     Cairo.select_font_face ctx font
       Cairo.FONT_SLANT_NORMAL Cairo.FONT_WEIGHT_NORMAL;
     Cairo.set_font_size ctx font_size;
     let ext = Cairo.text_extents ctx txt in
     Cairo.move_to ctx
       (x -. ext.Cairo.x_bearing -. ext.Cairo.text_width /. 2.) y;
     Cairo.show_text ctx txt;
     perform_draw ctx fill stroke

  type window = GMisc.drawing_area
  type drawable = GDraw.drawable
  type pixmap = GDraw.pixmap
  let get_drawable w = new GDraw.drawable (w#misc#window)
  let make_pixmap window width height =
    GDraw.pixmap ~width ~height ~window ()
  let drawable_of_pixmap p = (p : GDraw.pixmap :> GDraw.drawable)
  let get_context p = Cairo_lablgtk.create p#pixmap
  let put_pixmap ~(dst : GDraw.drawable) ~x ~y ~xsrc ~ysrc ~width ~height p =
    dst#put_pixmap ~x ~y ~xsrc ~ysrc ~width ~height p#pixmap;

  (****)

  type rectangle = Gtk.rectangle = {x : int; y : int; width : int; height: int}
  let compute_extents = Scene_extents.compute
end)

open Common

(****)

let set_visible w vis =
  if vis then begin
    if not w#misc#visible then w#misc#show ()
  end else begin
    if w#misc#visible then w#misc#hide ()
  end

let scroll_view ?width ?height ?packing st =
  let table = GPack.table ?width ?height ~columns:2 ~rows:2 ?packing () in
  let hadj = GData.adjustment () in
  let hbar =
    GRange.scrollbar `HORIZONTAL ~adjustment:hadj
      ~packing:(table#attach ~left:0 ~top:1 ~fill:`BOTH ~expand:`NONE) () in
  hbar#misc#hide ();
  let vadj = GData.adjustment () in
  let vbar =
    GRange.scrollbar `VERTICAL ~adjustment:vadj
      ~packing:(table#attach ~left:1 ~top:0 ~fill:`BOTH ~expand:`NONE) () in
  vbar#misc#hide ();
  let display =
    GMisc.drawing_area
      ~packing:(table#attach ~left:0 ~top:0 ~fill:`BOTH ~expand:`BOTH) () in
  display#misc#set_can_focus true;
  display#misc#set_double_buffered false;

  let sadj =
    GData.adjustment ~upper:20. ~step_incr:1. ~page_incr:1. ~page_size:0. () in
  let zoom_steps = 8. in (* Number of steps to get a factor of 2 *)
  let set_zoom_factor f =
    let count = ceil (log f /. log 2. *. zoom_steps) in
    let f = 2. ** (count /. zoom_steps) in
    sadj#set_bounds ~upper:count ();
Format.eprintf "Factor: %f@." f;
    st.zoom_factor <- f
  in
  let get_scale () = 2. ** (sadj#value /. zoom_steps) /. st.zoom_factor in

  let update_scrollbars () =
    let a = display#misc#allocation in
    let scale = get_scale () in
    let aw = ceil (float a.Gtk.width /. scale) in
    let ah = ceil (float a.Gtk.height /. scale) in
    hadj#set_bounds ~step_incr:(aw /. 20.) ~page_incr:(aw /. 2.)
      ~page_size:(min aw st.st_width) ~upper:st.st_width ();
    let mv = st.st_width -. hadj#page_size in
    if hadj#value > mv then hadj#set_value mv;
    vadj#set_bounds ~step_incr:(ah /. 20.) ~page_incr:(ah /. 2.)
      ~page_size:(min ah st.st_height) ~upper:st.st_height ();
    let mv = st.st_height -. vadj#page_size in
    if vadj#value > mv then vadj#set_value mv;
    set_visible hbar (aw < st.st_width);
    set_visible vbar (ah < st.st_height)
  in

  let refresh () =
    invalidate_pixmap st.st_pixmap;
    GtkBase.Widget.queue_draw display#as_widget
  in

  ignore (display#event#connect#configure
    (fun ev ->
prerr_endline "CONFIGURE";
       update_scrollbars (); false));
  ignore (display#event#connect#map
    (fun ev ->
       let a = display#misc#allocation in
Format.eprintf "alloc: %d %d@." a.Gtk.width a.Gtk.height;
       let zoom_factor =
         max (st.st_width /. float a.Gtk.width)
             (st.st_height /. float a.Gtk.height)
       in
       set_zoom_factor zoom_factor;
       refresh ();
       update_scrollbars (); false));
  display#event#add [`STRUCTURE];

  ignore (display#event#connect#expose
    (fun ev ->
       let area = GdkEvent.Expose.area ev in
       let x = Gdk.Rectangle.x area in
       let y = Gdk.Rectangle.y area in
       let width = Gdk.Rectangle.width area in
       let height = Gdk.Rectangle.height area in
       redraw st (get_scale ()) hadj#value vadj#value
         display display#misc#allocation x y width height;
       true));

  ignore (hadj#connect#value_changed
    (fun () -> GtkBase.Widget.queue_draw display#as_widget));
  ignore (vadj#connect#value_changed
    (fun () -> GtkBase.Widget.queue_draw display#as_widget));
  let prev_scale = ref (get_scale ()) in
  let zoom_center = ref (0.5, 0.5) in
  ignore (sadj#connect#value_changed
    (fun () ->
       let scale = get_scale () in
       let r = (1. -. !prev_scale /. scale) in
Format.eprintf "update@.";
       hadj#set_value (hadj#value +. hadj#page_size *. r *. fst !zoom_center);
       vadj#set_value (vadj#value +. vadj#page_size *. r *. snd !zoom_center);
       prev_scale := scale;
       refresh ();
       update_scrollbars ()));

  let bump_scale x y v =
    let a = display#misc#allocation in
    let x = x /. float a.Gtk.width in
    let y = y /. float a.Gtk.height in
    if x >= 0. && x <= 1. && y >= 0. && y <= 1. then
      zoom_center := (x, y);
Format.eprintf "loc: %f %f@." x y;
    sadj#set_value (sadj#value +. v *. sadj#step_increment);
Format.eprintf "reset@.";
    zoom_center := (0.5, 0.5);
    true
  in
  (* Zoom using the mouse wheel *)
  ignore (display#event#connect#scroll
    (fun ev ->
       let x = GdkEvent.Scroll.x ev in
       let y = GdkEvent.Scroll.y ev in
       match GdkEvent.Scroll.direction ev with
         `UP   -> bump_scale x y 1.
       | `DOWN -> bump_scale x y (-1.)
       | _     -> false));
  display#event#add [`SCROLL];

  let pos = ref None in
  ignore (display#event#connect#button_press
    (fun ev ->
       display#misc#grab_focus ();
       if
         GdkEvent.get_type ev = `BUTTON_PRESS && GdkEvent.Button.button ev = 1
       then begin
         pos := Some (GdkEvent.Button.x ev, GdkEvent.Button.y ev);
       end;
       false));
  ignore (display#event#connect#button_release
    (fun ev ->
       if GdkEvent.Button.button ev = 1 then begin
         pos := None;
       end;
       false));
  ignore (display#event#connect#motion_notify
    (fun ev ->
       begin match !pos with
         Some (x, y) ->
           let (x', y') =
             if GdkEvent.Motion.is_hint ev then
               let (x', y') = display#misc#pointer in
               (float x', float y')
             else
               (GdkEvent.Motion.x ev, GdkEvent.Motion.y ev)
           in
           let offset a d =
             a#set_value (min (a#value +. d) (a#upper -. a#page_size)) in
           let scale = get_scale () in
           offset (hadj) ((x -. x') /. scale);
           offset (vadj) ((y -. y') /. scale);
           pos := Some (x', y')
       | None ->
           ()
       end;
       false));
  display#event#add
    [`BUTTON_PRESS; `BUTTON_RELEASE; `BUTTON1_MOTION; `POINTER_MOTION_HINT];

  ignore (display#event#connect#key_press
    (fun ev ->
       let keyval = GdkEvent.Key.keyval ev in
       if
         keyval = GdkKeysyms._Up
       then begin
         vadj#set_value (vadj#value -. vadj#step_increment);
         update_scrollbars ();
         true
       end else if
         keyval = GdkKeysyms._Down
       then begin
         vadj#set_value (vadj#value +. vadj#step_increment);
         update_scrollbars ();
         true
       end else if
         keyval = GdkKeysyms._Left
       then begin
         hadj#set_value (hadj#value -. hadj#step_increment);
         update_scrollbars ();
         true
       end else if
         keyval = GdkKeysyms._Right
       then begin
         hadj#set_value (hadj#value +. hadj#step_increment);
         update_scrollbars ();
         true
       end else if
         keyval = GdkKeysyms._Page_Up
       then begin
         vadj#set_value (vadj#value -. vadj#page_increment);
         update_scrollbars ();
         true
       end else if
         keyval = GdkKeysyms._Page_Down
       then begin
         vadj#set_value (vadj#value +. vadj#page_increment);
         update_scrollbars ();
         true
       end else if
         keyval = GdkKeysyms._0 || keyval = GdkKeysyms._agrave
       then begin
         let a = table#misc#allocation in
Format.eprintf "alloc: %d %d@." a.Gtk.width a.Gtk.height;
         let zf =
           max (st.st_width /. float a.Gtk.width)
               (st.st_height /. float a.Gtk.height)
         in
         let v = ceil (log zf /. log 2. *. zoom_steps) in
Format.eprintf "ZOOM: %f %f %f@." zf v sadj#upper;
         sadj#set_value (min sadj#upper (max 0. (sadj#upper -. v)));
         true
       end else if
         keyval = GdkKeysyms._1 || keyval = GdkKeysyms._ampersand
       then begin
         sadj#set_value (sadj#upper);
         true
       end else if
         keyval = GdkKeysyms._plus ||
         keyval = GdkKeysyms._equal ||
         keyval = GdkKeysyms._KP_Add
       then begin
         let (x, y) = display#misc#pointer in
         bump_scale (float x) (float y) 1.
       end else if
         keyval = GdkKeysyms._minus ||
         keyval = GdkKeysyms._KP_Subtract
       then begin
         let (x, y) = display#misc#pointer in
         bump_scale (float x) (float y) (-1.)
       end else
         false));
  display#event#add [`KEY_PRESS];

  object
    method scale_adjustment = sadj
  end

let create ?(full_screen=false) (x1, y1, x2, y2) scene =
  let st =
    { bboxes = [||]; scene = Scene.get scene; zoom_factor = 20.;
      st_x = x1; st_y = y1; st_width = x2 -. x1; st_height = y2 -. y1;
      st_pixmap = make_pixmap () } in

  let initial_size = 600 in
  let w = GWindow.window () in
  ignore (w#connect#destroy GMain.quit);
  let b = GPack.hbox ~packing:w#add () in
  let f =
    scroll_view ~width:initial_size ~height:initial_size
      ~packing:(b#pack ~expand:true) st in
  ignore
    (GRange.scale `VERTICAL ~inverted:true ~draw_value:false
       ~adjustment:(f#scale_adjustment) ~packing:b#pack ());

(*XXX Tooltips
area#misc#set_has_tooltip true;
ignore (area#misc#connect#query_tooltip (fun ~x ~y ~kbd tooltip ->
Format.eprintf "%d %d %b@." x y kbd; false));
*)

  (* Full screen mode *)
  let fullscreen = ref false in
  let toggle_fullscreen () =
    if !fullscreen then w#unfullscreen () else w#fullscreen ();
    fullscreen := not !fullscreen;
    true
  in
  if full_screen then ignore (toggle_fullscreen ());

  ignore (w#event#connect#key_press
    (fun ev ->
       let keyval = GdkEvent.Key.keyval ev in
       if keyval = GdkKeysyms._q || keyval = GdkKeysyms._Q then
         exit 0
       else if
         keyval = GdkKeysyms._F11 ||
         keyval = GdkKeysyms._F5 ||
         (keyval = GdkKeysyms._Escape && !fullscreen) ||
         keyval = GdkKeysyms._f || keyval = GdkKeysyms._F
       then
         toggle_fullscreen ()
       else
         false));

  w#show ()
