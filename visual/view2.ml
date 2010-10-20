(*s: view2.ml *)
(*s: Facebook copyright *)
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
(*e: Facebook copyright *)

open Common

module G = Gui
module K = GdkKeysyms
module GR = Gdk.Rectangle

module F = Figures
module T = Treemap

module CairoH = Cairo_helpers

open Figures (* for the fields *)
open Model2 (* for the fields *)
module M = Model2

(* floats are the norm in graphics *)
open Common.ArithFloatInfix

module Style = Style2
module Draw = Draw2

module Flag = Flag_visual

module Db = Database_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2, pr2_once = Common.mk_pr2_wrappers Flag.verbose_visual

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

(*s: view globals *)
(* when some widgets need to access other widgets *)

(* Note that because we use toplevels 'let' for the GUI elements below,
 * Gtk must have also been initialized via a toplevel element, or
 * initialized by including gtkInit.cmo earlier in the linking command.
 *)

let statusbar = 
  GMisc.statusbar () 

let ctx = 
  statusbar#new_context "main" 

let statusbar_addtext s = 
  ctx#push s +> ignore

let title_of_path s = "Pfff_visual: " ^ s

let _set_title = ref (fun s ->
  failwith "_set_title not defined"
)
let _refresh_da = ref (fun () ->
  failwith "_refresh_da not defined"
)

let dw_stack = ref []

(* ugly *)
let root_orig () = 
  (Common.last !dw_stack).M.root

let paint_content_maybe_refresher = ref None
let current_rects_to_draw = ref []


let current_r = ref None

let current_motion_refresher = ref None

let interface_doc = "
This tool displays a \"code map\" of a software project using
Treemaps. \"Treemaps display hierarchical (tree-structured) data as a
set of nested rectangles. Each branch of the tree is given a 
rectangle, which is then tiled with smaller rectangles representing
sub-branches. A leaf node's rectangle has an area proportional 
to a specified dimension on the data.
\" - http://en.wikipedia.org/wiki/Treemapping:

In our case the dimension is the size of the file.
Moreover each file is colored according to its
\"category\": display code, third party code, etc.
See the legend. We use basic heuristcs based on the
name of the files and directory.

Files and directories are also sorted alphabetically
and partially ordered from top to bottom and left to right. 
So a toplevel 'zzz' subdirectory should be located at the bottom 
right of the screen.

As you move the mouse, the blue highlighted areas are the next
level of directories.

Double-clicking zooms in on the blue-highlighted area.
Right-clicking zoom directly to the file under the cursor.
Middle-clicking open the file under the cursor in your
favourite editor (provided you have M-x server-start
and have emacsclient in your path).

"
(*e: view globals *)

(*****************************************************************************)
(* Scaling *)
(*****************************************************************************)

(*s: zoom_pan_scale_map *)
let zoom_pan_scale_map cr dw =
  Cairo.scale cr 
    (dw.zoom * (float_of_int dw.width / T.xy_ratio))
    (dw.zoom * (float_of_int dw.height))
  ;
  (* I first scale and then translate as the xtrans are in user coordinates *)
  Cairo.translate cr dw.xtrans dw.ytrans;
  (* TODO clipping  Cairo.rectangle cr ~x:dw.xtrans ~y: *)
  ()
(*e: zoom_pan_scale_map *)

(*s: scale_minimap *)
let scale_minimap cr dw =
  (* no zoom, no pan, no clippnig *)
  Cairo.translate cr 0.0 0.0;
  Cairo.scale cr 
    (1.0 * (float_of_int dw.width_minimap / T.xy_ratio))
    (1.0 * (float_of_int dw.height_minimap))
(*e: scale_minimap *)

(*s: with_map *)
let with_map dw f = 
  let cr = Cairo_lablgtk.create dw.pm#pixmap in
  zoom_pan_scale_map cr dw;
  f cr
(*e: with_map *)

(*s: with_minimap *)
let with_minimap dw f =
  let cr = Cairo_lablgtk.create dw.pm_minimap#pixmap in
  scale_minimap cr dw;
  f cr
(*e: with_minimap *)

(*s: device_to_user_area *)
(* still needed ? reuse helper functions above ? *)
let device_to_user_area dw = 
  with_map dw (fun cr ->

    let device_point = { Cairo. x = 0.0; y = 0.0 } in
    let user_point1 = Cairo.device_to_user cr device_point in
    let device_point = { Cairo.x = float_of_int dw.width; 
                         Cairo.y = float_of_int dw.height; } in
    let user_point2 = Cairo.device_to_user cr device_point in
    
    { F.p = CairoH.cairo_point_to_point user_point1;
      F.q = CairoH.cairo_point_to_point user_point2;
    }
  )
(*e: device_to_user_area *)

(*****************************************************************************)
(* Painting *)
(*****************************************************************************)

(*s: paint *)
let context_of_drawing dw = 
  { Draw.
    nb_rects_on_screen = dw.nb_rects;
    model = dw.model;
    settings = dw.settings;
    current_grep_query = dw.current_grep_query;
  } 

let paint_content_maybe_rect ~user_rect dw rect =
  let cr = Cairo_lablgtk.create dw.pm#pixmap in
  zoom_pan_scale_map cr dw;

  let context = context_of_drawing dw in

  Draw.draw_treemap_rectangle_content_maybe ~cr  ~clipping:user_rect  ~context
    rect;

  (* have to redraw the label *)
  Draw.draw_treemap_rectangle_label_maybe ~cr ~zoom:dw.zoom ~color:"black" 
    rect;
  ()

(* todo: deadlock:  M.locked (fun () ->  ) dw.M.model.M.m *)
let lazy_paint ~user_rect dw () =
  pr2 "Lazy Paint";
  let start = Unix.gettimeofday () in
  while Unix.gettimeofday () - start < 0.3 do
    match !current_rects_to_draw with
    | [] -> ()
    | x::xs ->
        current_rects_to_draw := xs;
        pr2 (spf "Drawing: %s" (x.T.tr_label));
        paint_content_maybe_rect ~user_rect dw x;
  done;
  !_refresh_da ();
  if !current_rects_to_draw = []
  then false
  else true


let paint2 dw = 

  !paint_content_maybe_refresher +> Common.do_option (fun x ->
    GMain.Idle.remove x;
  );
  current_rects_to_draw := [];

  let cr = Cairo_lablgtk.create dw.pm#pixmap in
  dw.pm#rectangle 
    ~x:0 ~y:0 
    ~width:dw.width ~height:dw.height 
    ~filled:true () ;

  pr2 (spf "paint, with zoom = %f, xtrans = %f, ytrans = %f" 
          dw.zoom dw.xtrans dw.ytrans);
  let user_rect = device_to_user_area dw in
  pr2 (F.s_of_rectangle user_rect);

  zoom_pan_scale_map cr dw;

  let rects = dw.treemap in
  let nb_rects = dw.nb_rects in

  (* phase 1, draw the rectangles *)
  rects +> List.iter (Draw.draw_treemap_rectangle ~cr);

  (* phase 2, draw the labels, if have enough space *)
  rects +> List.iter 
    (Draw.draw_treemap_rectangle_label_maybe ~cr ~zoom:dw.zoom ~color:"black");

  (* phase 3, draw the content, if have enough space *)
  if not dw.in_dragging && nb_rects < !Flag.threshold_nb_rects_draw_content
    (* draw_content_maybe calls nblines which is quite expensive so
     * want to limit it *)
  then begin
    current_rects_to_draw := rects;
    paint_content_maybe_refresher := 
      Some (Gui.gmain_idle_add ~prio:3000 (lazy_paint ~user_rect dw));
  end;

  (* also clear the overlay *)
  let cr_overlay = Cairo.create dw.overlay in
  CairoH.clear cr_overlay;

  ()

let paint dw = 
  Common.profile_code2 "View.paint" (fun () -> paint2 dw)
(*e: paint *)

(*s: paint_minimap *)
let paint_minimap2 dw = 
  let cr = Cairo_lablgtk.create dw.pm_minimap#pixmap in
  dw.pm_minimap#rectangle 
    ~x:0 ~y:0 
    ~width:dw.width_minimap ~height:dw.height_minimap
    ~filled:true () ;

  scale_minimap cr dw;

  let rects = dw.treemap in
  (* draw the rectangles *)
  rects +> List.iter (Draw.draw_treemap_rectangle ~cr);

  (* draw the labels, if have enough space *)
  rects +> List.iter 
    (Draw.draw_treemap_rectangle_label_maybe ~cr ~zoom:1.0 ~color:"black");

  (* draw the zoom rectangle *)
  let user_rect = device_to_user_area dw in
  CairoH.draw_rectangle_figure ~cr ~color:"white" user_rect;
  ()


let paint_minimap dw = 
  Common.profile_code2 "View.paint minimap" (fun () -> paint_minimap2 dw)
(*e: paint_minimap *)

(*s: paint_legend *)
let paint_legend ~cr =

  Cairo.select_font_face cr "serif" 
    Cairo.FONT_SLANT_NORMAL Cairo.FONT_WEIGHT_NORMAL;
  let size = 25.  in

  Cairo.set_font_size cr (size * 0.6);

  Cairo.set_source_rgba cr 0. 0. 0.    1.0;
  
  let archis = Archi_code.source_archi_list in

  let grouped_archis = archis +> Common.group_by_mapped_key (fun archi ->
    let color = Treemap_pl.color_of_source_archi archi in
    (* I tend to favor the darker variant of the color in treemap_pl.ml hence
     * the 3 below
     *)
    let color = color ^ "3" in
    color
  )
  in

  
  grouped_archis +> Common.index_list_1 +> List.iter (fun ((color,kinds), i) ->
    
    let x = 10. in
    
    let y = float_of_int i * size in

    let w = size in
    let h = size in

    CairoH.fill_rectangle ~cr ~color ~x ~y ~w ~h ();

    let s = 
      kinds +> List.map Archi_code.s_of_source_archi +> Common.join ", " in

    Cairo.set_source_rgba cr 0. 0. 0.    1.0;
    Cairo.move_to cr (x + size * 2.) (y + size * 0.8);
    Cairo.show_text cr s;
  );
  ()
(*e: paint_legend *)

(*****************************************************************************)
(* Overlays *)
(*****************************************************************************)

(* ---------------------------------------------------------------------- *)
(* The current filename *)
(* ---------------------------------------------------------------------- *)

(*s: draw_label_overlay *)
let draw_label_overlay ~cr_overlay ~dw ~x ~y r =

  let txt = r.T.tr_label in

  let readable_txt = 
    if dw.root = txt (* when we are fully zoomed on one file *)
    then "root"
    else 
      Common.filename_without_leading_path dw.root txt in

  let readable_txt =
    if String.length readable_txt > 15
    then Filename.basename readable_txt
    else readable_txt
  in

  Cairo.select_font_face cr_overlay "serif" 
    Cairo.FONT_SLANT_NORMAL Cairo.FONT_WEIGHT_NORMAL;
  Cairo.set_source_rgba cr_overlay 1. 1. 1.    1.0;
  Cairo.set_font_size cr_overlay Style2.font_size_filename_cursor;
      
  let extent = CairoH.text_extents cr_overlay readable_txt in
  let tw = extent.Cairo.text_width in
  let _th = extent.Cairo.text_height in


  Cairo.move_to cr_overlay (x - tw / 2.) (y);
  CairoH.show_text cr_overlay readable_txt;
  
  (*
  Cairo.set_source_rgb cr_overlay 0.3 0.3 0.3;
  Cairo.move_to cr_overlay x y;
  Cairo.line_to cr_overlay (x + 10.) (y + 10.);
  Cairo.stroke cr_overlay;
  *)
  ()
(*e: draw_label_overlay *)
(* ---------------------------------------------------------------------- *)
(* The current rectangles *)
(* ---------------------------------------------------------------------- *)

(*s: draw_rectangle_overlay *)
let draw_rectangle_overlay ~cr_overlay ~dw (r, middle, r_englobing) =
  Cairo.save cr_overlay;
  zoom_pan_scale_map cr_overlay dw;
  CairoH.draw_rectangle_figure ~cr:cr_overlay ~color:"white" r.T.tr_rect;

  CairoH.draw_rectangle_figure
    ~cr:cr_overlay ~color:"blue" r_englobing.T.tr_rect;
  Draw.draw_treemap_rectangle_label_maybe 
    ~cr:cr_overlay ~color:"blue" ~zoom:dw.zoom r_englobing;

  middle +> Common.index_list_1 +> List.iter (fun (r, i) ->
    let color = 
      match i with
      | 1 -> "grey70"
      | 2 -> "grey40"
      | _ -> spf "grey%d" (max 1 (50 -.. (i *.. 10)))
    in
    CairoH.draw_rectangle_figure
      ~cr:cr_overlay ~color r.T.tr_rect;
    Draw.draw_treemap_rectangle_label_maybe 
      ~cr:cr_overlay ~color ~zoom:dw.zoom r;
  );
    
  Cairo.restore cr_overlay;
  ()
(*e: draw_rectangle_overlay *)
(* ---------------------------------------------------------------------- *)
(* The selected rectangles *)
(* ---------------------------------------------------------------------- *)

(*s: draw_searched_rectangles *)
let draw_searched_rectangles ~cr_overlay ~dw =
  Cairo.save cr_overlay;
  zoom_pan_scale_map cr_overlay dw;

  dw.current_searched_rectangles +> List.iter (fun r ->
    CairoH.draw_rectangle_figure ~cr:cr_overlay 
      ~color:"yellow" r.T.tr_rect
  );
  (* 
   * would also like to draw not matching rectangles
   * bug the following code is too slow on huge treemaps. 
   * Probably because it is doing lots of drawing and alpha
   * computation.
   *
   * old:
   * let color = Some "grey3" in
   * Draw.draw_treemap_rectangle ~cr:cr_overlay 
   * ~color ~alpha:0.3
   * r
   *)
  Cairo.restore cr_overlay;
  ()
(*e: draw_searched_rectangles *)

(* ---------------------------------------------------------------------- *)
(* The magnifying glass *)
(* ---------------------------------------------------------------------- *)

(*s: zoomed_surface_of_rectangle *)
let _hmemo_surface = Hashtbl.create 101
let zoomed_surface_of_rectangle dw r =
  Common.memoized _hmemo_surface (r.T.tr_label, dw.zoom) (fun () ->

  let user_rect = device_to_user_area dw in
  
  let sur =
    Cairo.surface_create_similar (CairoH.surface_of_pixmap dw.pm)
      (* subtle: can not use dw.width or dw.height here because at
       * the zoom level we will proceed, the whole file would probably not
       * feel on the full screen. If it does not fit, then having a too
       * small surface mean parts of the rendering of the file will not
       * be stored.
       *)
      Cairo.CONTENT_COLOR_ALPHA 9000 9000;
  in
  let cr = Cairo.create sur in

  (* simplify the drawing context, draw on 0 x 0 a rectangle that itself
   * starts at 0 x 0
   *)
  let dw' = { dw with 
    zoom = dw.zoom * Style.zoom_factor_incruste_mode; (* CONFIG *)
    xtrans = 0.; ytrans = 0.;
  } 
  in
  zoom_pan_scale_map cr dw';
  (* a normalized rectangle that starts at 0 x 0 *)
  let rect = r.T.tr_rect in
  let rect' = { 
    F.p = { F. x = 0.; y = 0.;};
    F.q = { F. x = F.rect_width rect; y = F.rect_height rect;};
  }
  in
  let r' = { r with T.tr_rect = rect' } in

  let user_width = F.rect_width rect in
  let user_height = F.rect_height rect in

  let device_width  = CairoH.user_to_device_distance_x cr user_width in
  let device_height = CairoH.user_to_device_distance_y cr user_height in
  (* now have on the surface the same thing we would have got if we had
   * zoomed a lot.
   *)

  let context = context_of_drawing dw in
  let context = { context with Draw.nb_rects_on_screen = 1 } in

  Draw.draw_treemap_rectangle ~cr ~alpha:0.9 r';
  Draw.draw_treemap_rectangle_content_maybe ~cr ~context ~clipping:user_rect r';
  sur, device_width, device_height
  )



let draw_zoomed_overlay ~cr_overlay ~user ~dw ~x ~y r =

  let percent_x = 
    (user.Cairo.x - r.T.tr_rect.p.F.x) / F.rect_width r.T.tr_rect in
  let percent_y = 
    (user.Cairo.y - r.T.tr_rect.p.F.y) / F.rect_height r.T.tr_rect in
  
  let zoomed_surface, zoomed_device_width, zoomed_device_height = 
    zoomed_surface_of_rectangle dw r
  in
  Cairo.set_operator cr_overlay Cairo.OPERATOR_OVER;
  (* old:
     Cairo.set_source_surface cr_overlay zoomed_surface (x - 100.) (y - 100.);
     Cairo.paint cr_overlay;
  *)
  (* see http://cairographics.org/FAQ/#paint_from_a_surface *)
  let dest_x = (x + 20.) in
  let dest_y = (y + 20.) in
  let width = float_of_int dw.width / 2.5 in
  let height = float_of_int dw.height / 2.5 in
  let source_x = 
    Common.borne
      ~min:0. ~max:(zoomed_device_width - width)
      ((percent_x * zoomed_device_width) - 140.)
  in
  let source_y = 
    Common.borne
      ~min:0. ~max:(zoomed_device_height - height)
      ((percent_y * zoomed_device_height) - 30.)
  in

  pr2 (spf "at x%%= %.3f, y%% = %.3f, zoom_w = %.3f, zoom_h = %.3f" 
          percent_x percent_y
          zoomed_device_width
          zoomed_device_height
  );

  Cairo.set_source_surface cr_overlay zoomed_surface
    (dest_x -. source_x) (dest_y -. source_y);
  Cairo.rectangle cr_overlay dest_x dest_y width height;
  Cairo.fill cr_overlay;
  ()
(*e: zoomed_surface_of_rectangle *)

(*****************************************************************************)
(* Layering *)
(*****************************************************************************)

(* ---------------------------------------------------------------------- *)
(* The main-map *)
(* ---------------------------------------------------------------------- *)

(*s: assemble_layers *)
(* Composing the "layers". See cairo/tests/knockout.ml example.
 * Each move of the cursor will call assemble_layers which does all
 * those pixels copying but this is fast enough.
 *)
let assemble_layers cr_final dw ~width ~height =

  let surface_src = CairoH.surface_of_pixmap dw.pm in

  Cairo.set_operator cr_final Cairo.OPERATOR_OVER;
  Cairo.set_source_surface cr_final surface_src 0. 0.;
  Cairo.paint cr_final;

  Cairo.set_operator cr_final Cairo.OPERATOR_OVER;
  Cairo.set_source_surface cr_final dw.overlay 0. 0.;
  Cairo.paint cr_final;
  ()
(*e: assemble_layers *)

(*s: expose *)
let expose2 da dw_ref ev = 
  let dw = !dw_ref in

  (* opti: don't 'paint dw;' if not needed! painting is the computation
   * heavy function. expose just copy the "canvas" layers  
   *)

  (* todo? equivalent to   
   *  let allocation = d_area#misc#allocation in
   * allocation.Gtk.width allocation.Gtk.height
   * ?
   *)
  let area = GdkEvent.Expose.area ev in
  let width = GR.width area +> float_of_int in
  let height = GR.height area +> float_of_int in
  (* todo? use ? it can optimise things ? *)
  let _x = GR.x area in
  let _y = GR.y area in

  let gwin = da#misc#window in
  let cr = Cairo_lablgtk.create gwin in
  assemble_layers cr dw ~width ~height;
  (* old:
  Common.profile_code "View.put_pixmap" (fun () ->
    let d = new GDraw.drawable gwin in
    d#put_pixmap ~x ~y ~xsrc:x ~ysrc:y ~width ~height dw.pm#pixmap;
  );
  *)
  true

let expose a b c = 
  Common.profile_code2 "View.expose" (fun () -> expose2 a b c)
(*e: expose *)

(*s: configure *)
let configure2_bis da dw_ref ev = 
  let dw = !dw_ref in

  let w = GdkEvent.Configure.width ev in
  let h = GdkEvent.Configure.height ev in

  dw.width <- w;
  dw.height <- h;
  dw.pm <- Model2.new_pixmap dw.width dw.height;
  let cr_src = Cairo_lablgtk.create dw.pm#pixmap in
  let sur_src = Cairo.get_target cr_src in
  dw.overlay <- 
    Cairo.surface_create_similar sur_src 
    Cairo.CONTENT_COLOR_ALPHA w h;
    
  paint dw;
  true

(* ugly: for some unknown reason configure get called twice at
 * the beginning of the program
 *)
let first_call = ref true
let configure2 a b c =
  (* should probably do is_old_gtk() *)
  if !first_call && CairoH.is_old_cairo () 
  then begin first_call := false; true end
  else 
    configure2_bis a b c

let configure a b c =
  Common.profile_code2 "View.configure" (fun () -> configure2 a b c)
(*e: configure *)

(* ---------------------------------------------------------------------- *)
(* The mini-map *)
(* ---------------------------------------------------------------------- *)

(*s: expose_minimap *)
let expose_minimap da dw_ref ev = 
  let dw = !dw_ref in

  (* todo? opti? don't paint if not needed ? *)
  (*paint_minimap dw;*)

  let area = GdkEvent.Expose.area ev in
  let x = GR.x area in
  let y = GR.y area in
  let width = GR.width area in
  let height = GR.height area in

  let gwin = da#misc#window in
  let d = new GDraw.drawable gwin in

  Common.profile_code2 "View.put_pixmap mini" (fun () ->
    d#put_pixmap ~x ~y ~xsrc:x ~ysrc:y ~width ~height 
      dw.pm_minimap#pixmap;
  );
  true
(*e: expose_minimap *)

(*s: configure_minimap *)
let configure_minimap da2 dw_ref ev = 
  let dw = !dw_ref in

  let w = GdkEvent.Configure.width ev in
  let h = GdkEvent.Configure.height ev in
  dw.width_minimap <- w;
  dw.height_minimap <- h;
  dw.pm_minimap <- new_pixmap dw.width_minimap dw.height_minimap;
  true
(*e: configure_minimap *)

(* ---------------------------------------------------------------------- *)
(* The legend *)
(* ---------------------------------------------------------------------- *)
(*s: expose_legend *)
let expose_legend da dw_ref ev = 

  let gwin = da#misc#window in
  let cr = Cairo_lablgtk.create gwin in
  paint_legend ~cr;
  true
(*e: expose_legend *)

(*****************************************************************************)
(* Events *)
(*****************************************************************************)

(* ---------------------------------------------------------------------- *)
(* Navigation *)
(* ---------------------------------------------------------------------- *)

(*s: go_back *)
let go_back dw_ref = 

  (* reset also the motion notifier ? less needed because
   * the next motion will reset it
   *)
  !paint_content_maybe_refresher +> Common.do_option (fun x ->
    GMain.Idle.remove x;
  );

  let old_dw = Common.pop2 dw_stack in
  dw_ref := old_dw;
  
  let path = !dw_ref.root in
  !_set_title (title_of_path path);

  !_refresh_da();
  ()
(*e: go_back *)

(*s: go_dirs_or_file *)
let go_dirs_or_file ?(current_entity=None) ?(current_grep_query=None) 
  dw_ref paths =

  let root = Common.common_prefix_of_files_or_dirs paths in
  pr2 (spf "zooming in %s" (Common.join "|" paths));

  (* reset the painter ? not needed because will call draw below
   * which will reset it
   *)

  let dw = !dw_ref in
  !_set_title (title_of_path root);

  (* the release event will arrive later on the new dw so
   * has to first set the context good for the old dw
   *)
  !dw_ref.in_dragging <- false;
  
  Common.push2 !dw_ref dw_stack;

  dw_ref := 
    Model2.init_drawing 
      ~width:dw.width
      ~height:dw.height
      ~width_minimap:dw.width_minimap
      ~height_minimap:dw.height_minimap
      dw.treemap_func 
      dw.model 
      paths;
  !dw_ref.current_entity <- current_entity;
  (match current_grep_query with
  | Some h ->
      !dw_ref.current_grep_query <- h;
  | None ->
      (* wants to propagate the query so when right-click the query
       * is still there
       *)
      !dw_ref.current_grep_query <- dw.current_grep_query;
  );
  paint !dw_ref;
  !_refresh_da ();
  ()
(*e: go_dirs_or_file *)

(* ---------------------------------------------------------------------- *)
(* Search *)
(* ---------------------------------------------------------------------- *)

(*s: dialog_search_def *)
let dialog_search_def model = 
  let idx = (fun () -> 
    let model = Model2.async_get model in
    model.Model2.big_grep_idx 
  )
  in
  let entry = 
    Completion2.my_entry_completion_eff 
      ~callback_selected:(fun entry str file e ->
        true
      )
      ~callback_changed:(fun str ->
        ()
      )
      idx
  in

  let res =
    G.dialog_ask_generic ~title:"" 
      (fun vbox -> 
        vbox#pack (G.with_label "search:" entry#coerce);
      )
      (fun () -> 
        let text = entry#text in 
        pr2 text;
        text
      )
  in
  res +> Common.do_option (fun s -> 
    pr2 ("selected: " ^ s);
  );
  res
(*e: dialog_search_def *)

(*s: run_grep_query *)
let run_grep_query ~root s =
  (* --cached so faster ? use -w ?  
   * -I means no search for binary files
   * -n to show also line number
   *)
  let git_grep_options = 
    "-I -n"
  in
  let cmd = 
    spf "cd %s; git grep %s %s" root git_grep_options s
  in
  let xs = Common.cmd_to_list cmd in
  let xs = xs +> List.map (fun s ->
    if s =~ "\\([^:]*\\):\\([0-9]+\\):.*"
    then
      let (filename, lineno) = Common.matched2 s in
      let lineno = s_to_i lineno in
      let fullpath = Filename.concat root filename in
      fullpath, lineno
    else 
      failwith ("wrong git grep line: " ^ s)
  ) in
  xs
(*e: run_grep_query *)

(*s: run_tbgs_query *)
let run_tbgs_query ~root s =
  let cmd = 
    spf "cd %s; tbgs --stripdir %s" root s
  in
  let xs = Common.cmd_to_list cmd in
  let xs = xs +> List.map (fun s ->
    if s =~ "\\([^:]*\\):\\([0-9]+\\):.*"
    then
      let (filename, lineno) = Common.matched2 s in
      let lineno = s_to_i lineno in
      let fullpath = Filename.concat root filename in
      fullpath, lineno
    else 
      failwith ("wrong tbgs line: " ^ s)
  ) in
  xs
(*e: run_tbgs_query *)

(* ---------------------------------------------------------------------- *)
(* The main map *)
(* ---------------------------------------------------------------------- *)

(*s: key_pressed *)
let key_pressed (da, da2) dw_ref ev = 
  let dw = !dw_ref in

  pr2 ("key pressed");

  (* this is in device coordinate, so no need to take into account the zoom *)
  let _delta_move = float dw.width /. 16. in
  let delta_move_user = 0.1 in (* TODO *)

  let delta_zoom = 1.3 in

  let b = 
    (match GdkEvent.Key.keyval ev with
    | k when k = K._Left ->
        dw.xtrans <- dw.xtrans +. delta_move_user;
        (* todo opti: *)
        paint dw;
        true
    | k when k = K._Right ->
        dw.xtrans <- dw.xtrans -. delta_move_user;
        (* todo opti: *)
        paint dw;
        true

    | k when k = K._Up ->
        dw.ytrans <- dw.ytrans +. delta_move_user;
        (* todo opti: *)
        paint dw;
        true
    | k when k = K._Down ->
        dw.ytrans <- dw.ytrans -. delta_move_user;
        (* todo opti: *)
        paint dw;
        true

    | k when k = K._plus ->
        dw.zoom <- dw.zoom /. delta_zoom;
        (* can't optimize here, have to paint *)
        paint dw;
        true
    | k when k = K._minus ->
        dw.zoom <- dw.zoom *. delta_zoom;
        (* can't optimize here, have to paint *)
        paint dw;
        true




    | k when k = K._z ->
        dw.in_zoom_incruste <- not (dw.in_zoom_incruste);
        true

    | k when k = K._b ->
        go_back dw_ref;
        true

    | k when k = K._e ->
        raise Todo

    | k when k = K._q ->
        GMain.quit () ; false

    | _ -> false
    )
  in
  if b then begin
    GtkBase.Widget.queue_draw da#as_widget;
    GtkBase.Widget.queue_draw da2#as_widget;
  end;
  b
(*e: key_pressed *)

(*s: find_filepos_in_rectangle_at_user_point *)
(* Cannot move this to model.ml because we abuse the drawing function
 * and the Draw.text_with_user_pos 100.
 *)
let find_filepos_in_rectangle_at_user_point user_pt dw r = 
  (* ugly, but if use dw.pm#pixmap directly then it has
   * weird side effects like darking the label
   *)
  let sur =
    Cairo.surface_create_similar (CairoH.surface_of_pixmap dw.pm)
      Cairo.CONTENT_COLOR_ALPHA dw.width dw.height
  in
  let cr = Cairo.create sur in
           
  zoom_pan_scale_map cr dw;
  let user_rect = device_to_user_area dw in

  let context = context_of_drawing dw in
  let context = { context with Draw.nb_rects_on_screen = 1 } in
  
  (* does side effect on Draw.text_with_user_pos *)
  Draw.draw_treemap_rectangle_content_maybe ~cr ~clipping:user_rect ~context r;
  let xs = !Draw.text_with_user_pos in

  let scores = xs
    +> List.map (fun (s, filepos, pt) ->
      (s, filepos), CairoH.distance_points user_pt pt
    )
    +> Common.sort_by_val_lowfirst 
    +> List.map fst
  in
  (match scores with
  | [] -> 
      pr2 ("no filepos found");
      None
  | (s, filepos)::xs ->
      pr2 (spf "closest point is: %s at %d:%d"
              s filepos.Common.l filepos.Common.c);
      Some filepos
  )
(*e: find_filepos_in_rectangle_at_user_point *)
            

(*s: button_action *)
let button_action da dw_ref ev =
  let dw = !dw_ref in

  let pt = { Cairo. x = GdkEvent.Button.x ev; y = GdkEvent.Button.y ev;} in
  let user = with_map dw (fun cr -> Cairo.device_to_user cr pt) in

  let r_opt = Model2.find_rectangle_at_user_point dw user in

  match GdkEvent.get_type ev with
  | `BUTTON_PRESS ->

      let button = GdkEvent.Button.button ev in
      pr2 (spf "button %d pressed" button);

      (match button with
      | 1 -> 

(* DISABLED FOR NOW
          dw.drag_pt <- { 
            Cairo.x = GdkEvent.Button.x ev; 
            Cairo.y = GdkEvent.Button.y ev; 
          };
          dw.in_dragging <- true;
*)

          r_opt +> Common.do_option (fun (r, _, _r_englobing) ->
            let file = r.T.tr_label in
            pr2 (spf "clicking on %s" file);
          );
          true
      | 2 ->
          r_opt +> Common.do_option (fun (r, _, _r_englobing) ->
            let file = r.T.tr_label in
            pr2 (spf "opening %s" file);

            match find_filepos_in_rectangle_at_user_point user dw r with
            | None ->
                Editor_connection.open_file_in_current_editor ~file ~line:0;
            | Some (fpos) ->
                Editor_connection.open_file_in_current_editor ~file 
                  ~line:fpos.Common.l;
          );
          true

      | 3 ->
          
      r_opt +> Common.do_option (fun (r, _, _r_englobing) ->
        let path = r.T.tr_label in

        go_dirs_or_file dw_ref [path];
      );
      true
      | _ -> false
      )
  | `BUTTON_RELEASE ->

      let button = GdkEvent.Button.button ev in
      pr2 (spf "button %d released" button);

      (match button with
      | 1 ->
          dw.in_dragging <- false;

          GtkBase.Widget.queue_draw da#as_widget;
          true
      | _ -> false
      )

  | `TWO_BUTTON_PRESS ->
      pr2 ("double click");

      r_opt +> Common.do_option (fun (_r, _, r_englobing) ->
        let path = r_englobing.T.tr_label in
        go_dirs_or_file dw_ref [path];
      );

      true
  | _ -> false
(*e: button_action *)

(*s: motion_refresher *)
(* todo: deadclock   M.locked (fun () ->    ) dw.M.model.m *)
let motion_refresher ev dw () =
  let cr_overlay = Cairo.create dw.overlay in
  CairoH.clear cr_overlay;

  let x = GdkEvent.Motion.x ev in
  let y = GdkEvent.Motion.y ev in

  let pt = { Cairo. x = GdkEvent.Motion.x ev; y = GdkEvent.Motion.y ev;} in
  let user = with_map dw (fun cr -> Cairo.device_to_user cr pt) in

  let r_opt = find_rectangle_at_user_point dw user in
  r_opt +> Common.do_option (fun (r, middle, r_englobing) ->
    let txt = r.T.tr_label in
    statusbar_addtext txt;
    
    draw_label_overlay ~cr_overlay ~dw ~x ~y r;
    draw_rectangle_overlay ~cr_overlay ~dw (r, middle, r_englobing);
    
    if dw.settings.draw_searched_rectangles;
    then
        draw_searched_rectangles ~cr_overlay ~dw;
    
    current_r := Some r;
    
    (* it has been computed, use it then *)
    if Hashtbl.mem _hmemo_surface (r.T.tr_label, dw.zoom) &&
      dw.in_zoom_incruste
    then
      draw_zoomed_overlay ~cr_overlay ~user ~dw ~x ~y r;
      
  );
  !_refresh_da ();
  false


let motion_notify (da, da2) dw ev =

  !current_motion_refresher +> Common.do_option (fun x ->
    GMain.Idle.remove x;
  );

  let dw = !dw in

  let x = GdkEvent.Motion.x ev in
  let y = GdkEvent.Motion.y ev in
  pr2 (spf "motion: %f, %f" x y);

  if dw.in_dragging then begin

    let deltax = x -. dw.drag_pt.Cairo.x in
    let deltay = y -. dw.drag_pt.Cairo.y in
    
    let deltax_user = 
      with_map dw (fun cr -> CairoH.device_to_user_distance_x cr deltax)
    in
    let deltay_user = 
      with_map dw (fun cr -> CairoH.device_to_user_distance_y cr deltay)
    in
    
    dw.xtrans <- dw.xtrans +. deltax_user;
    dw.ytrans <- dw.ytrans +. deltay_user;
    
    dw.drag_pt <- { Cairo.x = x ; Cairo.y = y } ;
    
    GtkBase.Widget.queue_draw da#as_widget;
    GtkBase.Widget.queue_draw da2#as_widget;
  
    true
  end else begin
    current_motion_refresher := 
      Some (Gui.gmain_idle_add ~prio:100 (motion_refresher ev dw));
    true
  end
(*e: motion_refresher *)

(*s: idle *)
let idle dw () = 
  let dw = !dw in

  (*pr2 "idle";*)
  !current_r +> Common.do_option (fun r ->
    (* will compute and cache *)
    if dw.in_zoom_incruste 
    then zoomed_surface_of_rectangle dw r +> ignore;
  );
  true
(*e: idle *)

(* ---------------------------------------------------------------------- *)
(* The mini-map *)
(* ---------------------------------------------------------------------- *)

(*s: motion_notify_minimap *)
let motion_notify_minimap (da, da2) dw ev =
  let dw = !dw in

  let x = GdkEvent.Motion.x ev in
  let y = GdkEvent.Motion.y ev in
  pr2 ("motion minimap");

  if dw.in_dragging then begin

    let deltax = x -. dw.drag_pt_minimap.Cairo.x in
    let deltay = y -. dw.drag_pt_minimap.Cairo.y in

    let deltax_user = 
      with_minimap dw (fun cr -> CairoH.device_to_user_distance_x cr deltax)
    in
    let deltay_user = 
      with_minimap dw (fun cr -> CairoH.device_to_user_distance_y cr deltay)
    in
    dw.xtrans <- dw.xtrans -. deltax_user;
    dw.ytrans <- dw.ytrans -. deltay_user;
    
  
    (* pr2_gen (deltax, deltay); *)
    dw.drag_pt_minimap <- { Cairo.x = x ; Cairo.y = y } ;
    
    (* TODO: opti, should not recompute when just move! *)
    paint dw;
    GtkBase.Widget.queue_draw da#as_widget;
    GtkBase.Widget.queue_draw da2#as_widget;
    
    true
  end else begin
    true
  end
(*e: motion_notify_minimap *)

(*s: button_action_minimap *)
let button_action_minimap (da,da2) dw ev =
  let dw = !dw in

  match GdkEvent.get_type ev with
  | `BUTTON_PRESS ->
      pr2 ("button pressed minimap");

      dw.drag_pt_minimap <- { 
        Cairo.x = GdkEvent.Button.x ev ; 
        Cairo.y = GdkEvent.Button.y ev 
      };
      dw.in_dragging <- true;

      true

  | `BUTTON_RELEASE ->
      pr2 ("button released minimap");
      dw.in_dragging <- false;

      (* TODO: opti *)
      paint dw;

      GtkBase.Widget.queue_draw da#as_widget;
      GtkBase.Widget.queue_draw da2#as_widget;
      true
  | _ -> false
(*e: button_action_minimap *)

(*****************************************************************************)
(* The main UI *)
(*****************************************************************************)

(*s: mk_gui() *)
let mk_gui ~screen_size test_mode (root, model, dw, dbfile_opt) =

  let dw = ref dw in
  Common.push2 !dw dw_stack;

  let width, height, minimap_hpos, minimap_vpos = 
    Style.windows_params screen_size in

  let w = GWindow.window 
    ~title:(title_of_path root)
    ~width
    ~height
    ~allow_shrink: true
    ~allow_grow:true
    () 
  in
  _set_title := (fun s -> w#set_title s);

  let accel_group = GtkData.AccelGroup.create () in
  w#misc#set_name "main window";

  let quit () = 
    (*Controller.before_quit_all model;*)
    GMain.Main.quit ();
  in

  w#add_accel_group accel_group;

  (*-------------------------------------------------------------------*)
  (* Layout *)
  (*-------------------------------------------------------------------*)

  (* if use my G.mk style for that, then get some pbs when trying
   * to draw stuff :(
   *)
  let vbox = GPack.vbox ~packing:w#add () in

    (*-------------------------------------------------------------------*)
    (* Menu *)
    (*-------------------------------------------------------------------*)
    vbox#pack (G.mk (GMenu.menu_bar) (fun m -> 
      
      let factory = new GMenu.factory m in

      factory#add_submenu "_File" +> (fun menu -> 
        let fc = new GMenu.factory menu ~accel_group in

        (* todo? open Db ? *)

        fc#add_item "_Open stuff from db" ~key:K._O ~callback:(fun () -> 
          ();
        ) +> ignore;
        fc#add_separator () +> ignore;

        fc#add_item "_Quit" ~key:K._Q ~callback:quit;
      ) +> ignore;

      factory#add_submenu "_Edit" +> (fun menu -> 
        GToolbox.build_menu menu ~entries:[
          `S;
        ];
      ) +> ignore;

      factory#add_submenu "_Move" +> (fun menu -> 
        let fc = new GMenu.factory menu ~accel_group in

        (* todo? open Db ? *)
        fc#add_item "_Go back" ~key:K._B ~callback:(fun () -> 
          go_back dw;
        ) +> ignore;

        fc#add_item "_Go to example" ~key:K._E ~callback:(fun () -> 
          let model = !dw.model in
          let model = Model2.async_get model in
          match !dw.current_entity, model.db with
          | Some e, Some db ->
              (match e.Db.e_good_examples_of_use with
              | [] -> failwith "no good examples of use for this entity"
              | x::xs ->
                  let e = db.Db.entities.(x) in
                  let file = e.Db.e_file in

                  let final_file = 
                    Model2.readable_to_absolute_filename_under_root file 
                      ~root:!dw.root in

                  go_dirs_or_file ~current_entity:(Some e) dw [final_file];
              )
          | _ -> failwith "no entity currently selected or no db"
        ) +> ignore ;
      );

      factory#add_submenu "_Search" +> (fun menu -> 
        let fc = new GMenu.factory menu ~accel_group in

        (* todo? open Db ? *)
        fc#add_item "_Git grep" ~key:K._G ~callback:(fun () -> 

          let res = dialog_search_def !dw.model in
          res +> Common.do_option (fun s ->
            let root = 
              (* could also support local grep? and use !dw.root instead ?  *)
              root_orig ()
            in
            let matching_files = run_grep_query ~root s in
            let files = matching_files +> List.map fst +> Common.uniq in
            let current_grep_query = 
              Some (Common.hash_of_list matching_files)
            in
            go_dirs_or_file ~current_grep_query dw files
          );
        ) +> ignore;

        fc#add_item "_Tbgs query" ~key:K._T ~callback:(fun () -> 

          let res = dialog_search_def !dw.model in
          res +> Common.do_option (fun s ->
            let root = !dw.root in
            let matching_files = run_tbgs_query ~root s in
            let files = matching_files +> List.map fst +> Common.uniq in
            let current_grep_query = 
              Some (Common.hash_of_list matching_files)
            in
            go_dirs_or_file ~current_grep_query dw files
          );
        ) +> ignore;

      );

      factory#add_submenu "_Misc" +> (fun menu -> 
        let fc = new GMenu.factory menu ~accel_group in

        (* todo? open Db ? *)

        fc#add_item "_Refresh" ~key:K._R ~callback:(fun () -> 
          let current_root = !dw.root in
          let _old_dw = Common.pop2 dw_stack in
          (* have to disable the AST caching.
           * todo? disable all entries in the cache ?
           *)
          if Common.is_file current_root
          then Parsing2.disable_file_in_cache current_root;

          go_dirs_or_file dw [current_root];

        ) +> ignore;

        fc#add_item "_Zoom" ~key:K._Z ~callback:(fun () -> 
          !dw.in_zoom_incruste <- not (!dw.in_zoom_incruste);
          !_refresh_da();
        ) +> ignore;

      );

      factory#add_submenu "_Help" +> (fun menu -> 
        let fc = new GMenu.factory menu ~accel_group in

        fc#add_item "_Interface" ~key:K._H ~callback:(fun () -> 
            G.dialog_text interface_doc "Help"
        ) +> ignore;

        fc#add_item "_Legend" ~key:K._L ~callback:(fun () -> 
          raise Todo
        ) +> ignore;

        fc#add_item "_Help on Pfff" ~callback:(fun () -> 
            G.dialog_text "Read\nthe\nsource\n\ndude" "Help"
        ) +> ignore;
        fc#add_separator () +> ignore;
        fc#add_item "About" ~callback:(fun () -> 
            G.dialog_text "Brought to you by pad\nwith love" "About"
        ) +> ignore;
      );


    ));

    (*-------------------------------------------------------------------*)
    (* toolbar *)
    (*-------------------------------------------------------------------*)

    vbox#pack (G.mk (GButton.toolbar) (fun tb ->

(*
      tb#insert_widget (G.mk (GButton.button ~stock:`OPEN) (fun b -> 
        b#connect#clicked ~callback:(fun () -> 
          pr2 "OPEN";
        );
      ));
      tb#insert_widget (G.mk (GButton.button ~stock:`SAVE) (fun b -> 
        b#connect#clicked ~callback:(fun () -> 
          pr2 "SAVE";
        );
      ));
      tb#insert_space ();
      tb#insert_button ~text:"SAVE THIS" ~callback:(fun () -> 
        pr2 "SAVE THIS";
      ) () +> ignore;
      tb#insert_space ();

*)
      let idx = (fun () -> 
        let model = Model2.async_get model in
        model.Model2.big_grep_idx 
      )
      in

      let entry = 
        Completion2.my_entry_completion_eff 
         ~callback_selected:(fun entry str file e ->
          (* pb is that we may have run the visualizer on a subdir
           * of what is mentionned in the database code. We have
           * then to find the real root.
           *)
          entry#set_text "";

          let readable_paths = 
            (* hack to handle multidirs *)
            match e.Db.e_kind with
            | Database_code.MultiDirs ->
                (* hack: coupling: with mk_multi_dirs_entity *)
                Common.split "|" e.Db.e_file
            | _ ->
                [e.Db.e_file]
          in

          let final_paths = 
            readable_paths +> List.map 
              (Model2.readable_to_absolute_filename_under_root ~root:!dw.root)
          in

          pr2 (spf "e= %s, final_paths= %s" str(Common.join "|" final_paths));
          go_dirs_or_file ~current_entity:(Some e) dw final_paths;
          true
        )
        ~callback_changed:(fun str ->
          !dw.current_query <- str;
          !dw.current_searched_rectangles <- [];

          if !dw.settings.draw_searched_rectangles
          then begin
            (* better to compute once the set of matching rectangles
             * cos doing it each time in motify would incur too much
             * calls to ==~
             *)
            let minimum_length = 3 in

            if String.length str > minimum_length then begin

              let rects = !dw.treemap in
              let re_opt = 
                try Some (Str.regexp (".*" ^ str))
               (* can raise exn when have bad or not yet complete regexp *)
                with _ -> None
              in
              let res = 
                match re_opt with
                | None -> []
                | Some re ->
                    rects +> List.filter (fun r -> 
                      let label = r.T.tr_label +> String.lowercase in
                      label ==~ re
                    )
              in
              !dw.current_searched_rectangles <- res;
              
            end;
            let cr_overlay = Cairo.create !dw.overlay in
            CairoH.clear cr_overlay;
            draw_searched_rectangles ~cr_overlay ~dw:!dw;
            !_refresh_da();
          end
        )
        idx
        ;

      in


      tb#insert_widget (G.with_label "Search:" entry#coerce);

      tb#insert_widget (G.mk (GButton.button ~stock:`GO_BACK) (fun b -> 
        b#connect#clicked ~callback:(fun () -> 
          go_back dw;
        )
      ));

      tb#insert_widget (G.mk (GButton.button ~stock:`GO_UP) (fun b -> 
        b#connect#clicked ~callback:(fun () -> 
          let current_root = !dw.root in
          go_dirs_or_file dw [Common.dirname current_root];
        )
      ));

      tb#insert_widget (G.mk (GButton.button ~stock:`GOTO_TOP) (fun b -> 
        b#connect#clicked ~callback:(fun () -> 
          dw_stack := [Common.last !dw_stack];
          go_back dw;

        )
      ));

    ));

    (*-------------------------------------------------------------------*)
    (* main view *)
    (*-------------------------------------------------------------------*)

    let hpane = GPack.paned `HORIZONTAL
      ~packing:(vbox#pack ~expand:true ~fill:true) () in

    let da = GMisc.drawing_area
      ~packing:(hpane#add1) () in
    da#misc#set_double_buffered false;

    let vpane = GPack.paned `VERTICAL
      ~packing:(hpane#add2) () in
    hpane#set_position minimap_hpos;
    

    let da2 = GMisc.drawing_area
      ~packing:(vpane#add1) () in
    da2#misc#set_double_buffered false;

    
    let da3 = GMisc.drawing_area
      ~packing:(vpane#add2) () in
    vpane#set_position minimap_vpos;

    da#misc#set_can_focus true ;
    da#event#add [ `KEY_PRESS;
   `BUTTON_MOTION; `POINTER_MOTION;
   `BUTTON_PRESS; `BUTTON_RELEASE ];

    da2#misc#set_can_focus true ;
    da2#event#add [ `KEY_PRESS;
                 (* weird, but because even if didn't say
                  * POINTER_MOTION here, the minimap still
                  * gets an event for mouse over :(
                  *)
   `BUTTON_MOTION; `POINTER_MOTION;
   `BUTTON_PRESS; `BUTTON_RELEASE ];


    da#event#connect#expose ~callback:(expose da dw) +> ignore;
    da#event#connect#configure ~callback:(configure da dw) +> ignore;

    da3#event#connect#expose ~callback:(expose_legend da3 dw) +> ignore;

(*
    da2#event#connect#expose ~callback:(expose_minimap da2 dw) +> ignore;
    da2#event#connect#configure ~callback:(configure_minimap da2 dw) +> ignore;
*)

    da#event#connect#button_press   (button_action da dw) +> ignore;
    da#event#connect#button_release (button_action da dw) +> ignore;
    da#event#connect#motion_notify  (motion_notify (da,da2) dw) +> ignore; 

(*
    da2#event#connect#button_press  
      (button_action_minimap (da,da2) dw) +> ignore;
    da2#event#connect#button_release 
      (button_action_minimap (da, da2) dw) +> ignore;
    da2#event#connect#motion_notify 
      (motion_notify_minimap (da,da2) dw) +> ignore; 
*)

    _refresh_da := (fun () ->
      GtkBase.Widget.queue_draw da#as_widget;
    );
      

(*
    da#event#connect#key_press ~callback:(key_pressed da dw);
*)

    (*-------------------------------------------------------------------*)
    (* status bar *)
    (*-------------------------------------------------------------------*)
    (* the statusbar widget is defined in beginning of this file because *)
    vbox#pack (*~from: `END*) statusbar#coerce;

  (*  )); *)

  (*-------------------------------------------------------------------*)
  (* End *)
  (*-------------------------------------------------------------------*)

  (* Controller._before_quit_all_func +> Common.push2 Model.close_model; *)

  GtkSignal.user_handler := (fun exn -> 
    pr2 "fucking callback";
    (* old: before 3.11: Features.Backtrace.print(); *)
    let s = Printexc.get_backtrace () in
    pr2 s;
    let pb = "pb: " ^ string_of_exn exn in
    G.dialog_text ~text:pb ~title:"pb";
    raise exn
  );

  (* TODO: should do that on 'da', not 'w' 
  w#event#connect#key_press ~callback:(key_pressed (da,da2) dw) +> ignore;
  *)

(*
  w#event#connect#key_press ~callback:(fun ev -> 
    let k = GdkEvent.Key.keyval ev in
    (match k with
    | _ when k = Char.code 'q' -> 
        quit();
        true   
    | _ -> false
    )
  );
*)

  w#event#connect#delete    ~callback:(fun _  -> quit(); true) +> ignore;
  w#connect#destroy         ~callback:(fun () -> quit(); ) +> ignore;
  w#show ();

  (* test *)
  test_mode +> Common.do_option (fun s -> 
    (* View_test.do_command s model *)
    ()
  );

  (* Gui.gmain_idle_add ~prio: 1000 (idle dw) +> ignore; *)

  GtkThread.main ();
  ()
(*e: mk_gui() *)
  
(*e: view2.ml *)
