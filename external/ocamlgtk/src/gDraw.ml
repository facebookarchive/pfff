(**************************************************************************)
(*                Lablgtk                                                 *)
(*                                                                        *)
(*    This program is free software; you can redistribute it              *)
(*    and/or modify it under the terms of the GNU Library General         *)
(*    Public License as published by the Free Software Foundation         *)
(*    version 2, with the exception described in file COPYING which       *)
(*    comes with the library.                                             *)
(*                                                                        *)
(*    This program is distributed in the hope that it will be useful,     *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of      *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *)
(*    GNU Library General Public License for more details.                *)
(*                                                                        *)
(*    You should have received a copy of the GNU Library General          *)
(*    Public License along with this program; if not, write to the        *)
(*    Free Software Foundation, Inc., 59 Temple Place, Suite 330,         *)
(*    Boston, MA 02111-1307  USA                                          *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: gDraw.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

open Gaux
open Gobject
open Gdk

type color = [
  | `COLOR of Gdk.color
  | `WHITE
  | `BLACK
  | `NAME of string
  | `RGB of int * int * int
]

let default_colormap = GtkBase.Widget.get_default_colormap

let color ?(colormap = default_colormap ()) (c : color) =
  match c with
  | `COLOR col -> col
  | #Gdk.Color.spec as def -> Color.alloc ~colormap def

let conv_color : color data_conv =
  { kind = `POINTER;
    proj = (function `POINTER (Some c) -> `COLOR (Obj.magic c)
           | _ -> failwith "GDraw.get_color");
    inj = (fun c -> `POINTER (Some (Obj.magic (color c : Gdk.color)))) }

type optcolor = [
  | `COLOR of Gdk.color
  | `WHITE
  | `BLACK
  | `NAME of string
  | `RGB of int * int * int
  | `DEFAULT
]

let optcolor ?colormap (c : optcolor) =
  match c with
  | `DEFAULT -> None
  | #color as c -> Some (color ?colormap c)

let conv_optcolor : optcolor data_conv =
  { kind = `POINTER;
    proj = (function `POINTER (Some c) -> `COLOR (Obj.magic c)
           | `POINTER None -> `DEFAULT
           | _ -> failwith "GDraw.get_color");
    inj = (fun c -> `POINTER (Obj.magic (optcolor c : Gdk.color option))) }

class drawable ?(colormap = default_colormap ()) w = object (self)
  val colormap = colormap
  val mutable gc = GC.create w
  val w = w
  method colormap = colormap
  method gc = gc
  method set_gc x = gc <- x
  method color = color ~colormap
  method set_foreground col = GC.set_foreground gc (self#color col)
  method set_background col = GC.set_background gc (self#color col)
  method size = Drawable.get_size w
  method depth = Drawable.get_depth w
  method gc_values = GC.get_values gc
  method set_clip_region = GC.set_clip_region gc
  method set_clip_origin = GC.set_clip_origin gc
  method set_clip_mask = GC.set_clip_mask gc
  method set_clip_rectangle = GC.set_clip_rectangle gc
  method set_line_attributes ?width ?style ?cap ?join () =
    let v = GC.get_values gc in
    GC.set_line_attributes gc
      ~width:(default v.GC.line_width ~opt:width)
      ~style:(default v.GC.line_style ~opt:style)
      ~cap:(default v.GC.cap_style ~opt:cap)
      ~join:(default v.GC.join_style ~opt:join)
  method point = Draw.point w gc
  method line = Draw.line w gc
  method rectangle = Draw.rectangle w gc
  method arc = Draw.arc w gc
  method polygon = Draw.polygon w gc
  method string s = Draw.string w gc s
  method put_layout ~x ~y ?fore ?back lay =
    Draw.layout w gc ~x ~y lay
      ?fore:(may_map self#color fore) ?back:(may_map self#color back)
  method put_image ~x ~y = Draw.image w gc ~xdest:x ~ydest:y
  method put_pixmap ~x ~y = Draw.pixmap w gc ~xdest:x ~ydest:y
  method put_rgb_data = Rgb.draw_image w gc
  method put_pixbuf ~x ~y = GdkPixbuf.draw_pixbuf w gc ~dest_x:x ~dest_y:y
  method points = Draw.points w gc
  method lines = Draw.lines w gc
  method segments = Draw.segments w gc
end

class pixmap ?colormap ?mask pm = object
  inherit drawable ?colormap pm as pixmap
  val bitmap = may_map mask ~f:
      begin fun x ->
        let mask = new drawable x in
        mask#set_foreground `WHITE;
        mask
      end
  val mask : Gdk.bitmap option = mask
  method pixmap : Gdk.pixmap = w
  method mask = mask
  method set_line_attributes ?width ?style ?cap ?join () =
    pixmap#set_line_attributes ?width ?style ?cap ?join ();
    may bitmap ~f:(fun m -> m#set_line_attributes ?width ?style ?cap ?join ())
  method point ~x ~y =
    pixmap#point ~x ~y;
    may bitmap ~f:(fun m -> m#point ~x ~y)
  method line ~x ~y ~x:x' ~y:y' =
    pixmap#line ~x ~y ~x:x' ~y:y';
    may bitmap ~f:(fun m -> m#line ~x ~y ~x:x' ~y:y')
  method rectangle ~x ~y ~width ~height ?filled () =
    pixmap#rectangle ~x ~y ~width ~height ?filled ();
    may bitmap ~f:(fun m -> m#rectangle ~x ~y ~width ~height ?filled ())
  method arc ~x ~y ~width ~height ?filled ?start ?angle () =
    pixmap#arc ~x ~y ~width ~height ?filled ?start ?angle ();
    may bitmap
      ~f:(fun m -> m#arc ~x ~y ~width ~height ?filled ?start ?angle ());
  method polygon ?filled l =
    pixmap#polygon ?filled l;
    may bitmap ~f:(fun m -> m#polygon ?filled l)
  method string s ~font ~x ~y =
    pixmap#string s ~font ~x ~y;
    may bitmap ~f:(fun m -> m#string s ~font ~x ~y)
  method points pts = pixmap#points pts; may bitmap ~f:(fun m -> m#points pts)
  method lines pts = pixmap#lines pts; may bitmap ~f:(fun m -> m#lines pts)
  method segments lns =
    pixmap#segments lns; may bitmap ~f:(fun m -> m#segments lns)
  method put_layout ~x ~y ?fore ?back lay =
    pixmap#put_layout ~x ~y ?fore ?back lay;
    may bitmap ~f:(fun (m : #drawable) -> m#put_layout ~x ~y lay)
end

class type misc_ops = object
  method colormap : colormap
  method realize : unit -> unit
  method visual_depth : int
  method window : window
end

let pixmap ~width ~height ?(mask=false)
    ?(window : < misc : #misc_ops; .. > option) ?colormap () =
  let window, depth, colormap =
    match window with
      Some w ->
        begin try
          w#misc#realize ();
          Some w#misc#window, w#misc#visual_depth,
          match colormap with Some c -> c | None -> w#misc#colormap
        with Gpointer.Null -> failwith "GDraw.pixmap : window"
        end
    | None ->
        let colormap =
          match colormap with Some c -> c | None -> default_colormap () in
        None, (Gdk.Visual.depth (Gdk.Color.get_visual colormap)), colormap
  in
  let mask =
    if not mask then None else
    let bm = Bitmap.create ?window ~width ~height () in
    let mask = new drawable bm in
    mask#set_foreground `BLACK;
    mask#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
    Some bm
  in
  new pixmap (Pixmap.create ?window ~width ~height ~depth ()) ~colormap ?mask

let pixmap_from_xpm ~file ?window ?colormap ?transparent () =
  let window =
    try may_map window ~f:(fun w -> w#misc#realize (); w#misc#window)
    with Gpointer.Null -> invalid_arg "GDraw.pixmap_from_xpm : window"
  in
  let colormap =
    if colormap <> None || window <> None then colormap
    else Some (default_colormap ())
  in
  let pm, mask =
    try Pixmap.create_from_xpm  ~file ?window ?colormap
	?transparent:(may_map transparent ~f:(fun c -> color c)) ()
    with _ -> invalid_arg ("GDraw.pixmap_from_xpm : " ^ file) in
  new pixmap pm ?colormap ~mask

let pixmap_from_xpm_d ~data ?window ?colormap ?transparent () =
  let window =
    try may_map window ~f:(fun w -> w#misc#realize (); w#misc#window)
    with Gpointer.Null -> failwith "GDraw.pixmap_from_xpm_d : no window" in
  let pm, mask =
    Pixmap.create_from_xpm_d ~data ?colormap ?window
      ?transparent:(may_map transparent ~f:(fun c -> color c)) () in
  new pixmap pm ?colormap ~mask

class drag_context context = object
  val context = context
  method status ?(time=Int32.zero) act = DnD.drag_status context act ~time
  method suggested_action = DnD.drag_context_suggested_action context
  method targets = List.map Gdk.Atom.name (DnD.drag_context_targets context)
end
