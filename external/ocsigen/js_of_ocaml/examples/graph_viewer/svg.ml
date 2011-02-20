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

type command =
    Move_to of float * float
  | Curve_to of float * float * float * float * float * float

type color = float * float * float
type element =
    Path of command list * color option * color option
  | Ellipse of float * float * float * float * color option * color option
  | Polygon of (float * float) list * color option * color option
  | Text of
      float * float * string * string * float * color option * color option

(****)

let width = 16499.
let height = 22807.

let h = (*8192*)2000
let w = truncate (width *. float h /. height +. 0.5)
let s = Cairo.image_surface_create Cairo.FORMAT_ARGB32 w h

let perform_draw ctx fill stroke =
(*
  print_extent ctx fill stroke;
*)
  begin match fill with
    Some (r, g, b) ->
      Cairo.set_source_rgb ctx r g b;
      if stroke <> None then Cairo.fill_preserve ctx
      else Cairo.fill ctx
  | None ->
      ()
  end;
  begin match stroke with
    Some (r, g, b) ->
      Cairo.set_source_rgb ctx r g b;
      Cairo.stroke ctx
  | None ->
      ()
  end

let pi = 4. *. atan 1.

let draw_element ctx e =
  match e with
    Path (cmd, fill, stroke) ->
      List.iter
        (fun c ->
           match c with
             Move_to (x, y) ->
               Cairo.move_to ctx x y
           | Curve_to (x1, y1, x2, y2, x3, y3) ->
               Cairo.curve_to ctx x1 y1 x2 y2 x3 y3)
        cmd;
      perform_draw ctx fill stroke
  | Ellipse (cx, cy, rx, ry, fill, stroke) ->
      Cairo.save ctx;
      Cairo.translate ctx cx cy;
      Cairo.scale ctx rx ry;
      Cairo.arc ctx 0. 0. 1. 0. (2. *. pi);
      Cairo.restore ctx;
      perform_draw ctx fill stroke
  | Polygon (points, fill, stroke) ->
      begin match points with
        (x, y) :: rem ->
          Cairo.move_to ctx x y;
          List.iter (fun (x, y) -> Cairo.line_to ctx x y) rem;
          Cairo.close_path ctx;
          perform_draw ctx fill stroke
      | [] ->
          ()
      end
  | Text (x, y, txt, font, font_size, fill, stroke) ->
      let ext = Cairo.text_extents ctx txt in
      Cairo.move_to ctx
        (x -. ext.Cairo.x_bearing -. ext.Cairo.text_width /. 2.) y;
      Cairo.select_font_face ctx font
        Cairo.FONT_SLANT_NORMAL Cairo.FONT_WEIGHT_NORMAL;
      Cairo.set_font_size ctx font_size;
      Cairo.show_text ctx txt;
      perform_draw ctx fill stroke

let path_extent ctx fill stroke =
  if stroke <> None then Cairo.stroke_extents ctx
  else Cairo.fill_extents ctx

let compute_extent ctx e =
  Cairo.new_path ctx;
  match e with
    Path (cmd, fill, stroke) ->
      List.iter
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
      begin match points with
        (x, y) :: rem ->
          Cairo.move_to ctx x y;
          List.iter (fun (x, y) -> Cairo.line_to ctx x y) rem;
          Cairo.close_path ctx;
          path_extent ctx fill stroke
      | [] ->
          assert false
      end
  | Text (x, y, txt, font, font_size, fill, stroke) ->
      let ext = Cairo.text_extents ctx txt in
      (x -. ext.Cairo.text_width /. 2.,
       y +. ext.Cairo.y_bearing,
       x +. ext.Cairo.text_width /. 2.,
       y +. ext.Cairo.y_bearing +. ext.Cairo.text_height)

let ctx = Cairo.create s

let scale = float h /. height
let _ = Cairo.scale ctx scale scale; Cairo.translate ctx 364. 22443.

(****)

let convert (r, g, b) =
  let c i = float i /. 255.99 in
  (c r, c g, c b)

let named_colors =
  let colors = Hashtbl.create 101 in
  List.iter (fun (nm, v) -> Hashtbl.add colors nm (convert v))
    ["aliceblue", (240, 248, 255);
     "antiquewhite", (250, 235, 215);
     "aqua", ( 0, 255, 255);
     "aquamarine", (127, 255, 212);
     "azure", (240, 255, 255);
     "beige", (245, 245, 220);
     "bisque", (255, 228, 196);
     "black", ( 0, 0, 0);
     "blanchedalmond", (255, 235, 205);
     "blue", ( 0, 0, 255);
     "blueviolet", (138, 43, 226);
     "brown", (165, 42, 42);
     "burlywood", (222, 184, 135);
     "cadetblue", ( 95, 158, 160);
     "chartreuse", (127, 255, 0);
     "chocolate", (210, 105, 30);
     "coral", (255, 127, 80);
     "cornflowerblue", (100, 149, 237);
     "cornsilk", (255, 248, 220);
     "crimson", (220, 20, 60);
     "cyan", ( 0, 255, 255);
     "darkblue", ( 0, 0, 139);
     "darkcyan", ( 0, 139, 139);
     "darkgoldenrod", (184, 134, 11);
     "darkgray", (169, 169, 169);
     "darkgreen", ( 0, 100, 0);
     "darkgrey", (169, 169, 169);
     "darkkhaki", (189, 183, 107);
     "darkmagenta", (139, 0, 139);
     "darkolivegreen", ( 85, 107, 47);
     "darkorange", (255, 140, 0);
     "darkorchid", (153, 50, 204);
     "darkred", (139, 0, 0);
     "darksalmon", (233, 150, 122);
     "darkseagreen", (143, 188, 143);
     "darkslateblue", ( 72, 61, 139);
     "darkslategray", ( 47, 79, 79);
     "darkslategrey", ( 47, 79, 79);
     "darkturquoise", ( 0, 206, 209);
     "darkviolet", (148, 0, 211);
     "deeppink", (255, 20, 147);
     "deepskyblue", ( 0, 191, 255);
     "dimgray", (105, 105, 105);
     "dimgrey", (105, 105, 105);
     "dodgerblue", ( 30, 144, 255);
     "firebrick", (178, 34, 34);
     "floralwhite", (255, 250, 240);
     "forestgreen", ( 34, 139, 34);
     "fuchsia", (255, 0, 255);
     "gainsboro", (220, 220, 220);
     "ghostwhite", (248, 248, 255);
     "gold", (255, 215, 0);
     "goldenrod", (218, 165, 32);
     "gray", (128, 128, 128);
     "grey", (128, 128, 128);
     "green", ( 0, 128, 0);
     "greenyellow", (173, 255, 47);
     "honeydew", (240, 255, 240);
     "hotpink", (255, 105, 180);
     "indianred", (205, 92, 92);
     "indigo", ( 75, 0, 130);
     "ivory", (255, 255, 240);
     "khaki", (240, 230, 140);
     "lavender", (230, 230, 250);
     "lavenderblush", (255, 240, 245);
     "lawngreen", (124, 252, 0);
     "lemonchiffon", (255, 250, 205);
     "lightblue", (173, 216, 230);
     "lightcoral", (240, 128, 128);
     "lightcyan", (224, 255, 255);
     "lightgoldenrodyellow", (250, 250, 210);
     "lightgray", (211, 211, 211);
     "lightgreen", (144, 238, 144);
     "lightgrey", (211, 211, 211);
     "lightpink", (255, 182, 193);
     "lightsalmon", (255, 160, 122);
     "lightseagreen", ( 32, 178, 170);
     "lightskyblue", (135, 206, 250);
     "lightslategray", (119, 136, 153);
     "lightslategrey", (119, 136, 153);
     "lightsteelblue", (176, 196, 222);
     "lightyellow", (255, 255, 224);
     "lime", ( 0, 255, 0);
     "limegreen", ( 50, 205, 50);
     "linen", (250, 240, 230);
     "magenta", (255, 0, 255);
     "maroon", (128, 0, 0);
     "mediumaquamarine", (102, 205, 170);
     "mediumblue", ( 0, 0, 205);
     "mediumorchid", (186, 85, 211);
     "mediumpurple", (147, 112, 219);
     "mediumseagreen", ( 60, 179, 113);
     "mediumslateblue", (123, 104, 238);
     "mediumspringgreen", ( 0, 250, 154);
     "mediumturquoise", ( 72, 209, 204);
     "mediumvioletred", (199, 21, 133);
     "midnightblue", ( 25, 25, 112);
     "mintcream", (245, 255, 250);
     "mistyrose", (255, 228, 225);
     "moccasin", (255, 228, 181);
     "navajowhite", (255, 222, 173);
     "navy", ( 0, 0, 128);
     "oldlace", (253, 245, 230);
     "olive", (128, 128, 0);
     "olivedrab", (107, 142, 35);
     "orange", (255, 165, 0);
     "orangered", (255, 69, 0);
     "orchid", (218, 112, 214);
     "palegoldenrod", (238, 232, 170);
     "palegreen", (152, 251, 152);
     "paleturquoise", (175, 238, 238);
     "palevioletred", (219, 112, 147);
     "papayawhip", (255, 239, 213);
     "peachpuff", (255, 218, 185);
     "peru", (205, 133, 63);
     "pink", (255, 192, 203);
     "plum", (221, 160, 221);
     "powderblue", (176, 224, 230);
     "purple", (128, 0, 128);
     "red", (255, 0, 0);
     "rosybrown", (188, 143, 143);
     "royalblue", ( 65, 105, 225);
     "saddlebrown", (139, 69, 19);
     "salmon", (250, 128, 114);
     "sandybrown", (244, 164, 96);
     "seagreen", ( 46, 139, 87);
     "seashell", (255, 245, 238);
     "sienna", (160, 82, 45);
     "silver", (192, 192, 192);
     "skyblue", (135, 206, 235);
     "slateblue", (106, 90, 205);
     "slategray", (112, 128, 144);
     "slategrey", (112, 128, 144);
     "snow", (255, 250, 250);
     "springgreen", ( 0, 255, 127);
     "steelblue", ( 70, 130, 180);
     "tan", (210, 180, 140);
     "teal", ( 0, 128, 128);
     "thistle", (216, 191, 216);
     "tomato", (255, 99, 71);
     "turquoise", ( 64, 224, 208);
     "violet", (238, 130, 238);
     "wheat", (245, 222, 179);
     "white", (255, 255, 255);
     "whitesmoke", (245, 245, 245);
     "yellow", (255, 255, 0);
     "yellowgreen", (154, 205, 50)];
  colors

let svg_name nm = ("http://www.w3.org/2000/svg", nm)
let d_attr = ("", "d")
let x_attr = ("", "x")
let y_attr = ("", "y")
let cx_attr = ("", "cx")
let cy_attr = ("", "cy")
let rx_attr = ("", "rx")
let ry_attr = ("", "ry")
let points_attr = ("", "points")
let taxt_anchor_attr = ("", "text-anchor")
let font_family_attr = ("", "font-family")
let font_size_attr = ("", "font-size")
let fill_attr = ("", "fill")
let stroke_attr = ("", "stroke")

let stack = ref []
let push e = stack := e :: !stack

let skip_whitespace i =
  (* XXX Check white-space only *)
  match Xmlm.peek i with
    `Data s -> ignore (Xmlm.input i)
  | _       -> ()

let end_tag i =
  let e = Xmlm.input i in
  assert (e = `El_end)

let rec empty_tag i =
  match Xmlm.input i with
    `Data s -> (*Whitespace*) empty_tag i
  | `El_end -> ()
  | _       -> assert false

let rec text_tag i =
  match Xmlm.input i with
    `Data s -> empty_tag i; s
  | `El_end -> ""
  | _       -> assert false

let comma_wsp = Str.regexp "[\x20\x09\x0D\x0A,]+"
let cmd = Str.regexp "[a-zA-Z]"

let rec parse_curve_to args rem =
  match args with
    [] ->
      rem
  | x1 :: y1 :: x2 :: y2 :: x3 :: z3 :: r ->
      Curve_to (x1, y1, x2, y2, x3, z3) :: parse_curve_to r rem
  | _ ->
      assert false

let rec parse_cmds l =
  match l with
    Str.Delim cmd :: Str.Text args :: rem ->
      let args = List.map float_of_string (Str.split comma_wsp args) in
      let rem = parse_cmds rem in
      begin match cmd, args with
        "M", [x; y] ->
          Move_to (x, y) :: rem
      | "C", (_ :: _ as args) ->
          parse_curve_to args rem
      | _ ->
          assert false
      end
  | [] ->
      []
  | _ ->
      assert false

let parse_path s =
  let l = Str.full_split cmd s in
  parse_cmds l

let parse_color c =
  if c = "none" then None else
  if String.length c = 7 && c.[0] = '#' then begin
    let conv s = int_of_string ("0x" ^ s) in
    let c =
      (conv (String.sub c 1 2),
       conv (String.sub c 3 2),
       conv (String.sub c 5 2))
    in
    Some (convert c)
  end else
    Some (try Hashtbl.find named_colors c
          with Not_found -> Format.eprintf "%s@." c; assert false)

let read_path attrs i =
  let d = List.assoc d_attr attrs in
(*Format.eprintf "d=%s@." d;*)
  let cmd = parse_path d in
  let fill = parse_color (List.assoc fill_attr attrs) in
  let stroke = parse_color (List.assoc stroke_attr attrs) in
  let e = Path (cmd, fill, stroke) in
  push e;
  empty_tag i

let read_ellipse attrs i =
  let cx = float_of_string (List.assoc cx_attr attrs) in
  let cy = float_of_string (List.assoc cy_attr attrs) in
  let rx = float_of_string (List.assoc rx_attr attrs) in
  let ry = float_of_string (List.assoc ry_attr attrs) in
  let fill = parse_color (List.assoc fill_attr attrs) in
  let stroke = parse_color (List.assoc stroke_attr attrs) in
  let e = Ellipse (cx, cy, rx, ry, fill, stroke) in
  push e;
  empty_tag i

let rec group l =
  match l with
    x :: y :: r -> (x, y) :: group r
  | []          -> []
  | _           -> assert false

let read_polygon attrs i =
  let points = List.assoc points_attr attrs in
  let points = group (List.map float_of_string (Str.split comma_wsp points)) in
  let fill = parse_color (List.assoc fill_attr attrs) in
  let stroke = parse_color (List.assoc stroke_attr attrs) in
  let e = Polygon (points, fill, stroke) in
  push e;
  empty_tag i

let read_text attrs i =
  let fill = parse_color (try List.assoc fill_attr attrs with Not_found -> "black") in
  let stroke = parse_color (try List.assoc stroke_attr attrs with Not_found -> "none") in
  let x = float_of_string (List.assoc x_attr attrs) in
  let y = float_of_string (List.assoc y_attr attrs) in
  let font = List.assoc font_family_attr attrs in
  let font_size = float_of_string (List.assoc font_size_attr attrs) in
  let txt = text_tag i in
  let e = Text (x, y, txt, font, font_size, fill, stroke) in
  push e

let rec read_element nm attrs i =
  skip_whitespace i;
  match Xmlm.input i with
    `El_end ->
      ()
  | `Data d ->
      begin match Xmlm.input i with
        `El_end ->
          ()
      | _ ->
        assert false
      end
  | `El_start ((_, nm'), attrs') ->
(*
      Format.eprintf "%s" nm';
List.iter (fun ((_, nm), _) -> Format.eprintf " %s" nm) attrs';
Format.eprintf "@.";
*)
      begin match nm' with
        "path" ->
          ignore (read_path attrs' i)
      | "ellipse" ->
          ignore (read_ellipse attrs' i)
      | "polygon" ->
          ignore (read_polygon attrs' i)
      | "text" ->
          ignore (read_text attrs' i)
      | _ ->
          read_element nm' attrs' i
      end;
      read_element nm attrs i
  | _ ->
    assert false

let _ =
  let  ch = open_in "/tmp/foo.svg" in
  let i = Xmlm.make_input (`Channel ch) in
  begin match Xmlm.input i with
    `Dtd (Some nm) -> ()
  | _ ->
      assert false
  end;
  begin match Xmlm.input i with
    `El_start ((_, nm), attrs) -> assert (nm = "svg"); read_element nm attrs i
  | _                          -> assert false
  end

let l = List.rev !stack

let bboxes = ref []

let intersects (x1, y1, x2, y2) (x3, y3, x4, y4) =
  x1 <= x4 && y1 <= y4 && x3 <= x2 && y3 <= y4

let redraw w range ev =
(*
  let t1 = Unix.gettimeofday () in
*)
  let ctx = Cairo_lablgtk.create w#misc#window in
  Cairo.save ctx;
  if !bboxes = [] then bboxes := List.map (fun e -> compute_extent ctx e) l;
  Cairo.new_path ctx;
  Cairo_lablgtk.region ctx (GdkEvent.Expose.region ev);
  let rect = Gdk.Rectangle.create 0 0 0 0 in
  Gdk.Region.get_clipbox (GdkEvent.Expose.region ev) rect;
  Cairo.clip ctx;
  let scale = scale *. (1. /. scale) ** range#adjustment#value in
  Cairo.scale ctx scale scale; Cairo.translate ctx 364. 22443.;
  let bbox =
    let x = float (Gdk.Rectangle.x rect) /. scale -. 364. in
    let y = float (Gdk.Rectangle.y rect) /. scale -. 22443. in
    (x, y,
     x +. float (Gdk.Rectangle.width rect) /. scale,
     y +. float (Gdk.Rectangle.height rect) /. scale)
  in
(*
let (x1, y1, x2, y2) = bbox in
Format.eprintf "%f %f %f %f (%f)@." x1 y1 x2 y2 scale;
*)
  List.iter2
    (fun box e -> if intersects box bbox then draw_element ctx e) !bboxes l;
  Cairo.restore ctx;
(*
  let t2 = Unix.gettimeofday () in
  Format.eprintf "%f@." (t2 -. t1);
*)
  true

let slider_changed (area : GMisc.drawing_area) range () =
  let scale = scale *. (1. /. scale) ** range#adjustment#value in
  area#misc#set_size_request
    ~width:(truncate (width *. scale))
    ~height:(truncate (height *. scale))
    ();
  GtkBase.Widget.queue_draw area#as_widget

let _ =
  ignore (GMain.Main.init ());
  let initial_size = 600 in
  let w = GWindow.window () in
  ignore (w#connect#destroy GMain.quit);

  let b = GPack.vbox ~spacing:6 ~border_width:12
      ~packing:w#add () in

(*
  let f = GBin.frame ~shadow_type:`IN
      ~packing:(b#pack ~expand:true ~fill:true) () in
*)
  let f =
    GBin.scrolled_window ~packing:(b#pack ~expand:true)
(*      ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC *) ()
  in

  let area = GMisc.drawing_area
      ~width:initial_size ~height:initial_size
      ~packing:f#add_with_viewport () in

  area#misc#set_size_request
    ~width:(truncate (width *. scale))
    ~height:(truncate (height *. scale))
    ();
  let slider = GRange.scale `HORIZONTAL
      ~draw_value:false ~packing:b#pack () in
  slider#adjustment#set_bounds
    ~lower:0. ~upper:1.
    ~step_incr:0.1 () ;
(*
  let button = GButton.check_button ~label:"Animate"
      ~packing:b#pack () in

  ignore (area#event#connect#expose
            (redraw area slider)) ;
  ignore (slider#connect#value_changed 
            (slider_changed area)) ;
  ignore (button#connect#toggled
            (animate_toggled button slider)) ;
*)
  ignore (area#event#connect#expose
            (redraw area slider));
  ignore (slider#connect#value_changed
            (slider_changed area slider)) ;
  w#show () ;
  GMain.main ()


(*
let _ =
  let l = List.rev !stack in
  Format.eprintf "len: %d@." (List.length l);
  let t1 = Unix.gettimeofday () in
  List.iter (fun e -> draw_element ctx e) l;
  let t2 = Unix.gettimeofday () in
  Format.eprintf "%f@." (t2 -. t1);

(*
  let ch = open_out "/tmp/foo.mar" in
  Marshal.to_channel ch l [];
  close_out ch;
*)
  Cairo_png.surface_write_to_file s "/tmp/foo.png"
*)
