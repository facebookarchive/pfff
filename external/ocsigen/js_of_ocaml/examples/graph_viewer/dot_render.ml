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

module G = Dot_graph
module IntMap = G.IntMap
module StringMap = G.StringMap

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

let rgb_of_hsv h s v =
  if s <= 0.0 then
    (v, v, v)
  else begin
    let h = 6. *. if h >= 1. then 0. else h in
    let i = truncate h in
    let f = h -. float i in
    let p = v *. (1. -. s) in
    let q = v *. (1. -. s *. f) in
    let t = v *. (1. -. s *. (1. -. f)) in
    match i with
      0 -> (v, t, p)
    | 1 -> (q, v, p)
    | 2 -> (p, v, t)
    | 3 -> (p, q, v)
    | 4 -> (t, p, v)
    | 5 -> (v, p, q)
    | _ -> assert false
  end

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
    try
      Scanf.sscanf c "%f,%f,%f" (fun h s v -> Some (rgb_of_hsv h s v))
    with Scanf.Scan_failure _ | Failure _ | End_of_file | Invalid_argument _ ->
      Some (try Hashtbl.find named_colors c
            with Not_found -> Format.eprintf "%s@." c; assert false)

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

(****)

let comma_re = Str.regexp ","
let semi_re = Str.regexp ";"
let wsp_re = Str.regexp "[\x20\x09\x0D\x0A]+"

let parse_float s = try float_of_string s with Failure _ -> raise Not_found

let parse_rectangle s =
  match Str.split comma_re s with
    [x1; y1; x2; y2] ->
      (parse_float x1, -. parse_float y2,
       parse_float x2, -. parse_float y1)
  | _ ->
      raise Not_found

let parse_point s =
  match Str.split comma_re s with
    [x; y] -> (parse_float x, -. parse_float y)
  | _      -> raise Not_found

let start_point l = match l with x :: _ -> x | _ -> raise Not_found
let rec end_point l =
  match l with [x] -> x | _ :: r -> end_point r | _ -> raise Not_found

let epsilon = 0.0001
let add_arrow scene (px, py) (ux, uy) color arrow_size (*XXX pen_width*) =
  let dx = ux -. px in
  let dy = uy -. py in
  let s = 10. /. (sqrt (dx *. dx +. dy *. dy) +. epsilon) in
  let dx = s *. if dx >= 0. then dx +. epsilon else dx -. epsilon in
  let dy = s *. if dy >= 0. then dy +. epsilon else dy -. epsilon in

  let arrow_width = 0.35 in
  let vx = -. dy *. arrow_width in
  let vy = dx *. arrow_width in
  let qx = px +. dx in
  let qy = py +. dy in
  let l = [|(px, py); (px -. vx, py -. vy); (qx, qy); (px +. vx, py +. vy)|] in
  Scene.add scene (Scene.Polygon (l, color, color))

let rec render_spline_rec l =
  match l with
    [] ->
      []
  | (x1, y1) :: (x2, y2) :: (x3, y3) :: r ->
      Scene.Curve_to (x1, y1, x2, y2, x3, y3) :: render_spline_rec r
  | _ ->
      raise Not_found

let render_spline l =
  match l with
    (x, y) :: r -> Array.of_list (Scene.Move_to (x, y) :: render_spline_rec r)
  | _           -> raise Not_found

let parse_spline scene s color arrow_size =
  let l = Str.split semi_re s in
  List.iter
    (fun s ->
       let l = List.map (fun s -> Str.split comma_re s) (Str.split wsp_re s) in
       let (endp, l) =
         match l with
           ["e"; x; y] :: r -> Some (parse_float x, -. parse_float y), r
         | _                -> None, l
       in
       let (startp, l) =
         match l with
           ["s"; x; y] :: r -> Some (parse_float x, -. parse_float y), r
         | _                -> None, l
       in
       let l =
         List.map
           (fun l ->
              match l with
                [x; y] -> (parse_float x, -. parse_float y)
              | _      -> raise Not_found)
           l
       in
       begin match endp with
         Some u -> add_arrow scene (end_point l) u color arrow_size
       | None   -> ()
       end;
       begin match startp with
         Some u -> add_arrow scene (start_point l) u color arrow_size
       | None   -> ()
       end;
       Scene.add scene (Scene.Path (render_spline l, None, color)))
    l

let add_rect_margin (x1, y1, x2, y2) w =
  (x1 -. w, y1 -. w, x2 +. w, y2 +. w)

let dpi = 72.

let f g =
  let bbox = parse_rectangle (StringMap.find "bb" g.G.graph_attr) in
  let margin =
    try
      parse_float (StringMap.find "margin" g.G.graph_attr)
    with Not_found ->
      4.
  in
  let bbox = add_rect_margin bbox margin in

  let scene = Scene.make () in

  IntMap.iter
    (fun _ n ->
       let (x, y) = parse_point (StringMap.find "pos" n.G.node_attr) in
       let width =
         dpi *. parse_float (StringMap.find "width" n.G.node_attr) in
       let height =
         dpi *. parse_float (StringMap.find "height" n.G.node_attr) in
       let color =
         parse_color
          (try StringMap.find "color" n.G.node_attr with Not_found -> "black")
       in
       let shape =
         try StringMap.find "shape" n.G.node_attr with Not_found -> "ellipse" in
(*XXX parse style *)
       let style =
         try StringMap.find "style" n.G.node_attr with Not_found -> "" in
       let fillcolor =
         if style <> "filled" then None else
         try
           parse_color (StringMap.find "fillcolor" n.G.node_attr)
         with Not_found ->
           color
       in
       begin match shape with
         "box" | "rect" | "rectangle" ->
           let w2 = width /. 2. in
           let h2 = height /. 2. in
           Scene.add scene
             (Scene.rectangle (x -. w2, y -. h2, x +. w2, y +. h2)
                fillcolor color)
       | _ ->
           Scene.add scene
             (Scene.Ellipse (x, y, width /. 2., height /. 2., fillcolor, color))
       end;
       let font_color =
         parse_color
          (try StringMap.find "color" n.G.node_attr with Not_found -> "black")
       in
       let font_size =
         try
           parse_float (StringMap.find "fontsize" n.G.node_attr)
         with Not_found ->
           14.
       in
       let label =
         (*XXX Parse...*)
         try StringMap.find "label" n.G.node_attr with Not_found -> n.G.name
       in
       let font_family = "serif" in
       Scene.add scene
         (Scene.Text (x, y +. height *. 0.1, label,
                      (font_family, font_size), font_color, None));
    ()
    )
    g.G.nodes.G.seq;
  IntMap.iter
    (fun _ e ->
(*      Format.eprintf "%s -> %s@." e.G.tail.G.name e.G.head.G.name;*)
      let color =
        parse_color
          (try StringMap.find "color" e.G.edge_attr with Not_found -> "black")
      in
      let arrow_size =
        try
          parse_float (StringMap.find "arrowsize" e.G.edge_attr)
        with Not_found -> 1.
      in

      parse_spline scene (StringMap.find "pos" e.G.edge_attr) color arrow_size;
    ())
    g.G.edges.G.seq;

  (bbox, scene)
