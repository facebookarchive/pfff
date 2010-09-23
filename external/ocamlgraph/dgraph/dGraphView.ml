(**************************************************************************)
(*                                                                        *)
(*  This file is part of OcamlGraph.                                      *)
(*                                                                        *)
(*  Copyright (C) 2009                                                    *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1, with a linking exception.                    *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the file ../LICENSE for more details.                             *)
(*                                                                        *)
(*  Authors:                                                              *)
(*    - Jean-Denis Koeck (jdkoeck@gmail.com)                              *)
(*    - Julien Signoles  (Julien.Signoles@cea.fr)                         *)
(*                                                                        *)
(**************************************************************************)

open DGraphViewItem
open Printf

let ($) f x = f x

(* SIMPLE VIEW CLASS *)

(* Widget derived from the Gnome Canvas
   Supports zooming and scrolling
*)
class ['v, 'e, 'c] view obj (model : ('v, 'e, 'c) DGraphModel.abstract_model) =
  let ((x1,y2),(x2,y1)) = model#bounding_box in
object(self)
  inherit GnoCanvas.canvas obj

  method model = model

  (* Hash tables from the model to the view items*)
  val node_hash : ('v, 'v view_node) Hashtbl.t = Hashtbl.create 30
  val edge_hash : ('e, 'e view_edge) Hashtbl.t = Hashtbl.create 30
  val cluster_hash : ('c, 'c view_cluster) Hashtbl.t = Hashtbl.create 30

  (* Canvas items creation *)
    
  method private add_vertex vertex =
    try
      let layout = model#get_vertex_layout vertex in
      let item = DGraphViewItem.view_node ~view:(self:>common_view)
	~vertex ~layout () in
      Hashtbl.add node_hash vertex item
    with Not_found -> ()

  method private add_edge edge =
    try
      let layout = model#get_edge_layout edge in
      let item = DGraphViewItem.view_edge ~view:(self:>common_view)
	~edge ~layout () in
      Hashtbl.add edge_hash edge item
    with Not_found -> ()

  method private add_cluster cluster =
    let layout = model#get_cluster_layout cluster in
    let item = DGraphViewItem.view_cluster ~view:(self :> common_view)
      ~cluster ~layout () in
    Hashtbl.add cluster_hash cluster item
 
  (* From model to view items *)
  method get_node = Hashtbl.find node_hash
  method get_edge = Hashtbl.find edge_hash
  method get_cluster = Hashtbl.find cluster_hash

  (* Iterate on nodes and edges *)
  method iter_nodes f = Hashtbl.iter (fun _ v -> f v) node_hash
  method iter_edges_e f = Hashtbl.iter (fun _ e -> f e) edge_hash
  method iter_clusters f = Hashtbl.iter (fun _ c -> f c) cluster_hash

  method iter_edges f =
    model#iter_edges
      (fun v1 v2 -> f (self#get_node v1) (self#get_node v2))

  (* Iterate on successors of a node *)
  method iter_succ f (node : 'v DGraphViewItem.view_node) =
    let f' v = f (self#get_node v) in
    model#iter_succ f' node#vertex

  (* Iterate on predecessors of a node *)
  method iter_pred f (node : 'v DGraphViewItem.view_node) =
    let f' v = f (self#get_node v) in
    model#iter_pred f' node#vertex

  method iter_succ_e f (node : 'v view_node) =
    let f' e = f (self#get_edge e) in
    model#iter_succ_e f' node#vertex

  method iter_pred_e f (node : 'v view_node) =
    let f' e = f (self#get_edge e) in
    model#iter_pred_e f' node#vertex

  (* Membership functions *)
  method mem_edge (n1:'v DGraphViewItem.view_node)
                  (n2:'v DGraphViewItem.view_node) =
    model#mem_edge n1#vertex n2#vertex
  method find_edge (n1:'v DGraphViewItem.view_node)
                   (n2:'v DGraphViewItem.view_node) =
    self#get_edge (model#find_edge n1#vertex n2#vertex)
  method src (e : 'e DGraphViewItem.view_edge) = self#get_node (model#src e#edge)
  method dst (e : 'e DGraphViewItem.view_edge) = self#get_node (model#dst e#edge)

  (* Zoom factor *)
  val mutable zoom_f = 1.
  method zoom_factor = zoom_f

  method private set_zoom_f x =
    if x > 1e-10 then zoom_f <- x

  (* Zooms the canvas according to the zoom factor *)
  method private zoom () =
    self#set_pixels_per_unit zoom_f;
    let zoom_text t =
      let new_size = t#init_size *. zoom_f in
      t#resize new_size in
    self#iter_nodes (fun n -> List.iter zoom_text n#texts);
    self#iter_edges_e (fun e -> List.iter zoom_text e#texts);
    self#iter_clusters (fun c -> List.iter zoom_text c#texts)

  (* Zoom to a particular factor *)
  method zoom_to x =
    self#set_zoom_f x;
    self#zoom ()

  method zoom_in () =
    self#set_zoom_f (zoom_f +. 0.1);
    self#zoom ()

  method zoom_out () =
    self#set_zoom_f (zoom_f -. 0.1);
    self#zoom ()

  method adapt_zoom () =
    let (x1',y1') = self#w2c ~wx:x1 ~wy:y1 in
    let (x2',y2') = self#w2c ~wx:x2 ~wy:y2 in
    let w = self#hadjustment#page_size in 
    let h = self#vadjustment#page_size in 
    let w_zoom = 0.9 *. w /. float_of_int (x2' - x1') in
    let h_zoom = 0.9 *. h /. float_of_int (y2' - y1') in
    self#zoom_to (min w_zoom h_zoom);
    ignore $ self#scroll_to ~x:x1' ~y:y1';

  (* EVENTS *)

  (* Zoom with the keys *)
  method private zoom_keys_ev ev =
    match GdkEvent.Key.keyval ev with
      | k when k = GdkKeysyms._KP_Subtract -> self#zoom_out (); true
      | k when k = GdkKeysyms._KP_Add   -> self#zoom_in ();  true
      | _ -> false

  (* Zoom with the mouse *)
  method private zoom_mouse_ev ev =
    match GdkEvent.Scroll.direction ev with
      | `UP ->   self#zoom_in ();  true
      | `DOWN -> self#zoom_out (); true
      | _ -> false

  initializer
    (* Create and add items from the model vertices, edges and clusters *)
    model#iter_vertex self#add_vertex;
    model#iter_edges_e self#add_edge;
    model#iter_clusters self#add_cluster;

    (* Set up scroll region *)
    ignore $ self#set_scroll_region ~x1 ~y1 ~x2 ~y2;
    let (x1',y1') = self#w2c ~wx:x1 ~wy:y1 in
    ignore $ self#scroll_to ~x:x1' ~y:y1';

    (* Attach zoom events *)
    ignore $ self#event#connect#key_press self#zoom_keys_ev;
    ignore $ self#event#connect#scroll self#zoom_mouse_ev;

    (* Set background color *)
    (*self#misc#modify_bg [`NORMAL, `NAME model#background_color];*)
end

(* Constructor copied from gnoCanvas.ml *)
let view ?(aa=false) model =
  GContainer.pack_container [] ~create:(fun pl ->
    let w =
      if aa then GnomeCanvas.Canvas.new_canvas_aa ()
      else GnomeCanvas.Canvas.new_canvas () in
    Gobject.set_params w pl;
    new view w model)

(* VIEW CLASS AUGMENTED WITH HIGHLIGHTING AND FOCUS *)

class ['v, 'e, 'c] highlight_focus_view obj model =
object(self)
  inherit ['v, 'e, 'c] view obj model as view

  (* EVENTS *)
    
  (* Highlighting *)
  method private highlight_node_ev node = function
    | `MOTION_NOTIFY _ ->
	node#highlight ();
	(* Highlight successors *)
	view#iter_succ_e
	  (fun e -> e#highlight ())
	  node;
	false
    | `LEAVE_NOTIFY _ ->
	node#dehighlight ();
	(* De-highlight successors *)
	view#iter_succ_e
	  (fun e -> e#dehighlight ())
	  node;
	false
    | _ -> false

  method private center_node_ev node = function
    | `TWO_BUTTON_PRESS _ -> node#center (); true
    | _ -> false

  initializer
    (* Connect highligh events on node shapes *)
    let connect_node n =
      let highlight = self#highlight_node_ev n in
      let center = self#center_node_ev n in

      n#iter_shapes (fun s -> ignore $ s#connect#event ~callback:highlight);
      n#iter_texts  (fun t -> ignore $ t#connect#event ~callback:highlight);

      n#iter_shapes (fun s -> ignore $ s#connect#event ~callback:center);
      n#iter_texts  (fun t -> ignore $ t#connect#event ~callback:center) in

    self#iter_nodes connect_node;
end

let highlight_focus_view ?(aa=false) model =
  GContainer.pack_container [] ~create:(fun pl ->
    let w =
      if aa then GnomeCanvas.Canvas.new_canvas_aa ()
      else GnomeCanvas.Canvas.new_canvas () in
    Gobject.set_params w pl;
    new highlight_focus_view w model)

(* LABELED VIEW *)
(* View augmented with a label showing the dot name of the node under the pointer *)
class ['v,'e,'c] labeled_view obj model (label:GMisc.label) =
object(self)
  inherit ['v,'e,'c] highlight_focus_view obj model as view

  (* EVENTS *)
  method private show_node_name node = function
    | `ENTER_NOTIFY _ ->
       let layout = model#get_vertex_layout node#vertex in
       let name = layout.XDot.n_name in
       label#set_text name;
       false
    | `LEAVE_NOTIFY _ ->
	label#set_text "";
	false
    | _ -> false

  initializer
  let connect_node n =
    let callback = self#show_node_name n in
    n#iter_shapes (fun s -> ignore $ s#connect#event ~callback);
    n#iter_texts  (fun t -> ignore $ t#connect#event ~callback) in
  self#iter_nodes connect_node
end

let labeled_view ?(aa=false) model label =
  GContainer.pack_container [] ~create:(fun pl ->
    let w =
      if aa then GnomeCanvas.Canvas.new_canvas_aa ()
      else GnomeCanvas.Canvas.new_canvas () in
    Gobject.set_params w pl;
    new labeled_view w model label)

(* VIEW CLASS AUGMENTED WIDTH DRAGGING *)
(* Not really working, not exported *)
class ['v, 'e, 'c] drag_view obj model =
object(self)
  inherit ['v, 'e, 'c] highlight_focus_view obj model
    
  val mutable drag = false
  val mutable prev_pos = (0.,0.)

  (* EVENTS *)
    
  method private drag_start button =
    if not drag then begin
      drag <- true;
      let wx,wy = GdkEvent.Button.x button, GdkEvent.Button.y button in
      let x,y = self#w2c_d ~wx ~wy in
      prev_pos <- x, y
    end;
    false

  method private drag_end _button =
    if drag then begin
      drag <- false;  
    end;
    false

  method private drag_move motion =
    let wx',wy' = GdkEvent.Motion.x motion, GdkEvent.Motion.y motion in
    let x',y' = self#w2c_d ~wx:wx' ~wy:wy' in

    if drag then begin
      let x,y = prev_pos in
      let dx, dy = (x'-.x) , (y'-.y)  in
	      
      let offx, offy = self#hadjustment#value, self#vadjustment#value in
      let f = self#zoom_factor in
      let dx_scroll = dx /. f in
      let dy_scroll = dy /. f in
      
      self#hadjustment#set_value (offx -. dx_scroll);
      self#vadjustment#set_value (offy -. dy_scroll);
    end;

    prev_pos <- (x',y');
    false

  initializer
    (* Attach drag events *)
    ignore $ self#event#connect#button_press self#drag_start;
    ignore $ self#event#connect#button_release self#drag_end;
    ignore $ self#event#connect#motion_notify self#drag_move
end

let drag_view ?(aa=false) model =
  GContainer.pack_container [] ~create:(fun pl ->
    let w =
      if aa then GnomeCanvas.Canvas.new_canvas_aa ()
      else GnomeCanvas.Canvas.new_canvas () in
    Gobject.set_params w pl;
    new highlight_focus_view w model)
