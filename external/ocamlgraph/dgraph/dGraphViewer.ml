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

open Ocamlgraph
open Dgraph
open Printf

let ($) f x = f x

let debug = false

type state = {
  mutable file : string option;
  mutable random : bool;
  mutable window : GWindow.window;
  mutable view : DGraphViewItem.common_view option;
  mutable table : GPack.table option;
  mutable status : GMisc.label;
}

(* Creates a scrolled graphView in a table *)
let scrolled_view ~packing view frame =
  ignore $ view#set_center_scroll_region true;
  let table = GPack.table ~packing
                ~rows:2 ~columns:2 ~row_spacings:4 ~col_spacings:4 () in
  ignore $ table#attach ~left:0 ~right:1 ~top:0 ~bottom:1
           ~expand:`BOTH ~fill:`BOTH ~shrink:`BOTH ~xpadding:0 ~ypadding:0
           frame#coerce;
  let w = GRange.scrollbar `HORIZONTAL ~adjustment:view#hadjustment () in
  ignore $ table#attach ~left:0 ~right:1 ~top:1 ~bottom:2
            ~expand:`X ~fill:`BOTH ~shrink:`X ~xpadding:0 ~ypadding:0
            w#coerce;
  let w = GRange.scrollbar `VERTICAL ~adjustment:view#vadjustment () in
  ignore $ table#attach ~left:1 ~right:2 ~top:0 ~bottom:1
            ~expand:`Y ~fill:`BOTH ~shrink:`Y ~xpadding:0 ~ypadding:0 
            w#coerce;
  table

let init_state () = 
   let window = GWindow.window ~title:"Graph Widget"
                 ~allow_shrink:true ~allow_grow:true () in
   let status = GMisc.label ~markup:"" () in
   status#set_use_markup true;
   let random = ref false in
   let file = ref None in
   for i=1 to Array.length Sys.argv - 1 do
     if Sys.argv.(i) = "--random" then
       random := true
     else
       file := Some Sys.argv.(i)
   done;
   { file = !file;
     random = !random;
     window = window;
     view = None;
     table = None;
     status = status }

(* Top menu *)

let menu_desc = "<ui>\
  <menubar name='MenuBar'>\
    <menu action='FileMenu'>\
      <menuitem action='Open'/>\
      <menuitem action='Zoom fit'/>\
      <menuitem action='Quit'/>\
    </menu>\
  </menubar>
</ui>"

let update_state state ~packing =
  begin match state.table with
    | None -> ()
    | Some t -> t#destroy ()
  end;
  match state.file with	
    | Some file ->
	if debug then printf "Building Model...\n";
	let model = 
	  if Filename.check_suffix file "xdot" then
	    DGraphModel.read_xdot ~xdot_file:file
	  else
	    DGraphModel.read_dot ~cmd:"dot" ~dot_file:file in
	if debug then printf "Building View...\n";
	let frame = GBin.frame ~shadow_type:`IN () in
	let aa = true (* anti-aliasing *) in
	let view = 
	  DGraphView.labeled_view
	    ~aa ~width:1280 ~height:1024 ~packing:frame#add
	    model state.status () 
	in
	let table = scrolled_view ~packing view frame in
	state.file <- Some file;
	state.view <- Some (view :> DGraphViewItem.common_view);
	state.table <- Some table;
	state.window#show ();
	view#misc#show ();
	ignore $ view#adapt_zoom ()
    | None when state.random ->
	let model = DGraphRandModel.create () in	
	let frame = GBin.frame ~shadow_type:`IN () in
	let aa = true (* anti-aliasing *) in
	let view = 
	  DGraphView.labeled_view
	    ~aa ~width:1280 ~height:1024 ~packing:frame#add
	    model state.status () 
	in
	let table = scrolled_view ~packing view frame in
	state.view <- Some (view :> DGraphViewItem.common_view);
	state.table <- Some table;
	state.window#show ();
	view#misc#show ();
	(* ignore $ view#adapt_zoom () *)
    | None -> ()

let all_files () =
  let f = GFile.filter ~name:"All" () in
  f#add_pattern "*" ;
  f

let open_file state ~packing () = 
  let dialog = GWindow.file_chooser_dialog 
    ~action:`OPEN
    ~title:"Open File"
    ~parent:state.window () in
  dialog#add_button_stock `CANCEL `CANCEL ;
  dialog#add_select_button_stock `OPEN `OPEN ;
  dialog#add_filter (all_files ()) ;
  match dialog#run () with
  | `OPEN ->
      state.file <- dialog#filename;
      dialog#destroy ();
      update_state state ~packing
  | `DELETE_EVENT | `CANCEL -> dialog#destroy ()

let create_menu state ~packing =
  let ui_m = GAction.ui_manager () in
  let actions = GAction.action_group ~name:"Actions" () in
  GAction.add_actions actions [
    GAction.add_action "FileMenu" ~label:"File" ;
    GAction.add_action "Open" ~label:"Open" ~accel:"<Control>o" ~stock:`OPEN
      ~callback:(fun _ -> open_file state ~packing ());
    GAction.add_action
      "Zoom fit" ~label:"Zoom fit" ~accel:"<Control>t" ~stock:`ZOOM_FIT
      ~callback:
      (fun _ -> match state.view with Some v -> v#adapt_zoom() | None -> ());
    GAction.add_action "Quit" ~label:"Quit" ~accel:"<Control>q" ~stock:`QUIT
      ~callback:(fun _ -> GMain.Main.quit ());
  ];
  ui_m#insert_action_group actions 0 ;
  ignore $ ui_m#add_ui_from_string menu_desc;
  ui_m

(* Main loop *)

let main () =
  (* GUI *)
  let state = init_state () in
  let vbox = 
    GPack.vbox ~border_width:4 ~spacing:4 ~packing:state.window#add () 
  in
  let packing = vbox#pack ~expand:true ~fill:true in
  (* Menu *)
  let ui_m = create_menu state ~packing in
  state.window#add_accel_group ui_m#get_accel_group ;
  vbox#pack ~expand:false (ui_m#get_widget "/MenuBar");
  vbox#pack (state.status :> GObj.widget);
  ignore $ state.window#connect#destroy ~callback:GMain.Main.quit;
  if debug then printf "GUI built, time: %f\n" (Sys.time ());
  update_state state ~packing;
  state.window#show ();
  GMain.Main.main ()
    
(* [JS 2009/09/21] Printexc.print prevents to use ocaml < 3.11 *)
let _ = (*Printexc.print*) main ()
