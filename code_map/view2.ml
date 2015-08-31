(*s: view2.ml *)
(*s: Facebook copyright *)
(* Yoann Padioleau
 * 
 * Copyright (C) 2010-2012 Facebook
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
open Common2
open Common

module G = Gui
module K = GdkKeysyms
module GR = Gdk.Rectangle
module F = Figures
module T = Treemap
module CairoH = Cairo_helpers
open Model2 (* for the fields *)
module M = Model2
module Controller = Controller2
module Flag = Flag_visual
module Style = Style2
module Db = Database_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2, _pr2_once = Common2.mk_pr2_wrappers Flag.verbose_visual

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

(*s: view globals *)
(* when some widgets need to access other widgets *)

(* Note that because we use toplevels 'let' for the GUI elements below,
 * Gtk must have also been initialized via a toplevel element, or
 * initialized by including gtkInit.cmo earlier in the linking command.
 *)

(*e: view globals *)

(*****************************************************************************)
(* Final view rendering *)
(*****************************************************************************)

(* ---------------------------------------------------------------------- *)
(* The main-map *)
(* ---------------------------------------------------------------------- *)

(*s: assemble_layers *)
(* Composing the "layers". See cairo/tests/knockout.ml example.
 * Each move of the cursor will call assemble_layers which does all
 * those pixels copying (which is very fast).
 * 
 * The final target is the actual gtk window which is represented by cr_final.
 * We copy the pixels from the pixmap dw.pm on the window. Then
 * we copy the pixels from the pixmap dw.overlay on the window
 * getting the final result.
 *)
let assemble_layers cr_final dw =
  let surface_src = dw.base in

  Cairo.set_operator cr_final Cairo.OPERATOR_OVER;
  Cairo.set_source_surface cr_final surface_src 0. 0.;
  Cairo.paint cr_final;

  Cairo.set_operator cr_final Cairo.OPERATOR_OVER;
  Cairo.set_source_surface cr_final dw.overlay 0. 0.;
  Cairo.paint cr_final;
  ()
(*e: assemble_layers *)

(*s: expose *)
(* opti: don't 'paint dw;' painting is the computation
 * heavy function. expose() just copy the "canvas" layers.
 *)
let expose2 da w _ev = 
  let dw = w.dw in
  let gwin = da#misc#window in
  let cr = Cairo_lablgtk.create gwin in
  assemble_layers cr dw;
  true

let expose a b c = 
  Common.profile_code "View.expose" (fun () -> expose2 a b c)
(*e: expose *)

(*s: configure *)
let configure2_bis w ev = 
  let dw = w.dw in
  let width = GdkEvent.Configure.width ev in
  let height = GdkEvent.Configure.height ev in
  dw.width <- width;
  dw.height <- height;
  dw.base <- Model2.new_surface ~alpha:false ~width ~height;
  dw.overlay <- Model2.new_surface ~alpha:true ~width ~height;
  View_mainmap.paint dw w.model;
  true

(* ugly: for some unknown reason configure get called twice at
 * the beginning of the program
 *)
let first_call = ref true
let configure2 a b =
  (* should probably do is_old_gtk() *)
  if !first_call && CairoH.is_old_cairo () 
  then begin first_call := false; true end
  else 
    configure2_bis a b

let configure a b =
  Common.profile_code "View.configure" (fun () -> configure2 a b)
(*e: configure *)

(* ---------------------------------------------------------------------- *)
(* The legend *)
(* ---------------------------------------------------------------------- *)
(*s: expose_legend *)
let expose_legend da w _ev = 
  let dw = w.dw in
  let cr = Cairo_lablgtk.create da#misc#window in

  (* todo: make the architecture a layer so no need for special case *)
  (if not (Layer_code.has_active_layers dw.layers)
  then Draw_legend.draw_legend ~cr
  else Draw_legend.draw_legend_layer ~cr dw.layers
  );
  true
(*e: expose_legend *)

(*****************************************************************************)
(* The main UI *)
(*****************************************************************************)

(*s: mk_gui() *)
let mk_gui ~screen_size ~legend test_mode w =
  let width, height, minimap_hpos, minimap_vpos = 
    Style.windows_params screen_size in

  let win = GWindow.window 
    ~title:(Controller.title_of_path w.dw.current_root)
    ~width ~height
    ~allow_shrink: true
    ~allow_grow:true
    () 
  in
  Controller._set_title := (fun s -> win#set_title s);

  let statusbar = GMisc.statusbar () in
  let ctx = statusbar#new_context "main" in
  Controller._statusbar_addtext := (fun s -> ctx#push s |> ignore);

  let accel_group = GtkData.AccelGroup.create () in
  win#misc#set_name "main window";

  let quit () = 
    (*Controller.before_quit_all model;*)
    GMain.Main.quit ();
  in

  win#add_accel_group accel_group;

  (*-------------------------------------------------------------------*)
  (* Layout *)
  (*-------------------------------------------------------------------*)

  (* if use my G.mk style for that, then get some pbs when trying
   * to draw stuff :(
   *)
  let vbox = GPack.vbox ~packing:win#add () in
  let hbox = GPack.hbox ~packing:(vbox#pack ~expand:false ~fill:false) () in

    (*-------------------------------------------------------------------*)
    (* Menu *)
    (*-------------------------------------------------------------------*)
    hbox#pack (G.mk (GMenu.menu_bar) (fun m -> 
      let factory = new GMenu.factory m in
(*
      factory#add_submenu "_File" |> (fun menu -> 
        let fc = new GMenu.factory menu ~accel_group in
        (* todo? open Db ? *)
        fc#add_item "_Open stuff from db" ~key:K._O ~callback:(fun () -> 
          ();
        ) |> ignore;
        fc#add_separator () |> ignore;

      factory#add_submenu "_Edit" |> (fun menu -> 
        GToolbox.build_menu menu ~entries:[
          `S;
        ];
      ) |> ignore;
*)

      factory#add_submenu "_Move" |> (fun menu -> 
        let fc = new GMenu.factory menu ~accel_group in

        (* todo? open Db ? *)
        fc#add_item "_Go back" ~key:K._B ~callback:(fun () -> 
          !Controller._go_back w;
        ) |> ignore;

        fc#add_item "_Go to example" ~key:K._E ~callback:(fun () -> 
          let model = Async.async_get w.model in
          match w.current_entity, model.db with
          | Some e, Some db ->
              (match e.Db.e_good_examples_of_use with
              | [] -> failwith "no good examples of use for this entity"
              | x::_xs ->
                  let e = db.Db.entities.(x) in
                  let file = e.Db.e_file in

                  let final_file = 
                    Model_database_code.readable_to_absolute_filename_under_root
                      file ~root:w.dw.current_root in
                  w.current_entity <- Some e;
                  !Controller._go_dirs_or_file w [final_file];
              )
          | _ -> failwith "no entity currently selected or no db"
        ) |> ignore;
        fc#add_item "_Quit" ~key:K._Q ~callback:quit |> ignore;
      );

      factory#add_submenu "_Search" |> (fun menu -> 
        let fc = new GMenu.factory menu ~accel_group in

        (* todo? open Db ? *)
        fc#add_item "_Git grep" ~key:K._G ~callback:(fun () -> 

          let res = Ui_search.dialog_search_def w.model in
          res |> Common.do_option (fun s ->
            let root = w.root_orig in
            let matching_files = Ui_search.run_grep_query ~root s in
            let files = matching_files |> List.map fst |> Common2.uniq in
            let current_grep_query = 
              Some (Common.hash_of_list matching_files)
            in
            !Controller._go_dirs_or_file ~current_grep_query w files
          );
        ) |> ignore;

        fc#add_item "_Tbgs query" ~key:K._T ~callback:(fun () -> 

          let res = Ui_search.dialog_search_def w.model in
          res |> Common.do_option (fun s ->
            let root = w.dw.current_root in
            let matching_files = Ui_search.run_tbgs_query ~root s in
            let files = matching_files |> List.map fst |> Common2.uniq in
            let current_grep_query = 
              Some (Common.hash_of_list matching_files)
            in
            !Controller._go_dirs_or_file ~current_grep_query w files
          );
        ) |> ignore;

      );
      factory#add_submenu "_Layers" |> (fun menu -> 
        let layers = 
          w.dw.layers.Layer_code.layers |> List.map (fun (layer, active) ->
            (layer.Layer_code.title, active, (fun b -> 
              if b then
                Ui_layers.choose_layer ~root:w.root_orig
                  (Some layer.Layer_code.title) w;
            ))
          )
        in
        (* todo: again, make the architecture a layer so less special cases *)
        let entries = [`R (
             ("Architecture", true, (fun _b ->
               Ui_layers.choose_layer ~root:w.root_orig None w;
             ))::
             layers)
        ]
        in
        GToolbox.build_menu menu ~entries
      );

      factory#add_submenu "_Misc" |> (fun menu -> 
        let fc = new GMenu.factory menu ~accel_group in

        (* todo? open Db ? *)

        fc#add_item "_Refresh" ~key:K._R ~callback:(fun () -> 
          let current_root = w.dw.current_root in
          let _old_dw = Common2.pop2 w.dw_stack in
          !Controller._go_dirs_or_file w [current_root];
        ) |> ignore;

        fc#add_item "Reset entity" ~callback:(fun () ->
          w.current_node_selected <- None;
          let cr_overlay = Cairo.create w.dw.overlay in
          CairoH.clear cr_overlay;
          !Controller._refresh_da();
        ) |> ignore;
      );

      factory#add_submenu "_Help" |> (fun menu -> 
        let fc = new GMenu.factory menu ~accel_group in

        fc#add_item "_Interface" ~key:K._H ~callback:(fun () -> 
            G.dialog_text Help.interface_doc "Help"
        ) |> ignore;

        fc#add_item "_Legend" ~key:K._L ~callback:(fun () -> 
          raise Todo
        ) |> ignore;

        fc#add_item "_Help on Pfff" ~callback:(fun () -> 
            G.dialog_text "Read\nthe\nsource\n\ndude" "Help"
        ) |> ignore;
        fc#add_separator () |> ignore;
        fc#add_item "About" ~callback:(fun () -> 
            G.dialog_text "Brought to you by pad\nwith love" "About"
        ) |> ignore;
      );
    ));

    (*-------------------------------------------------------------------*)
    (* toolbar *)
    (*-------------------------------------------------------------------*)
    hbox#pack ~padding:10 (G.mk (GButton.toolbar) (fun tb ->

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
      ) () |> ignore;
      tb#insert_space ();

*)
      let idx = (fun () -> 
        let model = Async.async_get w.model in
        model.Model2.big_grep_idx 
      )
      in

      let entry = 
        Completion2.my_entry_completion_eff 
         ~callback_selected:(fun entry str _file e ->
          (* pb is that we may have run the visualizer on a subdir
           * of what is mentionned in the database code. We have
           * then to find the real root.
           *)
          entry#set_text "";

          let readable_paths = 
            (* hack to handle multidirs *)
            match e.Db.e_kind with
            | Entity_code.MultiDirs ->
                (* hack: coupling: with mk_multi_dirs_entity *)
                Common.split "|" e.Db.e_file
            | _ ->
                [e.Db.e_file]
          in

          let final_paths = 
            readable_paths |> List.map 
              (Model_database_code.readable_to_absolute_filename_under_root 
                 ~root:w.dw.current_root)
          in

          pr2 (spf "e= %s, final_paths= %s" str(Common.join "|" final_paths));
          w.current_entity <- Some e;
          Async.async_get_opt w.model |> Common.do_option (fun model ->
            model.g |> Common.do_option (fun g ->
              w.current_node_selected <- 
                Model_graph_code.node_of_entity e g
            )
          );
          !Controller._go_dirs_or_file w final_paths;
          true
        )
        ~callback_changed:(fun str ->
          w.dw.current_query <- str;
          w.dw.current_searched_rectangles <- [];

          if w.settings.draw_searched_rectangles
          then begin
            (* better to compute once the set of matching rectangles
             * cos doing it each time in motify would incur too much
             * calls to ==~
             *)
            let minimum_length = 3 in

            if String.length str > minimum_length then begin

              let rects = w.dw.treemap in
              let re_opt = 
                try Some (Str.regexp (".*" ^ str))
               (* can raise exn when have bad or not yet complete regexp *)
                with _ -> None
              in
              let res = 
                match re_opt with
                | None -> []
                | Some re ->
                    rects |> List.filter (fun r -> 
                      let label = r.T.tr_label |> String.lowercase in
                      label ==~ re
                    )
              in
              w.dw.current_searched_rectangles <- res;
              
            end;
            let cr_overlay = Cairo.create w.dw.overlay in
            CairoH.clear cr_overlay;
            View_overlays.draw_searched_rectangles ~dw:w.dw;
            !Controller._refresh_da();
          end
        )
        idx
      in

      tb#insert_widget (G.with_label "Search:" entry#coerce);

      tb#insert_widget (G.mk (GButton.button ~stock:`GO_BACK) (fun b -> 
        b#connect#clicked ~callback:(fun () -> 
          !Controller._go_back w;
        )
      ));

      tb#insert_widget (G.mk (GButton.button ~stock:`GO_UP) (fun b -> 
        b#connect#clicked ~callback:(fun () -> 
          let current_root = w.dw.current_root in
          !Controller._go_dirs_or_file w [Common2.dirname current_root];
        )
      ));

      tb#insert_widget (G.mk (GButton.button ~stock:`GOTO_TOP) (fun b -> 
        b#connect#clicked ~callback:(fun () -> 
          let top = Common2.list_last !(w.dw_stack) in
          (* put 2 in the stack because _go_back will popup one *)
          w.dw_stack := [top; top];
          !Controller._go_back w;
        )
      ));
    ));

    (*-------------------------------------------------------------------*)
    (* main view *)
    (*-------------------------------------------------------------------*)

    let hpane = GPack.paned `HORIZONTAL
      ~packing:(vbox#pack ~expand:true ~fill:true) () in

    let da = GMisc.drawing_area () in
    da#misc#set_double_buffered false;
    hpane#add1 da#coerce;

   
    let vpane = GPack.paned `VERTICAL () in
    hpane#set_position minimap_hpos;

    let da3 = GMisc.drawing_area () in
    vpane#set_position minimap_vpos;
    vpane#add2 da3#coerce;


    if legend then hpane#add2 vpane#coerce;

    da#misc#set_can_focus true ;
    da#event#add [ `KEY_PRESS;
                   `BUTTON_MOTION; `POINTER_MOTION;
                   `BUTTON_PRESS; `BUTTON_RELEASE ];

    (* subtle: do not change those callbacks to get a dw; you need to
     * pass a w! Indee if you do ~callback:(expose da w.dw)
     * and an event changes w.dw (e.g. a resize of the window)
     * then the expose event will still expose the old drawing.
     *)
    da#event#connect#expose ~callback:(expose da w) |> ignore;
    da#event#connect#configure ~callback:(configure w) |> ignore;
    da3#event#connect#expose ~callback:(expose_legend da3 w) |> ignore;

    da#event#connect#button_press 
      (View_mainmap.button_action w) |> ignore;
    da#event#connect#button_release 
      (View_mainmap.button_action w) |> ignore;
    da#event#connect#motion_notify  
      (View_overlays.motion_notify w) |> ignore; 

    Controller._refresh_da := (fun () ->
      GtkBase.Widget.queue_draw da#as_widget;
    );
    Controller._refresh_legend := (fun () ->
      GtkBase.Widget.queue_draw da3#as_widget;
    );
    Controller._go_back := Ui_navigation.go_back;
    Controller._go_dirs_or_file := Ui_navigation.go_dirs_or_file;
    Controller.hook_finish_paint := (fun () ->
      View_overlays.hook_finish_paint w
    );
      
    (*-------------------------------------------------------------------*)
    (* status bar *)
    (*-------------------------------------------------------------------*)
    (* the statusbar widget is defined in beginning of this file because *)
    vbox#pack (*~from: `END*) statusbar#coerce;

  (*  )); *)

  (*-------------------------------------------------------------------*)
  (* End *)
  (*-------------------------------------------------------------------*)

  (* Controller._before_quit_all_func |> Common.push2 Model.close_model; *)

  GtkSignal.user_handler := (fun exn -> 
    pr2 "fucking callback";
    (* old: before 3.11: Features.Backtrace.print(); *)
    let s = Printexc.get_backtrace () in
    pr2 s;
    let pb = "pb: " ^ string_of_exn exn ^ "\n" ^ s in
    G.dialog_text ~text:pb ~title:"pb";
    raise exn
  );

  (* TODO: should do that on 'da', not 'w' 
  w#event#connect#key_press ~callback:(key_pressed (da,da2) dw) |> ignore;
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

  win#event#connect#delete    ~callback:(fun _  -> quit(); true) |> ignore;
  win#connect#destroy         ~callback:(fun () -> quit(); ) |> ignore;
  win#show ();

  (* test *)
  test_mode |> Common.do_option (fun _s -> 
    (* View_test.do_command s model *)
    ()
  );
  (* Gui.gmain_idle_add ~prio: 1000 (idle dw) |> ignore; *)
  GtkThread.main ();
  ()
(*e: mk_gui() *)

(*e: view2.ml *)
