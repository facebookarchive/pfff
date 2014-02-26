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

(* ugly, todo use model.root instead? *)
let root_orig () = 
  (Common2.list_last !Controller.dw_stack).M.current_root

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
 * we copy the pixels from the pixmap dw.overlay on the windown
 * getting the final result.
 *)
let assemble_layers cr_final dw ~width ~height =
  ignore(width);
  ignore(height);

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
  Common.profile_code "View.expose" (fun () -> expose2 a b c)
(*e: expose *)

(*s: configure *)
let configure2_bis da dw_ref ev = 
  ignore(da);
  let dw = !dw_ref in

  let w = GdkEvent.Configure.width ev in
  let h = GdkEvent.Configure.height ev in

  dw.width <- w;
  dw.height <- h;
  dw.pm <- Model2.new_pixmap dw.width dw.height;
  dw.overlay <- 
    Cairo.surface_create_similar (CairoH.surface_of_pixmap dw.pm)
    Cairo.CONTENT_COLOR_ALPHA w h;
    
  View_mainmap.paint dw;
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
  Common.profile_code "View.configure" (fun () -> configure2 a b c)
(*e: configure *)

(* ---------------------------------------------------------------------- *)
(* The legend *)
(* ---------------------------------------------------------------------- *)
(*s: expose_legend *)
let expose_legend da dw_ref _ev = 
  let cr = Cairo_lablgtk.create da#misc#window in

  (* todo: make the architecture a layer so no need for special case *)
  let dw = !dw_ref in
  (if not (Layer_code.has_active_layers dw.layers)
  then Draw_legend.draw_legend ~cr
  else Draw_legend.draw_legend_layer ~cr dw.layers
  );
  true
(*e: expose_legend *)

(*****************************************************************************)
(* Events *)
(*****************************************************************************)

(*****************************************************************************)
(* The main UI *)
(*****************************************************************************)

(*s: mk_gui() *)
let mk_gui ~screen_size ~legend test_mode (root, model, dw, _dbfile_opt) =

  let dw = ref dw in
  Common.push !dw Controller.dw_stack;

  let width, height, minimap_hpos, minimap_vpos = 
    Style.windows_params screen_size in

  let w = GWindow.window 
    ~title:(Controller.title_of_path root)
    ~width
    ~height
    ~allow_shrink: true
    ~allow_grow:true
    () 
  in
  Controller._set_title := (fun s -> w#set_title s);

  let statusbar = GMisc.statusbar () in
  let ctx = statusbar#new_context "main" in
  Controller._statusbar_addtext := (fun s -> ctx#push s +> ignore);

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
  let hbox = GPack.hbox ~packing:(vbox#pack ~expand:false ~fill:false) () in

    (*-------------------------------------------------------------------*)
    (* Menu *)
    (*-------------------------------------------------------------------*)
    hbox#pack (G.mk (GMenu.menu_bar) (fun m -> 
      
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
          !Controller._go_back dw;
        ) +> ignore;

        fc#add_item "_Go to example" ~key:K._E ~callback:(fun () -> 
          let model = !dw.model in
          let model = Async.async_get model in
          match !dw.current_entity, model.db with
          | Some e, Some db ->
              (match e.Db.e_good_examples_of_use with
              | [] -> failwith "no good examples of use for this entity"
              | x::_xs ->
                  let e = db.Db.entities.(x) in
                  let file = e.Db.e_file in

                  let final_file = 
                    Model_database_code.readable_to_absolute_filename_under_root
                      file ~root:!dw.current_root in

                  !Controller._go_dirs_or_file 
                    ~current_entity:(Some e) dw [final_file];
              )
          | _ -> failwith "no entity currently selected or no db"
        ) +> ignore ;
      );

      factory#add_submenu "_Search" +> (fun menu -> 
        let fc = new GMenu.factory menu ~accel_group in

        (* todo? open Db ? *)
        fc#add_item "_Git grep" ~key:K._G ~callback:(fun () -> 

          let res = Ui_search.dialog_search_def !dw.model in
          res +> Common.do_option (fun s ->
            let root = 
              (* could also support local grep? and use !dw.root instead ?  *)
              root_orig ()
            in
            let matching_files = Ui_search.run_grep_query ~root s in
            let files = matching_files +> List.map fst +> Common2.uniq in
            let current_grep_query = 
              Some (Common.hash_of_list matching_files)
            in
            !Controller._go_dirs_or_file ~current_grep_query dw files
          );
        ) +> ignore;

        fc#add_item "_Tbgs query" ~key:K._T ~callback:(fun () -> 

          let res = Ui_search.dialog_search_def !dw.model in
          res +> Common.do_option (fun s ->
            let root = !dw.current_root in
            let matching_files = Ui_search.run_tbgs_query ~root s in
            let files = matching_files +> List.map fst +> Common2.uniq in
            let current_grep_query = 
              Some (Common.hash_of_list matching_files)
            in
            !Controller._go_dirs_or_file ~current_grep_query dw files
          );
        ) +> ignore;

      );
      factory#add_submenu "_Layers" +> (fun menu -> 
        let layers = 
          !dw.layers.Layer_code.layers +> List.map (fun (layer, active) ->
            (layer.Layer_code.title, active, (fun b -> 
              if b then
                Ui_layers.choose_layer ~root:(root_orig())
                  (Some layer.Layer_code.title) dw;
            ))
          )
        in
        (* todo: again, make the architecture a layer so less special cases *)
        let entries = [`R (
             ("Architecture", true, (fun _b ->
               Ui_layers.choose_layer ~root:(root_orig()) None dw;
             ))::
             layers)
        ]
        in
        GToolbox.build_menu menu ~entries
      );

      factory#add_submenu "_Misc" +> (fun menu -> 
        let fc = new GMenu.factory menu ~accel_group in

        (* todo? open Db ? *)

        fc#add_item "_Refresh" ~key:K._R ~callback:(fun () -> 
          let current_root = !dw.current_root in
          let _old_dw = Common2.pop2 Controller.dw_stack in
          !Controller._go_dirs_or_file dw [current_root];
        ) +> ignore;

      );

      factory#add_submenu "_Help" +> (fun menu -> 
        let fc = new GMenu.factory menu ~accel_group in

        fc#add_item "_Interface" ~key:K._H ~callback:(fun () -> 
            G.dialog_text Help.interface_doc "Help"
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
      ) () +> ignore;
      tb#insert_space ();

*)
      let idx = (fun () -> 
        let model = Async.async_get model in
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
            | Database_code.MultiDirs ->
                (* hack: coupling: with mk_multi_dirs_entity *)
                Common.split "|" e.Db.e_file
            | _ ->
                [e.Db.e_file]
          in

          let final_paths = 
            readable_paths +> List.map 
              (Model_database_code.readable_to_absolute_filename_under_root 
                 ~root:!dw.current_root)
          in

          pr2 (spf "e= %s, final_paths= %s" str(Common.join "|" final_paths));
          !Controller._go_dirs_or_file ~current_entity:(Some e) dw final_paths;
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
            View_overlays.draw_searched_rectangles ~dw:!dw;
            !Controller._refresh_da();
          end
        )
        idx
      in

      tb#insert_widget (G.with_label "Search:" entry#coerce);

      tb#insert_widget (G.mk (GButton.button ~stock:`GO_BACK) (fun b -> 
        b#connect#clicked ~callback:(fun () -> 
          !Controller._go_back dw;
        )
      ));

      tb#insert_widget (G.mk (GButton.button ~stock:`GO_UP) (fun b -> 
        b#connect#clicked ~callback:(fun () -> 
          let current_root = !dw.current_root in
          !Controller._go_dirs_or_file dw [Common2.dirname current_root];
        )
      ));

      tb#insert_widget (G.mk (GButton.button ~stock:`GOTO_TOP) (fun b -> 
        b#connect#clicked ~callback:(fun () -> 
          let top = Common2.list_last !Controller.dw_stack in
          (* put 2 in the stack because _go_back will popup one *)
          Controller.dw_stack := [top; top];
          !Controller._go_back dw;

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


    da#event#connect#expose ~callback:(expose da dw) +> ignore;
    da#event#connect#configure ~callback:(configure da dw) +> ignore;

    da3#event#connect#expose ~callback:(expose_legend da3 dw) +> ignore;

    da#event#connect#button_press   
      (View_mainmap.button_action da dw) +> ignore;
    da#event#connect#button_release 
      (View_mainmap.button_action da dw) +> ignore;

    da#event#connect#motion_notify  
      (View_overlays.motion_notify da dw) +> ignore; 

    Controller._refresh_da := (fun () ->
      GtkBase.Widget.queue_draw da#as_widget;
    );
    Controller._refresh_legend := (fun () ->
      GtkBase.Widget.queue_draw da3#as_widget;
    );

    Controller._go_back := Ui_navigation.go_back;
    Controller._go_dirs_or_file := Ui_navigation.go_dirs_or_file;
      
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
    let pb = "pb: " ^ string_of_exn exn ^ "\n" ^ s in
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
  test_mode +> Common.do_option (fun _s -> 
    (* View_test.do_command s model *)
    ()
  );

  (* Gui.gmain_idle_add ~prio: 1000 (idle dw) +> ignore; *)

  GtkThread.main ();
  ()
(*e: mk_gui() *)

(*e: view2.ml *)
