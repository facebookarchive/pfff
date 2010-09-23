open Common

module G = Gui
module GKey = GdkKeysyms


open Model (* for field access *)

module Db = Database_php
module ES = Entities_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*
 * Overall organisation:
 *  - menu
 *  - toolbar
 *  - mainview
 *    left         right
 *    ----         -----
 *    playlist     instrinc props
 *                 objects
 *                 source
 *  - statusbar 
 * 
 * Conventions and info: cf commons/gui.ml
 *)

(*****************************************************************************)
(* Globals, when some widgets need to access other widgets *)
(*****************************************************************************)
(* can also use the Controller module to achieve sometimes the same thing *)

let statusbar = 
  GMisc.statusbar () 

let ctx = 
  statusbar#new_context "main" 

let statusbar_addtext s = 
  ctx#push s +> ignore


(* ------------------------------------------------------------------------- *)
let default_import_smpl_file path_acomment = 
  Filename.concat path_acomment "data/sgrep/foo.sgrep"

let default_import_annot_file path_acomment = 
  Filename.concat path_acomment "data/annotated_www.list"


(*****************************************************************************)
(* The main UI *)
(*****************************************************************************)

let mk_gui model path_xpm test_mode =
  let dbname = 
    Database_php.path_of_project model.db.Database_php.project
  in

  let w = GWindow.window 
    ~title: ("GUI on project: " ^ dbname)
    ~width:900 ~height:700 (* 1024 x 768. note that Lin prefer 700 *)
    ~allow_shrink: true
    () 
  in
  let accel_group = GtkData.AccelGroup.create () in
  w#misc#set_name "main window";

  let quit () = 
    Controller.before_quit_all model;
    GMain.Main.quit ();
  in

  View_helpers.start_build_completion_defs model;

  w#add_accel_group accel_group;
  
  (*-------------------------------------------------------------------*)
  (* Layout *)
  (*-------------------------------------------------------------------*)
  (*                         
            w
            vbox
             menubar (m)                 
             toolbar (tb)
             main view (hp) 
             statusbar
  *)

  w#add (G.mk (GPack.vbox) (fun vbox -> 
    (*-------------------------------------------------------------------*)
    (* Menu *)
    (*-------------------------------------------------------------------*)
    vbox#pack (G.mk (GMenu.menu_bar) (fun m -> 
      
      let factory = new GMenu.factory m in

      factory#add_submenu "_File" +> (fun menu -> 
        let fc = new GMenu.factory menu ~accel_group in

        (* todo? open Db ? *)

        fc#add_item "_Open PHP file from db" ~key:GKey._O ~callback:(fun () -> 
          View_dialogs.dialog_open_file model;
        );
        fc#add_separator ();
(*
        fc#add_item "Load annotation file"~callback:(fun () -> 
          View_dialogs.dialog_import_annot 
            ~default_file:(default_import_annot_file model.srcpath)
            model
        );
        fc#add_item "Save annotation file"~callback:(fun () -> 
          View_dialogs.dialog_save_annot model
        );
        fc#add_separator ();
*)
        fc#add_item "_Quit" ~key:GKey._Q ~callback:quit;
      );



      factory#add_submenu "_Edit" +> (fun menu -> 
        GToolbox.build_menu menu ~entries:[
          `S;
        ];
      );

      factory#add_submenu "_Search" +> (fun menu -> 

        let fc = new GMenu.factory menu ~accel_group in

        fc#add_item "Search _Entity Definition" ~key:GKey._E ~callback:(fun ()->
            View_dialogs.dialog_search_def model;
          );

        fc#add_item "Search _Glimpse" ~key:GKey._G ~callback:(fun ()->
            View_dialogs.dialog_search_glimpse model;
          );
        fc#add_item "Search _SmPL" ~key:GKey._S ~callback:(fun ()->
            View_dialogs.dialog_search_smpl_inline model;
          );

        fc#add_item "Search SmPL script" ~key:GKey._F ~callback:(fun ()->
            View_dialogs.dialog_search_smpl_file 
              ~default_file:(default_import_smpl_file model.pfff_srcpath)
              model;
          );

        fc#add_item "Search SmPL script with 2 patterns" (*~key:GKey._F*) ~callback:(fun ()->
            View_dialogs.dialog_search_smpl_double 
              ~default_file:(default_import_smpl_file model.pfff_srcpath)
              model;
          );

        fc#add_item "Search Words in comments" ~key:GKey._M ~callback:(fun ()->
            View_dialogs.dialog_search_words_in_comment model;
          );


        fc#add_separator ();
        fc#add_item "Callers of current id" ~callback:(fun () -> 
            View_dialogs.dialog_callers model;
          );
        fc#add_item "Callees of current id" ~callback:(fun () -> 
            View_dialogs.dialog_callees model;
          );
        fc#add_item "Build Callers tree of current id" ~callback:(fun () -> 
            View_dialogs.build_callers model;
          );
        fc#add_item "Build Callees tree of current id" ~callback:(fun () -> 
            View_dialogs.build_callees model;
          );

        fc#add_item "Build Use of global list of current id" ~callback:(fun () -> 
            View_dialogs.build_user model;
          );
        fc#add_item "Build use of struct/field/typedef/etc" ~callback:(fun ()->
            View_dialogs.build_use_xxx model;
          );

        fc#add_separator ();
        fc#add_item "Goto def of entity under point" ~callback:(fun () -> 
          (* normally, can do it also via double click on entity in view *)
          View_dialogs.goto_def_entity_under_point model;
        );


      );

      factory#add_submenu "_View" +> (fun menu -> 
        GToolbox.build_menu menu ~entries:[
          (* left view *)
          `I ("Expand all", (fun () -> 
            !Controller.expand_all();
          ));
          `I ("Expand Level 1", (fun () -> 
            !Controller.expand_level 1;
          ));
          `I ("Expand Level 2", (fun () -> 
            !Controller.expand_level 2;
          ));
          `I ("Expand Level 3", (fun () -> 
            !Controller.expand_level 3;
          ));
          `S;
          (* annot view *)
          `I ("Annotation view", (fun () -> 
            raise Todo
          ));
          `S;
          (* code view *)
          `C ("Show type error", model.show_type_error, 
             (fun b -> model.show_type_error <- b)
          );
          
        ];
      );


      factory#add_submenu "_Settings" +> (fun menu -> 
        GToolbox.build_menu menu ~entries:[
          `C ("Show type error", model.show_type_error, 
             (fun b -> model.show_type_error <- b)
          );
          `C ("Squeeze same target", true,
             (fun b -> raise Todo)
               (*
             model.calltree_prefs.Callgraph.squeeze_duplicate,
             (fun b -> 
               model.calltree_prefs <- { model.calltree_prefs with
                 Callgraph.squeeze_duplicate = true;
               }
             )
               *)
          );
          `C ("Show type in sexp", !Sexp_ast_php.show_expr_info, 
             (fun b -> Sexp_ast_php.show_expr_info := b)
          );

          `I ("www setting", (fun () ->  
            raise Todo
          ));
          `C ("Abstract prototype and extern in goto id", 
             model.abstract_proto_and_decl, 
             (fun b -> model.abstract_proto_and_decl <- b)
          );

          
        ];
      );

      factory#add_submenu "_Analysis" +> (fun menu -> 

        let fc = new GMenu.factory menu ~accel_group in

        fc#add_item "Check protocol" ~callback:(fun ()->
            View_dialogs.dialog_check_protocol 
              ~default_file:(default_import_smpl_file model.pfff_srcpath)
              model;
          );
      );



      factory#add_submenu "Annotations" +> (fun menu -> 

        let fc = new GMenu.factory menu ~accel_group in

        (* also in file menu, but maybe good to duplicate *)
        fc#add_item "Load annotation file"~callback:(fun () -> 
          View_dialogs.dialog_import_annot 
            ~default_file:(default_import_annot_file model.pfff_srcpath)
            model
        );

        fc#add_item "Check all annotations"~callback:(fun () -> 
            View_dialogs.check_all_annotations model;
        );

        fc#add_item "Add annotation on current id" ~callback:(fun ()->
          View_dialogs.add_annotation model;
        );

      );

      factory#add_submenu "_Information" +> (fun menu -> 

        let show_stat f name = 
            let xs = Common.with_pr2_to_string (fun () -> 
              Common._batch_mode := true;
              f model.db
            ) in
            xs +> List.iter pr2;
            ignore(
              G.dialog_text_large (Common.unlines xs)  (spf "%s stat" name)
            );
        in


        GToolbox.build_menu menu ~entries:[

          `I ("Show information on current id", (fun () -> 
            model.current_id +> Common.do_option (fun id -> 

              let typ = model.db.Db.defs.Db.id_type#assoc id in
              let s = Sexp_ast_php.string_of_phptype typ in
              ignore(G.dialog_text_large ~text:s ~title:"type");

            );

          ));
          `I ("Show ast (after annot) information on current id", (fun ()-> 
            model.current_id +> Common.do_option (fun id -> 
              let ast = model.db.Db.defs.Db.toplevels#assoc id in
              let s = Sexp_ast_php.string_of_toplevel ast in
              pr2 s;
              ignore(G.dialog_text_large ~text:s ~title:"ast");
            );
          ));
          `I ("Show sexp ast on current id", (fun ()-> 
            model.current_id +> Common.do_option (fun id -> 
              let ast = model.db.Db.defs.Db.toplevels#assoc id in
              let s = Sexp_ast_php.string_of_toplevel ast in
              ignore(G.dialog_text_large ~text:s ~title:"ast");
            );
          ));
          `I ("Show type information at point", (fun () -> 
            !Controller.type_info_at_point();
          ));
          `S;
          `I ("Stat parsing", (fun () -> 
            show_stat Database_php_statistics.parsing_stat_db "stat";
          ));
          `I ("Stat typing", (fun () -> 
            show_stat Database_php_statistics.typing_stat_db "type";
          ));
          `I ("Stat callgraph", (fun () -> 
            raise Todo
            (* show_stat Database_php_statistics.callgraph_stat_db "callgraph";
            *)
          ));
          `I ("Stat extra", (fun () -> 
            raise Todo
              (* show_stat Database_php_statistics.extra_stat_db "extra"; *)
          ));
          `S;
          `I ("All Stat", (fun () -> 
            show_stat Database_php_statistics.all_stat_db "all";
          ));
          `S;
          `S;
          `I ("function pointer call in ast", (fun () -> 
            raise Todo
            (*

            model.current_id +> Common.do_option (fun id -> 
              let ast = model.db.Database_c.objects#assoc id in
              let res = 
                Aliasing_function.function_pointer_call_in_ast ast 
              in
              let str = spf "nb candidates: %d" (List.length res) in
              ignore(G.dialog_text_large ~text:str ~title:"res");
            )
            *)

          ));
          `I ("function pointed candidate in ast", (fun () -> 
            raise Todo
            (*

            model.current_id +> Common.do_option (fun id -> 
              let ast = model.db.Database_c.objects#assoc id in
              let res = 
                Aliasing_function.function_pointed_candidate_in_ast ast 
              in
              let str = spf "nb candidates: %d" (List.length res) in
              ignore(G.dialog_text_large ~text:str ~title:"res");
            )
            *)
              ));

        ]
      );

      factory#add_submenu "_Help" +> (fun menu -> 
        let fc = new GMenu.factory menu ~accel_group in

        fc#add_item "_Legend" ~key:GKey._L ~callback:(fun () -> 
            G.dialog_text_large 
              Highlight_code.legend_color_codes
              "Legend" +> ignore;
        );

        fc#add_item "_Help on Pfff" ~key:GKey._H ~callback:(fun () -> 
            G.dialog_text "Read\nthe\nsource\n\ndude" "Help"
        );
        fc#add_separator ();
        fc#add_item "About" ~callback:(fun () -> 
            G.dialog_text "Brought to you by padator\nwith love" "About"
        );
      );
    ));
    
    (*-------------------------------------------------------------------*)
    (* toolbar *)
    (*-------------------------------------------------------------------*)
    vbox#pack (G.mk (GButton.toolbar) (fun tb ->

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
      ) ();
      tb#insert_space ();
      
      (*---------------------------------------------------------------*)
      (* request and pwd *)
      (*---------------------------------------------------------------*)
      (*XXX tb#insert_widget (View_widgets.mk_request model);*)
      tb#insert_space ();

      tb#insert_widget (G.mk (GButton.button ~stock:`MEDIA_PREVIOUS) (fun b -> 
        b#connect#clicked ~callback:(fun () -> 
          model.Model.playlist <- 
            Common.pop_undo model.Model.playlist;
          let top = Common.top_undo model.Model.playlist in
          let id = ES.first_id_in_tree top in
          !Controller._refresh_playlist top model;
          Controller.refresh_source_and_id (Some id) model;
          ()
        );
      ));
      tb#insert_widget (G.mk (GButton.button ~stock:`MEDIA_NEXT) (fun b -> 
        b#connect#clicked ~callback:(fun () -> 
          model.Model.playlist <- 
            Common.redo_undo model.Model.playlist;
          let top = Common.top_undo model.Model.playlist in
          let id = ES.first_id_in_tree top in
          !Controller._refresh_playlist top model;
          Controller.refresh_source_and_id (Some id) model;
        );
      ));

    ));


    (*-------------------------------------------------------------------*)
    (* main view *)
    (*-------------------------------------------------------------------*)
    vbox#pack ~expand:true ~fill:true (G.mk (GPack.paned `HORIZONTAL)(fun hp ->
    (*
       | hp#add1(pl) |                   hp#add2 (hpb)                        |
       |             |   hpb#add1 (vp1)                   |  hpb#add2 (pl2  |
       |             |     vp1#add1 (vp1_1)
       |             |      vp1_1#add1 (hbox)
       |             |      vp1_1#add2 (vp1_1b)
       |             |      vp1_1b#add2 (source)
       |             |      vp1_1b#add2 (objects)
                     
    *)

      (*---------------------------------------------------------------*)
      (* Playlist *)
      (*---------------------------------------------------------------*)
      hp#add1 (View_widgets_entities.mk_playlist_tree model);

      (*---------------------------------------------------------------*)
      (* Other *)
      (*---------------------------------------------------------------*)
      hp#add2  (G.mk (GPack.paned `HORIZONTAL) (fun hpb -> 

        hpb#add1 (G.mk (GPack.paned `VERTICAL) (fun vp1 -> 

          (*----------------------------------------------------*)
          (* Selection Columns *)
          (*----------------------------------------------------*)
          vp1#add1
            (G.mk (GPack.hbox ~height:250 ~border_width:4 ~spacing:1) (fun hbox -> 
              (* hbox#pack ~fill:true ~expand:true
                 (mk_generic_column model);
              *)

              ()
                (*
                  hbox#pack 
                  (mk_column ~width:250 model
                  "place:" (model.col1_name, model.col1));
                  hbox#pack 
                    (mk_column ~width:300 model
                    "dir1:"
                    (model.col2_name, model.col2));
                    hbox#pack 
                    (mk_column ~width:300 model
                    "time_relative_around:"
                    (model.col3_name, model.col3));
                    hbox#pack 
                    (mk_column ~width:300 model
                    "dir1:"
                    (model.col4_name, model.col4));
                  *)
            ));

          vp1#add2  (G.mk (GPack.paned `VERTICAL) (fun vp1b -> 

            (*----------------------------------------------------*)
            (* The source file *)
            (*----------------------------------------------------*)
            vp1b#add1 (View_widgets_source.mk_source_view 
                          ~path_xpm
                          ~statusbar_addtext
                          ~height:700 
                          model);
          
            (*----------------------------------------------------*)
            (* Objects *)
            (*----------------------------------------------------*)
(*            vp1b#add2 (View_widgets.mk_info_view ~height:150 model);
*)

            vp1b#set_position 700;
         
          ));
          vp1#set_position 20;
        ));

(*        hpb#add2 (View_widgets_annotations.mk_annotation_browser model);
*)
        hpb#set_position 700;

      ));
      hp#set_position 300;
    ));


    (*-------------------------------------------------------------------*)
    (* status bar *)
    (*-------------------------------------------------------------------*)
    (* the statusbar widget is defined in beginning of this file because *)
    vbox#pack (*~from: `END*) statusbar#coerce;
      
  ));

  (*-------------------------------------------------------------------*)
  (* End *)
  (*-------------------------------------------------------------------*)
  Controller._before_quit_all_func +> 
    Common.push2 Model.close_model;
  (*
  Controller._before_quit_all_func +> 
    Common.push2 View_dialogs.dialog_save_annot_before_quit;
  *)

  GtkSignal.user_handler := (fun exn -> 
    pr2 "fucking callback";
    (* Features.Backtrace.print(); *)
    (* for 3.11 *)
    let s = Printexc.get_backtrace () in
    pr2 s;
    let pb = "pb: " ^ string_of_exn exn in
    G.dialog_text ~text:pb ~title:"pb";
    raise exn
  );

  w#event#connect#key_press ~callback:(fun ev -> 
    let k = GdkEvent.Key.keyval ev in
    (match k with
(* pb intercept when enter stuff in object views
    | _ when k = Char.code 'q' -> 
        quit();
        true   
    | _ when k = Char.code 'n' -> 
        tag_expl_misc_current_object_and_go_next model;
        true
*)
    | _ -> false
    )
  );

  w#event#connect#delete    ~callback:(fun _  -> quit(); true);
  w#connect#destroy         ~callback:(fun () -> quit(); );
  w#show ();

  (* test *)
  test_mode +> Common.do_option (fun s -> View_test.do_command s model);
 
  (*GMain.Main.main ();*)
  (*XXX GtkThread.main ();*)
  GMain.Main.main ();
  ()

