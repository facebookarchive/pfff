
open Common

module G = Gui

open Model (* for field access *)

module CG = Callgraph_php
module Db = Database_php
module DbQ = Database_php_query



(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* The main widget  *)
(*****************************************************************************)

let mk_playlist_tree model =

  (* ----------------- *)
  (* the model *)
  (* ----------------- *)
  let cols = new GTree.column_list in

  let c_name = cols#add Gobject.Data.string in
  let c_depth = cols#add Gobject.Data.int in

  let c_id = cols#add Gobject.Data.caml in

  let c_tooltip = cols#add Gobject.Data.string in
  let c_color = cols#add Gobject.Data.string in
  let c_count = cols#add Gobject.Data.int in
  let c_score = cols#add Gobject.Data.int in
  let c_extra = cols#add Gobject.Data.string in

  let c_icon = cols#add GtkStock.conv in
  let c_icon2 = cols#add GtkStock.conv in


  let store = GTree.tree_store cols in

  let fill_store_row (store: GTree.tree_store) iter depth
      (name, id, count, tooltip, icon, icon2, score, extrastr, color) =
    store#set ~row:iter ~column:c_name name;
    store#set ~row:iter ~column:c_depth depth;

    store#set ~row:iter ~column:c_color color;
    store#set ~row:iter ~column:c_tooltip tooltip;
    store#set ~row:iter ~column:c_count count;
    store#set ~row:iter ~column:c_id id;
    store#set ~row:iter ~column:c_score score;
    store#set ~row:iter ~column:c_extra extrastr;

    store#set ~row:iter ~column:c_icon icon;
    store#set ~row:iter ~column:c_icon2 icon2;
  in

  let rec fill_store_tree 
      (store:GTree.tree_store) depth parent 
      (*annots_of_id*)
      (tree: CG.idtree) 
      =

    match tree with
    | NodeRef (node, xs) -> 
        let title = node.CG.name in
        let id = node.CG.id in

        let score = 
          (*(*CG.int_of_analysis_confidence*) node.CG.confidence *)
          1
        in

        let tooltip = "" in
        let count = 0 in

        (* why use icon ? because need gui support for good feedback, 
         * to know what is lacking, what is done. Partial match of 
         * coccinelle are not enough and not very practical to use.
         * 
         * Has Henry done something good for his gui ?
         * Anyway it is not interprocedural and I guess lack
         * the lxr and cscope coolness.
         *)
        let icon = 
          `OK
          (*
          match node.CG.match_result with
          | None -> `FILE (* default, DISCARD not in lablgtk :( *)

          | Some (Lib_analyze.MatchPos _) -> `YES
          | Some (Lib_analyze.MatchNeg _) -> `NO

          | Some (Lib_analyze.NoMatch) -> `QUIT
          | Some (Lib_analyze.MatchProblem _) -> `CLOSE 

          | Some (Lib_analyze.MatchPosCosAnnot) -> `ABOUT

          | Some (Lib_analyze.MatchPosChildren) -> `OK
          | Some (Lib_analyze.MatchNegChildren) -> `CANCEL
          *)
        in 

        let icon2 = `FILE
          (*
          let annots = annots_of_id id in
          if (null annots)
          then `FILE
          else `ABOUT
          *)
        in

        let extrastr = 
          (* Db.string_of_extra_id_info_db id model.db *)
          ""
        in
        let color = 
          (*
          if node.CG.gray
          then "Black" 
          else 
          *)
            List.nth G.mapping_color depth
        in

        let iter = 
          if depth = 0 
          then parent
          else store#append ~parent:parent () 
        in 
        fill_store_row store iter depth 
          (title, id, count, tooltip, icon, icon2, score, extrastr, color);
        !xs +> List.iter (fill_store_tree store (depth+1) iter (*annots_of_id*));
  in

  (* ----------------- *)
  (* the view *)
  (* ----------------- *)
  (G.with_viewport(G.mk (GTree.view ~model:store) (fun view ->

    let col0 = GTree.view_column ~title:"#"
      ~renderer:(GTree.cell_renderer_text [], ["text", c_count]) () in
    view#append_column col0 +> ignore;

    let col0bisbis = GTree.view_column ~title:"A"
      ~renderer:(GTree.cell_renderer_pixbuf [`STOCK_SIZE `BUTTON], ["stock_id", c_icon2]) () in
    view#append_column col0bisbis +> ignore;

    let col0bis = GTree.view_column ~title:"R"
      ~renderer:(GTree.cell_renderer_pixbuf [`STOCK_SIZE `BUTTON], ["stock_id", c_icon]) () in
    view#append_column col0bis +> ignore;


    let col1 = G.view_column ~title:"Prop"
      ~renderer:(GTree.cell_renderer_text [], 
                ["text", c_name;
                 "foreground",c_color;
                 (*"tooltip-markup", c_tooltip;*)
                ]) () in
    view#append_column col1 +> ignore;

    let col2 = GTree.view_column ~title:"score"
      ~renderer:(GTree.cell_renderer_text [], ["text", c_score]) () in
    view#append_column col2 +> ignore;

    let col3 = GTree.view_column ~title:"extra"
      ~renderer:(GTree.cell_renderer_text [], ["text", c_extra]) () in
    view#append_column col3 +> ignore;

    (* otherwise col0 is the default expander *)
    view#set_expander_column (Some col1);
    view#selection#set_mode `BROWSE;
   

    (* ----------------- *)
    (* the controller *)
    (* ----------------- *)
    let refresh_taxo_list ?(force_refresh=false) tree (model: model) = 
      pr2 "refresh_taxo_list";
      view#selection#unselect_all();
      pr2 "refresh_taxo_list2";

      (*
      let annots_of_id = 
        Annotations_database.mk_annots_of_id model.annot_info model.db in
      *)
      view +> G.model_modif (fun _store -> 
        store#clear ();
        let root_iter = store#append () in
        fill_store_tree store 0 root_iter (*annots_of_id*) tree;
      );
      !Controller.expand_level 2;
      pr2 "refresh_taxo_list3";
    
    in

    Controller._refresh_playlist := refresh_taxo_list ~force_refresh:true;

    Controller.expand_all := view#expand_all;
    Controller.expand_level := G.view_expand_level view;

    (*
    Controller._refresh_all_func +> 
      Common.push2 (refresh_taxo_list ~force_refresh:false);
    refresh_taxo_list model;
    *)


    (* double click *)
    view#connect#row_activated ~callback: (fun path column ->
      pr2 "Double click";
    );

    (* single click *)
    view#selection#connect#changed ~callback:(fun () -> 
      match view#selection#get_selected_rows with
      | [treepath] -> let row = store#get_iter treepath in

          let s = store#get ~row ~column:c_name in
          let id  = store#get ~row ~column:c_id in
          pr2 ("clicked on : "^ s);
          Controller.refresh_source_and_id (Some id) model;

      | _ -> pr2 "No selection in Callback, Impossible 256"
    );

    (* right click *)
    G.mk_right_click_menu_on_store view (fun path -> 
      [
        `I ("Info", (fun () -> let row = store#get_iter path in
                               
           let info = store#get ~row ~column:c_tooltip in
           G.dialog_text info "info"
        ));
        `I ("Add to current obj", (fun () -> 
          ()
        ));
        `I ("Expand callers", (fun () -> 
          (* maybe easier way is just to modify the global tree and
           * ask to refresh and ask to go to place where was before
           * (the id on which we clicked on expand). But I often
           * get problems with "No selection" error message so I prefer
           * for now also update the gtk store model in addition to the 
           * playlist (ocaml) model.
           *)

          (* 
          let row = store#get_iter path in
          let iter = row in 

          let id = store#get ~row ~column:c_id in
          let depth = store#get ~row ~column:c_depth in

          let tree = DbQ.calltree_callers_of_f 
            ~depth:model.depth_caller ~preferences:model.calltree_prefs
            id model.db in
          let trees = Common.treeref_children_ref tree in

          (* update tree in model.db.playlist, need first find treeref
           * inside the tree with this id *)
          let parent = model.playlist +> Common.top_undo +> Common.find_treeref 
            (fun (n,_) -> n.CG.id = id)
          in
          let aref = Common.treeref_children_ref parent in
          assert(null !aref);
          aref := !trees;


          let annots_of_id = 
            Annotations_database.mk_annots_of_id model.annot_info model.db in

          !trees +> List.iter (fill_store_tree store (depth+1) iter annots_of_id);

          *)
          ()

        ));
        `I ("Expand callee", (fun () -> 
          ()
        ));

      ]
    );
  )))
