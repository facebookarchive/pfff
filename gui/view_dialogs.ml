open Common

module G = Gui

open Model (* for field access *)
open Database_php

module Db = Database_php
module DbQ = Database_php_query
module ES = Entities_php

module CG = Callgraph_php

(*
module A = Annotation
module AnDb = Annotations_database 
*)

(*****************************************************************************)
(* Common stuff *)
(*****************************************************************************)

(*****************************************************************************)
(* Interactions *)
(*****************************************************************************)

let dialog_open_file model = 

(* obsolete code ?
  let candidates = model.db.Db.file_to_ids#keys in
  let candidates = candidates +> List.map (fun s -> 
    Common.filename_without_leading_path 
      (Db.path_of_project model.db.Db.project) s)
  in

  let xmodel = G.model_of_list Gobject.Data.string candidates in
  let entry = 
    G.entry_with_completion_eff ~text:"" ~model_col:xmodel
      (*~minimum_key_length:3 *) ()
  in

  let res = 
    G.dialog_ask_generic ~title:"" ~width:400
      (fun vbox -> 
        vbox#pack (G.with_label "entity:" entry#coerce);
      )
      (fun () -> 
        let text = entry#text in 
        pr2 text;
        text
      )
  in
*)

  let res = G.dialog_ask_filename ~title:"" 
    ~filename:((Db.path_of_project model.db.Db.project) ^ "/")
  in

  res +> Common.do_option (fun s -> 
    let ids = model.db.Db.file_to_topids#assoc s in
    let tree = ES.tree_of_ids ~sort:false ids 
      (fun id -> Db.name_of_id id model.db) in
    Controller.refresh_playlist tree model;

    Controller.refresh_source_and_id (Some (List.hd ids)) model;

  )
         
(*****************************************************************************)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
let dialog_search_def model = 
  let entry = View_helpers.my_entry_completion_eff model in

  let res =
    G.dialog_ask_generic ~title:"" 
      (fun vbox -> 
        vbox#pack (G.with_label "entity:" entry#coerce);
      )
      (fun () -> 
        let text = entry#text in 
        pr2 text;
        text
      )
  in
  res +> Common.do_option (fun s -> 
    View_helpers.choose_id_of_entity_and_refresh s model);
  ()

let goto_def_entity_under_point model = 
  let str = !Controller.string_at_point () in
  pr2 str;
  View_helpers.choose_id_of_entity_and_refresh str model;
  ()




(* ------------------------------------------------------------------------- *)
let dialog_search_glimpse model = 
  let entry = View_helpers.my_entry_completion_eff model in

  let res =
    G.dialog_ask_generic ~title:"" 
      (fun vbox -> 
        vbox#pack (G.with_label "glimpse:" entry#coerce);
      )
      (fun () -> 
        let text = entry#text in 
        pr2 text;
        text
      )
  in
  res +> Common.do_option (fun s -> 

    let ids = 
      (* DbQ.glimpse_grep_get_matching_ids s model.db *)
      raise Todo
    in
    let tree = ES.tree_of_ids ids (fun id -> Db.name_of_id id model.db) in
    model.current_glimpse_query <- Some s;

    Controller.refresh_playlist tree model;
    Controller.refresh_source_and_id (Some (List.hd ids)) model;
  )


(* ------------------------------------------------------------------------- *)

let dialog_search_smpl_file ~default_file model = 
  raise Todo
(*

  let res = 
    G.dialog_ask_filename ~title:"Sample file" 
      ~filename:default_file
  in
  let isofile = Model.iso_file model in

  res +> Common.do_option (fun file -> 

    let (re, (glimpse_str,_)) = 
      Smpl.parse_cocci_for_yacfe file isofile
    in
    Pretty_print_cocci.print_minus_flag := false;
    Pretty_print_cocci.print_plus_flag := false;
    Pretty_print_cocci.print_rulename_flag := false;
    pr2 (Pretty_print_cocci.rule_elem_to_string re);


    let ids = DbQ.smpl_get_matching_ids (re, glimpse_str) model.db in
    let tree = Database.tree_of_ids ids model.db in

    model.current_smpl_query <- Some re;
    model.current_glimpse_query <- Some glimpse_str;
    
    Controller.refresh_playlist tree model;
    Controller.refresh_source_and_id (Some (List.hd ids)) model;

  )
*)


let dialog_search_smpl_inline model = 
  raise Todo
(*
  let isofile = Model.iso_file model in

  let entry = View_helpers.my_entry_completion_eff model in
  let res =
    G.dialog_ask_generic ~title:"" 
      (fun vbox -> 
        vbox#pack (G.with_label "smpl:" entry#coerce);
      )
      (fun () -> 
        let text = entry#text in 
        pr2 text;
        text
      )
  in
  res +> Common.do_option (fun s -> 

    let (re, (glimpse_str, grepxs)) = 
      Smpl.rule_elem_of_string s isofile
    in
    let ids = DbQ.smpl_get_matching_ids (re, glimpse_str) model.db in
    let tree = Database.tree_of_ids ids model.db in

    model.current_glimpse_query <- Some glimpse_str;
    model.current_smpl_query <- Some re;

    Controller.refresh_playlist tree model;
    Controller.refresh_source_and_id (Some (List.hd ids)) model;

  )
*)



let dialog_search_smpl_double ~default_file model  = 
  raise Todo
(*
  let isofile = Model.iso_file model in

  let res = 
    G.dialog_ask_filename ~title:"SmPL file with 2 patterns" 
      ~filename:default_file
  in
  res +> Common.do_option (fun sgrepfile -> 
    let ((re1,re2), (glimpse_str,_)) = 
      Smpl.parse_cocci_for_yacfe_two_rule_elem sgrepfile isofile
    in

    let ids = DbQ.smpl_get_matching_ids (re1, glimpse_str) model.db in
    let tree = Database.tree_of_ids ids model.db in

    model.current_smpl_query_protocol <- Some (re1,re2);

    Controller.refresh_playlist tree model;
    Controller.refresh_source_and_id (Some (List.hd ids)) model;
  )
*)


(* ------------------------------------------------------------------------- *)
let dialog_search_words_in_comment model = 
  raise Todo
(*
  let entry = View_helpers.my_entry_completion_eff model in

  let res =
    G.dialog_ask_generic ~title:"" 
      (fun vbox -> 
        vbox#pack (G.with_label "words:" entry#coerce);
      )
      (fun () -> 
        let text = entry#text in 
        pr2 text;
        text
      )
  in
  res +> Common.do_option (fun s -> 
    let xs = Common.words s in
    let xs' = xs +> List.map Word.stem_multi_heuristics in
    pr2_gen xs';

    let ids_xs = xs' +> List.map (fun s -> 
      try 
        let v = model.db.Db.word_to_ids#find s in
        match v with
        | Right (Error s) -> failwith s
        | Left (ids, cnt) -> ids
      with Not_found -> []
    ) in
    let ids_inter = ids_xs +> Common.foldl1 Common.inter_set in

    let ids = ids_inter in
   
    let tree = Database.tree_of_ids ids model.db in
    model.current_words_query <- Some xs';

    Controller.refresh_playlist tree model;
    Controller.refresh_source_and_id (Some (List.hd ids)) model;
      

  )
*)  

(*****************************************************************************)
(*****************************************************************************)
(* ------------------------------------------------------------------------- *)
let dialog_callers model = 

  model.current_id +> Common.do_option (fun id -> 
    let _kind = model.db.defs.id_kind#assoc id in

    let ids = 
      Db.callers_of_id id model.db +> List.map CG.id_of_callerinfo
    in

    let newid = View_helpers.dialog_choose_id model ids in
    Controller.refresh_source_and_id (Some newid) model;
  )

let dialog_callees model = 
  model.current_id +> Common.do_option (fun id -> 

    let ids = Db.callees_of_id id model.db +> List.map CG.id_of_callsite in

    let newid = View_helpers.dialog_choose_id model ids in
    Controller.refresh_source_and_id (Some newid) model;
  )

(* ------------------------------------------------------------------------- *)
let build_callers model = 
  model.current_id +> Common.do_option (fun id -> 

    let tree = 
      DbQ.calltree_callers_of_f 
        ~depth:model.depth_caller ~preferences:model.calltree_prefs
        id model.db 
    in
    Controller.refresh_playlist tree model;
  )

let build_callees model = 
  model.current_id +> Common.do_option (fun id -> 
   
    let tree = 
      DbQ.calltree_callees_of_f 
        ~depth:model.depth_callee ~preferences:model.calltree_prefs
        id model.db 
    in
    Controller.refresh_playlist tree model;
  )


let build_user model = 
  raise Todo
(*

  model.current_id +> Common.do_option (fun id -> 
    let strglob = model.db.Db.names#assoc id +> Db.string_of_id_string in

    let ids = model.db.Db.user_of_global#assoc strglob in
    let tree = Database.tree_of_ids ids model.db in
    
    Controller.refresh_playlist tree model;
  )
*)


let build_use_xxx model = 
  raise Todo
(*
  let main_props = ["global";"struct";"field";"typedef"] in

  let entry = View_helpers.my_entry_completion_eff model in
  let combo = GEdit.combo ~popdown_strings:main_props () in

  let res =
    G.dialog_ask_generic ~title:"" 
      (fun vbox -> 
        vbox#pack combo#coerce;
        vbox#pack (G.with_label "entity:" entry#coerce);
      )
      (fun () -> 
        let text = entry#text in 
        let categ = combo#entry#text in
        pr2 (spf "chosen: %s, in categ %s" text categ);
        text, categ
      )
  in
  res +> Common.do_option (fun (s, kind) -> 
    let ids = 
      match kind with 
      | "global" -> model.db.Db.user_of_global#assoc s
      | "struct" -> model.db.Db.user_of_struct#assoc s
      | "field" -> model.db.Db.user_of_field#assoc s
      | "typedef" -> model.db.Db.user_of_typedef#assoc s
      | _ -> raise Impossible
    in
    let tree = Database.tree_of_ids ids model.db in

    (* or be more precise ? *)
    model.current_glimpse_query <- Some s;

    Controller.refresh_playlist tree model;
  ()
  )
*)

(*****************************************************************************)
(*****************************************************************************)

(* note: 20 jours apres my first -test_query_db, can now do same but via gui :)
 * mais entre temps j'ai rajoute le gui, smpl (and so glimpse), 
 * generalize code (calltree, etc), editor, type info, position info, etc.
 *)

let dialog_check_protocol ~default_file model  = 
  raise Todo
(*
  let isofile = Model.iso_file model in

  let res = 
    G.dialog_ask_filename ~title:"SmPL protocol file" 
      ~filename:default_file
  in
  res +> Common.do_option (fun sgrepfile -> 
    let ((re1,re2), (glimpse_str,_)) = 
      Smpl.parse_cocci_for_yacfe_two_rule_elem sgrepfile isofile
    in
    model.current_smpl_query_protocol <- Some (re1,re2);
    let tree = Common.top_undo model.playlist in

    let Common.NodeRef (_n, aref) = tree in

    Checking_c.check_upward_callers_tree 
      ~ast_of_id:(fun id -> model.db.Db.objects#assoc id)
      ~id_is_ok:(fun id -> false)
      !aref (re1, re2);
    
    Controller.refresh_playlist tree model;
  )
*)

(* ------------------------------------------------------------------------- *)
(* note: 2 months after builder caller and dialog_check_protocol, 
 * I can now do the same more conveniently via the gui by browsing 
 * annotations :) un peu lent :)
 * mais entre temps j'ai codé de meilleurs analyses, j'ai du optimiser du
 * coup la creation de la db, j'ai ajouté le rep annotations/, refactoriser
 * le code, etc. Also because job hunting less time to work on it ...
 * 
 * But still, took me more time that I expected ...
 *)

let check_all_annotations model = 
  raise Todo
(*
  let annots = model.annot_info.Annotations_list.annots in

  let ainfo = Model.annot_checking_info model in

  annots +> List.iter (fun annot -> 
    try 
      let (result, id, tree, (re_pos, re_neg)) = 
        Annotation_checking.check_annot ainfo annot model.db
      in
      annot.Annotation.result <- Some result;
    with exn -> 
      pr2 (spf "pb handling annot %s, exn = %s"
              (A.str_of_fullannot annot)
              (Common.string_of_exn exn))
  );
  Controller.refresh_annotlist annots model;
  ()
*)

(*****************************************************************************)
(*****************************************************************************)
(* ------------------------------------------------------------------------- *)
let dialog_import_annot ~default_file  model = 
  raise Todo
(*
  let res = 
    G.dialog_ask_filename ~title:"Annotation file" 
      ~filename:default_file
  in
  (match res with
  | None ->      model.annot_info <- Annotations_list.empty_annotations_info;
  | Some file -> model.annot_info <- Annotations_list.load_annotations_info file;
  );
  Controller.refresh_all model
*)
        

let dialog_save_annot model = 
  raise Todo
(*
  let annot = model.annot_info in
  if annot.Annotations_list.filename <> ""
  then Annotations_list.save_info annot
  else G.dialog_text "no annotation file opened" "error"
*)

let dialog_save_annot_before_quit model = 
  raise Todo
(*
  let annot = model.annot_info in
  if annot.Annotations_list.filename <> "" && 
     annot.Annotations_list.was_modified
  then 
    if G.dialog_ask_y_or_no
      ~text:"Your Annotation was modified. Save the file ?"
      ~title:"Save Annotations"
    then Annotations_list.save_info annot
    else ()
*)


(* ------------------------------------------------------------------------- *)

(* if you don't like the default, you can still man edit the annotation file *)
let add_annotation model =
  raise Todo
(*
  model.current_id +> Common.do_option (fun id -> 

    let xinfo = model.annot_info in

    let fullid = Model.fullid_of_id id model in
    let name = Model.string_of_id id model in

    let new_annot = { 
      Annotation.fullid = fullid;
      entity = 
        A.AnnotatedFunction name;
      origin = 
        A.DerivedFromComment;
      note = A.Note "via add_annotation";
      
      annotation = A.ContextInterrupt A.Disabled;

      kind = A.Ask;

      result = None;
    }
    in
    Annotations_list.append_annot new_annot xinfo;
    Controller.refresh_annotlist xinfo.Annotations_list.annots model;
  )
*)
 


