(*s: completion2.ml *)
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
open Common

module G = Gui
module E = Entity_code
module Db = Database_code
module BG = Big_grep
module Flag = Flag_visual
(* to optimize the completion by using a specialized fast ocaml-based model *)
open Custom_list_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Gtk is quite "fragile". You change what looks to be an innoncent line
 * and then suddenly your performance goes down or you get some
 * gtk warnings at runtime. So take care when changing this file.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*
let is_prefix2 s1 s2 =
  (String.length s1 <= String.length s2) && 
  (String.sub s2 0 (String.length s1) = s1)
let is_prefix a b = 
  Common.profile_code "Completion.is_prefix" (fun () -> is_prefix2 a b)
*)

(*****************************************************************************)
(*  *)
(*****************************************************************************)

(*s: build_completion_defs_index *)
(* I was previously using a prefix-clustering optimisation but it
 * does not allow suffix search. Moreover it was still slow so
 * big_grep is just simpler and better.
 *)

let build_completion_defs_index all_entities = 
  (* todo? compute stuff in background ?
   *  Thread.create (fun () -> 
   * while(true) do 
   * Thread.delay 4.0;
   * pr2 "thread1";
   * done
   * ) ();
   *)
  BG.build_index all_entities
(*e: build_completion_defs_index *)


(*****************************************************************************)
(* Model *)
(*****************************************************************************)

let icon_of_kind kind has_test =
  match kind with
  | E.Function -> 
      if has_test then `YES else `NO
  
  (* todo: do different symbols for unit tested class and methods ? 
   * or add another column in completion popup
   * todo? class vs interface ?
   *)
  | E.Class -> `CONNECT
  | E.Module -> `DISCONNECT
  | E.Package -> `DIRECTORY
  | E.Type -> `PROPERTIES
  | E.Constant -> `CONNECT
  | E.Global -> `MEDIA_RECORD
  | E.Method -> `CONVERT

  | E.File -> `FILE
  | E.Dir -> `DIRECTORY
  | E.MultiDirs -> `QUIT

  (* todo *)
  | E.ClassConstant -> `CONNECT
  | E.Field -> `CONNECT
  | E.Macro -> `CONNECT
  | E.Exception -> `CONNECT
  | E.Constructor -> `CONNECT
  | E.Prototype -> `CONNECT
  | E.GlobalExtern -> `CONNECT

  | (E.TopStmts | E.Other _ ) -> raise Todo


module L=struct
  type t = { 
    mutable entity: Database_code.entity;
    mutable text: string; 
    mutable file: string; 
    mutable count: string;
    mutable kind: string;
    mutable icon: GtkStock.id;
  }

  (** The columns in our custom model *)
  let column_list = new GTree.column_list ;;
  let col_full = (column_list#add Gobject.Data.caml: t GTree.column);;

  let col_text = column_list#add Gobject.Data.string;;
  let col_file = column_list#add Gobject.Data.string;;
  let col_count = column_list#add Gobject.Data.string;;
  let _col_kind = column_list#add Gobject.Data.string;;
  let col_icon = column_list#add GtkStock.conv;;

  let custom_value _ t ~column = 
    match column with
    | 0 -> (* col_full *) `CAML (Obj.repr t)

    | 1 -> (* col_text *) `STRING (Some t.text)
    | 2 -> (* col_file *) `STRING (Some t.file)
    | 3 -> (* col_count *) `STRING (Some t.count)
    | 4 -> (* col_kind *) `STRING (Some t.kind)

    (* pad: big hack, they use STRING to present stockid in gtkStock.ml *) 
    | 5 -> (* col_icon *) `STRING (Some (GtkStock.convert_id t.icon))
    | _ -> assert false

end

module MODEL=MAKE(L)

let model_of_list_pair_string_with_icon2 _query xs =

  let custom_list = MODEL.custom_list () in

  pr2 (spf "Size of model = %d" (List.length xs));
  xs +> List.iter (fun e ->
    let kind = e.Db.e_kind in
    
    let has_unit_test =
      List.length e.Db.e_good_examples_of_use >= 1
    in
    let name = e.Db.e_name in
    (* if the string is too long, we will not see the other properties *)
    let final_name = 
      try (String.sub name 0 30) ^ "..."
      with Invalid_argument _ -> name
    in
    custom_list#insert {L. 
      entity = e;                     

      (* had originally an ugly hack where we would artificially create
       * a text2 field with always set to query. Indeed
       * gtk seems to be confused if the column referenced
       * by set_text_column contains a string that is not matching
       * the current query. So here we were building this fake text entry.
       * In fact as explained on the pygtk entry of entry_completion
       * you don't have to use set_text_column if you provide
       * your own set_match_func, which we do.
       * Maybe we should just not use Entrycompletion at all and build
       * our own popup.
       *)
      text = final_name;


      file = e.Db.e_file;
      count = i_to_s (e.Db.e_number_external_users);
      kind = E.string_of_entity_kind kind;
      icon = icon_of_kind kind has_unit_test;
    };
  );
  (custom_list :> GTree.model)


let model_of_list_pair_string_with_icon query a =
  Common.profile_code "Completion2.model_of_list" (fun () ->
    model_of_list_pair_string_with_icon2 query a
  )

let model_col_of_prefix prefix_or_suffix idx =
  let xs = 
    BG.top_n_search 
      ~top_n:!Flag.top_n
      ~query:prefix_or_suffix idx
  in
  model_of_list_pair_string_with_icon prefix_or_suffix xs

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let add_renderer (completion : GEdit.entry_completion) =

  let renderer = 
    GTree.cell_renderer_pixbuf [ `STOCK_SIZE `BUTTON ] in
  completion#pack (renderer :> GTree.cell_renderer);
  completion#add_attribute (renderer :> GTree.cell_renderer) 
    "stock_id" L.col_icon;

  let renderer = GTree.cell_renderer_text [] in
  completion#pack (renderer :> GTree.cell_renderer);
  completion#add_attribute (renderer :> GTree.cell_renderer) 
    "text" L.col_text;

  let renderer = GTree.cell_renderer_text [] in
  completion#pack (renderer :> GTree.cell_renderer);
  completion#add_attribute (renderer :> GTree.cell_renderer) 
    "text" L.col_count;

  let renderer = GTree.cell_renderer_text [] in
  completion#pack (renderer :> GTree.cell_renderer);
  completion#add_attribute (renderer :> GTree.cell_renderer) 
    "text" L.col_file;

  (* can omit this:
   *    completion#set_text_column L.col_text2; 
   * 
   * but then must define a set_match_func otherwise will never
   * see a popup
   *)
  ()

let fake_entity = {Database_code.
     e_name = "foobar";
     e_fullname = "";
     e_file = "foo.php";
     e_kind = E.Function;
     e_pos = { Common2.l = -1; c = -1 };
     e_number_external_users = 0;
     e_good_examples_of_use = [];
     e_properties = [];
}

let my_entry_completion_eff2 ~callback_selected ~callback_changed fn_idx = 
  
  let entry = GEdit.entry ~width:500 () in
  let completion = GEdit.entry_completion () in
  entry#set_completion completion;

  let xs = [ fake_entity ] in
  let model_dumb = model_of_list_pair_string_with_icon "foo" xs in
  let model = ref (model_dumb) in

  add_renderer completion;
  completion#set_model (!model :> GTree.model);

  (* we don't use the builtin gtk completion mechanism as we
   * recompute the model each time using big_grep so where
   * we just always return true. Moreover the builtin gtk
   * function would do a is_prefix check between the row
   * and the current query which in our case would fail when
   * we use the suffix-search ability of big_grep.
   *)
  completion#set_match_func (fun _key _row ->
    true
  );
  completion#set_minimum_key_length 2;

  completion#connect#match_selected (fun model_filter row ->
     (* note: the code below does not work; the row is relative to the
      * model_filter.
      * let str = !model#get ~row ~column:col1 in
      * let file = !model#get ~row ~column:col2 in
      *)

      let str = 
        model_filter#child_model#get 
          ~row:(model_filter#convert_iter_to_child_iter row)
          ~column:L.col_text
      in
      let file = 
        model_filter#child_model#get 
          ~row:(model_filter#convert_iter_to_child_iter row)
          ~column:L.col_file
      in
      let t = 
        model_filter#child_model#get 
          ~row:(model_filter#convert_iter_to_child_iter row)
          ~column:L.col_full
      in
      callback_selected entry str file t.L.entity
  ) +> ignore;

  let current_timeout = ref None in

  entry#connect#changed (fun () -> 
    let s = entry#text in
    pr2 s;
    if s <> "" then begin
      !current_timeout +> Common.do_option (fun x ->
        GMain.Timeout.remove x;
      );
      current_timeout :=
        Some 
          (G.gmain_timeout_add ~ms:250
           ~callback:(fun _ -> 
            pr2 "changing model";
            let idx = fn_idx () in
            model := model_col_of_prefix s idx;
            completion#set_model (!model :> GTree.model);

            callback_changed s;
            false
          ));
    end
    else callback_changed s
  ) +> ignore;

  (* return the entry so someone can hook another signal *)
  entry
 
let my_entry_completion_eff ~callback_selected ~callback_changed x = 
  my_entry_completion_eff2 ~callback_selected ~callback_changed x


(*e: completion2.ml *)
