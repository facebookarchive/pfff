open Common

module G = Gui

(* for field access *)
open Model 

module E = Entity_php
module ES = Entities_php

(*****************************************************************************)
(* Helpers (sometimes using global) *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Completion optimisation (version 2) *)
(* ------------------------------------------------------------------------- *)
(* opti: to scale with 'www'. Need cache a few things.
 * but the pb is not the db, but gtk which is slow to build the model,
 * hence this cache.
 *)

let num_prefix = ref 4
let prefix_of_string s = 
  Common.lowercase (Common.take_string_safe !num_prefix s)
  
let _cache_entry2 = 
  ref (None: ((string * (string list * 'a option ref)) list) option)

let start_build_completion_defs model = 

  (* todo? compute stuff in background ?
  Thread.create (fun () -> 
    while(true) do 
      Thread.delay 4.0;
      pr2 "thread1";
    done
  ) ();
  *)

  Common.cache_in_ref _cache_entry2 (fun () -> 
   Common.profile_code2 "cache_entry2" (fun () -> 
    let all_funcs = 
      model.db.Db.defs.Db.name_defs#keys in
    if List.length all_funcs <= 10000
    then num_prefix := 2;

    all_funcs 
    +> Common.map_eff_rev (fun s -> prefix_of_string s, s)
    +> Common.group_assoc_bykey_eff
    +> Common.map_eff_rev (fun (k,v) -> (k,(v, ref None)))
    )
  )

let model_col_of_prefix prefix model = 
  match !_cache_entry2 with
  | None -> failwith "call start_build_completion_defs first"
  | Some xxs -> 
      try 
        let (xs, cacheref) = List.assoc prefix xxs in
        Common.cache_in_ref cacheref (fun () -> 
          G.model_of_list Gobject.Data.string xs
        )
      with Not_found -> 
        G.model_of_list Gobject.Data.string []


let my_entry_completion_eff2 bigmodel = 
  
  let entry = GEdit.entry ~text:"" () in

  let (model, col) = 
    G.model_of_list Gobject.Data.string ["foo"]  in
  let (_model2, _col2) = 
    G.model_of_list Gobject.Data.string ["abca"; "abda";"abcb";"abza"] in

  let c = GEdit.entry_completion ~model ~entry () in
  c#set_text_column col;
  entry#set_completion c;

  let _current_prefix = ref "" in

  entry#connect#changed (fun () -> 
    let s = entry#text in
    pr2 s;
    if String.length s >= !num_prefix && 
      (  !_current_prefix = "" 
      || not (Common.is_string_prefix !_current_prefix s)
      )
    then begin
      pr2 "changing model";
      let prefix = prefix_of_string s in
      _current_prefix := prefix;

      let (model, col) = 
        model_col_of_prefix prefix model in

      c#set_model (model :> GTree.model);
      c#set_text_column col;
      
    end
  );
  entry
 
let my_entry_completion_eff x = my_entry_completion_eff2 x

(* ------------------------------------------------------------------------- *)
(* SmPL *) 
(* ------------------------------------------------------------------------- *)
(*
let gen_pattern_func s = 
  Smpl.rule_elem_of_string (spf " %s(...)" s) None +> Common.fst
*)


(* ------------------------------------------------------------------------- *)
let dialog_choose_id model xs = 
  match xs with
  | [] -> failwith "dialog_choose_id: empty list"
  | [x] -> x
  | xs -> 
      let idx = ref 0 in
      
      let widget = 
        G.with_viewport (G.mk (GList.clist ~width:500 ~height:600
                                  ~titles:["entity";"kind";"file";"line";])
         (fun l -> 
           xs +> List.map (fun id -> 

             let fullid = model.db.Db.fullid_of_id#assoc id in

             let name, strkind = 
               try 
                 let kind = model.db.Db.defs.Db.id_kind#assoc id in
                 let name = Db.name_of_id id model.db in
                 let strkind = E.string_of_id_kind kind
                 in
                 name, strkind
               with Not_found ->
                 "NONAME", "NOKIND"
             in

             name,
             strkind,
             fullid.E.file, i_to_s fullid.E.line
           ) +> List.iter (fun (x1,x2,x3,x4) -> 
             l#append [x1;x2;x3;x4] +> ignore
           );

           l#columns_autosize (); (* ??? *)
           
           l#connect#select_row ~callback:(fun ~row ~column ~event -> 
             idx := row;
           );
         ))
      in
     
      let res = 
        G.dialog_ask_generic ~title:"" ~width:500
          (fun vbox -> 
            vbox#add widget;
          )
        (fun () -> 
          !idx
        )
      in
      match res with
      | Some n -> List.nth xs n
      | None -> failwith "no choice"




let choose_id_of_entity_and_refresh s model =

  let ids = Model.ids_of_string s model in
  
  let id = dialog_choose_id model ids in

  let tree = ES.tree_of_ids [id] (fun id -> Db.name_of_id id model.db) in
  Controller.refresh_playlist tree model;
  Controller.refresh_source_and_id (Some id) model;
  ()
