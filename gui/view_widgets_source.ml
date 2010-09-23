
open Common

module G = Gui

(* for field access *)
open Model 

module Ast    = Ast_php
module Finder = Finder_php

module EC  = Entity_php
module CG  = Callgraph_php
module Db  = Database_php
module DbQ = Database_php_query
module V   = Visitor_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* GText buffer and ast related position, php specific *)
(* ------------------------------------------------------------------------- *)

let iter_start_of_info bufinfo ii = 
  let buffer = bufinfo.Source.buffer in
  let cpos = Ast_php.pos_of_info ii in
  buffer#get_iter (`OFFSET (Source.offset cpos bufinfo))

let iter_end_of_info bufinfo ii = 
  let buffer = bufinfo.Source.buffer in
  let cpos = Ast_php.pos_of_info ii in
  let len = String.length (Ast_php.str_of_info ii) in
  buffer#get_iter (`OFFSET (Source.offset (cpos + len) bufinfo))


let iter_range_of_info bufinfo ii = 
  iter_start_of_info bufinfo ii,  iter_end_of_info bufinfo ii

let iter_range_of_origin_ii bufinfo ii = 
  Lib_parsing_php.range_of_origin_ii ii 
  +> Common.fmap (Source.iter_range_of_range bufinfo)


(*
let iterline_of_info bufinfo ii = 
  let buffer = bufinfo.buffer in
  let l = Ast_c.line_of_info ii in
  buffer#get_iter_at_char ~line:(l -1) 0
*)

let apply_tag_iter_range_of_origin_ii bufinfo ii tag = 
  let buffer = bufinfo.Source.buffer in

  (* (* if want range instead of individual *)
  iter_range_of_origin_ii buffer ii +> Common.do_option (fun (pos1, pos2) -> 
    buffer#apply_tag_by_name tag pos1 pos2
  )
  *)
  ii +> List.filter Ast_php.is_origintok +> List.iter (fun ii -> 
    iter_range_of_origin_ii bufinfo [ii] +> Common.do_option (fun (pos1,pos2)-> 
    buffer#apply_tag_by_name tag pos1 pos2
    )
  )

(* shortcut *)
let apply_tag buf info tag = 
  apply_tag_iter_range_of_origin_ii buf [info] 
    (Source.stag buf.Source.buffer tag)



(*****************************************************************************)
(* Source highlithing *)
(*****************************************************************************)
let source_view_add_style_and_stuff bufinfo asts_and_toks model = 

  let prefs = 
    { Highlight_code.
      show_type_error   = model.Model.show_type_error;
      show_local_global = model.Model.show_local_global;
    } 
  in
  let id = Common.some model.current_id in
  let current_file = Db.filename_of_id id model.db in

(*  
  let annots_of_id = 
    Annotations_database.mk_annots_of_id model.annot_info model.db in
*)
  (* (id, (ast, toks)) *)

  asts_and_toks +> List.iter (fun (id, ast, toks) -> 
    Source_php.process_one_ast bufinfo (id, ast, toks) 
      prefs 
      apply_tag 
      iter_end_of_info
      model.db 
      current_file (*annots_of_id*);
  );
  ()


(*****************************************************************************)
(* Query higlighter  *)
(*****************************************************************************)

let query_add_style_and_stuff bufinfo (srcview : GText.view) idtop model = 
  raise Todo


(*****************************************************************************)
(* Semantic information query  *)
(*****************************************************************************)

(* can explore the full asts, or use default gtk feature for that ? *)
let string_at_point bufinfo model = 
  let buffer = bufinfo.Source.buffer in

  let filename = Model.current_file model in
  let asts = model.db.Db.file_to_topids#assoc filename
    +> List.map (fun id -> model.db.Db.defs.Db.toplevels#assoc id)
  in

  let (iter1,iter2) = buffer#selection_bounds in
  let bufoffset = iter1#offset in

  (* note: need now adjust because of the insert_pixbuf *)
  let offset = Source.buf_offset_to_file_offset bufoffset bufinfo in

  (* This can raise some exn on certain position, for instance
   * when are on a comment, because the ast has no such ii in it.
   * Those ii are in the tokens (or now in the comment_tag)
   *)
  let info = 
    try Finder.info_at_pos_in_full_program offset asts
    with Not_found -> 
      failwith (spf "unable to find position: %d in %s" offset filename)
  in
  Ast.str_of_info info


let type_info_at_point buffer model = 
  let filename = Model.current_file model in
  let asts = model.db.Db.file_to_topids#assoc filename
      +> List.map (fun id -> model.db.Db.defs.Db.toplevels#assoc id) 
  in
  
  (* todo use cursor, todo sure have good index cos gtk may be 
   * 0-indexed and emacs and me 1-indexed when it comes to file
   * offset.
   *)
  let (iter1,iter2) = buffer#selection_bounds in
  let offset = iter1#offset in
  
  pr2_gen offset;
  asts +> List.iter (fun ast -> 
    try 
      let expr = Finder.expr_at_pos offset ast in
      pr2_gen expr;
      Sexp_ast_php.show_expr_info := true;
      let s = Sexp_ast_php.string_of_expr expr in
      pr2 s;
    with
    | Not_found -> 
        pr2 "Not_found"
    | Multi_found -> 
        pr2 "MultiFound"
  )


(*****************************************************************************)
(* The main widget  *)
(*****************************************************************************)

(* I do not need GSourceView as have redone the langage highlighting
 * from scratch. Simply use a GText.view.
 *)

let mk_source_view ?height ?width ~statusbar_addtext ~path_xpm model = 
  G.with_viewport (G.mk  (GText.view ?height ?width ~editable:false)
  (fun srcview ->
    let buffer = srcview#buffer in

    pr2 ("SRCVIEW");

    srcview#misc#modify_base [`NORMAL, `NAME "DarkSlateGray"];

    srcview#misc#modify_font_by_name 
      "-misc-*-*-*-*-20-*-*-*-*-*-*";
    srcview#misc#modify_font_by_name 
      "-misc-fixed-bold-r-normal--20-100-100-100-c-70-iso8859-1";

    (* old: build_tags (buffer :> GText.buffer); *)

    let pixbuf = GdkPixbuf.from_file (path_xpm ^ "/gtk-logo-rgb.gif") in
    let dest = GdkPixbuf.create ~has_alpha:true ~width:20 ~height:20 () in
    GdkPixbuf.scale ~dest ~width:20 ~height:20 ~interp:`BILINEAR pixbuf;
    let pixbuf = dest in

     
    let refresh_source id model = 
      pr2 "refresh_source";

      match id with
      | None -> 
          buffer#set_text "no object selected yet"
      | Some id -> 

          let fullid = model.db.Db.fullid_of_id#assoc id in
          let (filename, line) = fullid.EC.file, fullid.EC.line in

          let text = Common.read_file filename in

          statusbar_addtext filename;
          buffer#set_text text;

          buffer#apply_tag_by_name (Source.stag buffer 
                                       Highlight_code.BackGround)
            buffer#start_iter buffer#end_iter;
          buffer#apply_tag_by_name (Source.stag buffer 
                                       Highlight_code.ForeGround)
            buffer#start_iter buffer#end_iter;
          
          let iter1 = 
            buffer#get_iter_at_char ~line:(line -1) 0 in
          let iter2 = 
            iter1#forward_lines 1 in

          buffer#place_cursor iter1;
          buffer#select_range iter1 iter2;

          let scroll_iter = iter1#forward_lines 0 (* CONFIG *) in
          let mark = buffer#create_mark scroll_iter in
          srcview#scroll_mark_onscreen (`MARK mark);


          let ids = model.db.Db.file_to_topids#assoc filename in
          let asts_and_toks = ids +> List.map (fun id -> 
            id, 
            model.db.Db.defs.Db.toplevels#assoc id,
            model.db.Db.defs.Db.tokens_of_topid#assoc id
          ) 
          in

          (* order is important *)
          let bufinfo = {
            Source.buffer = (buffer :> GText.buffer);
            (* I multiply by 2 just to give me space, because the pixbuf
             * impose to have a larger array.
             *)
            posarray_adjust = Array.create (String.length text * 2) 0;
            pixbuf = pixbuf;
          } in
            
          (* have to each time readjust this controller *)
          Controller.string_at_point := 
            (fun () -> string_at_point bufinfo model);

          source_view_add_style_and_stuff bufinfo asts_and_toks model; 

          (* TODO query_add_style_and_stuff bufinfo srcview id model; *)

          srcview#scroll_to_iter ~use_align:true ~yalign:0.5 iter1;
          ()
    in
    (* estet? instead of settings those globals here, maybe more 
     * compositional to return those controllers hooks and let
     * the caller modify the globals ? then can reuse this code
     * elsewhere ?
     *)
    Controller._refresh_source := refresh_source;

    Controller.type_info_at_point := 
      (fun () -> type_info_at_point buffer model);

    srcview#connect#move_cursor ~callback:(fun step i ~extend -> 
      pr2 "here";
    );
    srcview#connect#populate_popup ~callback:(fun menuobj -> 
      pr2 "popup";
    );

    (* less: raccourci clavier ? or double click enough ? 
     * or middle click ? read gtk ? 
     *)
    srcview#event#connect#button_press ~callback:(fun ev -> 

      (* return false means propagate event *)

      (match () with
      (* single click *)
      | _ when GdkEvent.get_type ev = `BUTTON_PRESS -> 

          (* note: the cursor info is not yet accurate so 
           * string_at_point should return the old info.
           *)
          (* put in comment because raise exn when click on comment
           * which has no ii in the ast *)
          pr2 ("single"  (*^ !Controller.string_at_point ()*));
          false 

       (* double click *)
      | _ when GdkEvent.get_type ev = `TWO_BUTTON_PRESS -> 
          pr2 ("double: " ^ !Controller.string_at_point ());

          (* note: the cursor info is here accurate, probably because
           * the default action of the single BUTTON_PRESS has already
           * had the effect of set this new cursor position info.
           *)

          (* goto def of entity. If point on definition, then it also 
           * works as it will select the id, so can they get to know
           * if this id, in the extra_info_windows, has some
           * annotations.
           *)
          let str = !Controller.string_at_point() in
          pr2 str;
          View_helpers.choose_id_of_entity_and_refresh str model;
          true

       (* triple click *)
      | _ when GdkEvent.get_type ev = `THREE_BUTTON_PRESS -> 
          pr2 ("triple: " ^ !Controller.string_at_point());
          
          false

      | _ when GdkEvent.Button.button ev = 3 && (* ?? copy paste of labltk ex *)
               GdkEvent.get_type ev = `BUTTON_PRESS -> 
          pr2 ("middle: " ^ !Controller.string_at_point());
          false 

      | _ -> 
          false
      );
    );
  ))
