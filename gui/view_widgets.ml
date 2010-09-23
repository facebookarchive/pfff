open Common

module G = Gui

open Model (* for field access *)

(*****************************************************************************)
(* Intermediate (big) widgets *)
(*****************************************************************************)

(* do nothing for the moment, 
 * could do glimpse request ?
 * could to search in file ? use GEdit for that ? they provide 
 * helpers to search in file.
 *)
let mk_request model =
  let do_request request = 
    model.basic_query <- request;
    Controller.refresh_all model;
  in

  G.with_label "request:" 
    (G.mk (GEdit.entry ~text:"" ~max_length: 100) (fun e -> 
      e#connect#activate ~callback:(fun () -> 
        let request = e#text in
        pr ("Entry contents:" ^ request);
        do_request request
      );
    ))

(* ------------------------------------------------------------------------- *)
(* Object Info *)
(* ------------------------------------------------------------------------- *)

(* simple text widget. Just display annotation information for the moment
 *)
let mk_info_view ?height model = 
  G.with_viewport (G.mk  (GText.view ?height ~editable:false) (fun srcview ->

    raise Todo
(*
    let buffer = srcview#buffer in
    buffer#set_text "no object selected yet";



    let refresh_info_id id model = 
      try 
        (* todo better info ? 
           if model.current_id = ...
           if model.current_annot = ... then override the one from
           current_id ?
        *)

        let annots = Model.annots_of_id id model in
        let s = 
          (Model.string_of_id id model) ^ "\n" ^
          (annots +> List.map Annotation.str_of_fullannot +> Common.join "\n") 
        in
        buffer#set_text s
      with exn -> 
        pr2 (Common.string_of_exn exn);
    in
    Controller._refresh_info_id := refresh_info_id;
*)
  ))
