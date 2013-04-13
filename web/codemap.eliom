open Common

module H = Eliom_content.Html5.D

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* App *)
(*****************************************************************************)
module App = Eliom_registration.App (struct let application_name = "app" end)

(*****************************************************************************)
(* Shared *)
(*****************************************************************************)

{shared{
let width = 1200
let height = 680
module Model = Model_codemap
}}

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)
let main_service =
  App.register_service 
    ~path:["codemap"] 
    ~get_params:(Eliom_parameter.string "path")
  (fun path () ->
    pr2 path;

    (* TODO: compute config based on path and depending
     * on some OCaml pfff repo type.
     *)
    let rects = Globals.rects in
   
(*
    let rects = Server_codemap.treemap_generator [path] in
    Common.cache_computation ~verbose:true path "_treemap.marshall" 
      (fun () ->)
    in
*)
    (* optimize and filter very small rectangles so
     * that we send less data
     *)
    let rects = rects +> Common.exclude (fun rect ->
      let r = rect.Treemap.tr_rect in
      let w = Figures.rect_width r in
      let h = Figures.rect_height r in
      w *. h <= 0.000009
    )
    in
    pr2 (spf "nb rects after filtering: %d" (List.length rects));
    let w = { Model.
       rects;

       width = width;
       height = height;
       orig_coord_width = 0.;
       orig_coord_height = 0.;
       width_text_etalon_normalized_coord = 0.;
    }
    in

    ignore
      {unit { Client_codemap.paint %w }};
    Lwt.return
      (H.html 
          (H.head (H.title (H.pcdata "Codemap")) [ 
          ])
	  (H.body [
            H.div ~a:[H.a_id "output";] [];
            H.canvas 
              ~a:[H.a_id "main_canvas"; H.a_width width; H.a_height height] [];
          ]))
  )

(*****************************************************************************)
(* Testing *)
(*****************************************************************************)

let test_codemap_micro =
  App.register_service 
    ~path:["test_micro"] 
    ~get_params:(Eliom_parameter.unit)
  (fun () () ->

    let rects = [] in
    let w = { Model.
       rects;

       width = width;
       height = height;
       orig_coord_width = 0.;
       orig_coord_height = 0.;
       width_text_etalon_normalized_coord = 0.;
    }
    in
    let file = "/home/pad/pfff/Makefile" in
    let fileinfo = { Model.
      lines = Common.cat file;
      nblines = float_of_int (Common2.nblines_eff file);
    }
    in
  (* if the file is not textual, or contain weird characters, then
   * it confuses cairo which then can confuse computation done in gtk
   * idle callbacks
   *)
  (*if Common2.lfile_exists_eff file && File_type.is_textual_file file*)

  (* Common.nblines_with_wc was really slow. fork sucks.
   * alternative: we could store the nblines of a file in the db but
   * we would need a fast absolute_to_readable then.
   *)

    ignore
      {unit{ Client_codemap.test_paint_micro %w %fileinfo }};
    Lwt.return
      (H.html 
          (H.head (H.title (H.pcdata "Micro")) [ 
          ])
	  (H.body [
            H.div 
              ~a:[H.a_id "output";] [];
            H.canvas 
              ~a:[H.a_id "main_canvas"; H.a_width width; H.a_height height] [];

          ]))
  )
