open Common
module H = Eliom_content.Html5.D

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* App *)
(*****************************************************************************)
(* Without this, I get some "sitedata not defined" js error.
 * Maybe some headers sent back by eliom apps contain
 * some important information.
 *)
module App = Eliom_registration.App (struct
    let application_name = "app"
end)

(*****************************************************************************)
(* Shared *)
(*****************************************************************************)

{shared{

let width = 1200
let height = 680
module DM = Dependencies_matrix_code
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
         (* this is now included by default
         H.js_script 
            ~uri:(H.make_uri  (Eliom_service.static_dir ())["app.js"]) ();
         *)
          ])
	  (H.body [
            (* used by runtime1.js, useful to see exceptions thrown *)
            H.div ~a:[H.a_id "output";] [];

            H.canvas
              ~a:[H.a_id "main_canvas"; H.a_width width; H.a_height height]
              [];
          ]))
  )


(*****************************************************************************)
(* Testing *)
(*****************************************************************************)

let test_codemap_micro =
  App.register_service 
    ~path:["micro"] 
    ~get_params:(Eliom_parameter.string "path")
  (fun path () ->
    pr2 path;
    let path = Common2.relative_to_absolute path in
    let lines = Common.cat path in

    ignore
      {unit{ Client_codemap.draw_file %lines }};
    Lwt.return
      (H.html 
          (H.head (H.title (H.pcdata "Micro")) [ 
          ])
	  (H.body [
            (* used by runtime1.js, useful to see exceptions thrown *)
            H.div ~a:[H.a_id "output";] [];

          ]))
  )
