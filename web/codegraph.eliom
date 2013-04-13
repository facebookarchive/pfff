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
module DM = Dependencies_matrix_code
module Model = Model_codegraph
}}

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)
let main_service =
  App.register_service 
    ~path:["codegraph"] 
    ~get_params:(Eliom_parameter.string "path")
  (fun path () ->
    pr2 path;

    (* TODO: compute config based on path, and compute g depending
     * on some OCaml pfff repo type.
     *)
    let path = [DM.Expand ("lang_php", Database_code.Dir)] in
    DM.threshold_pack := 4000;
    let config, _goptiTODO = DM.config_of_path path Globals.gopti in
    let m, _goptiTODO =
      DM.build config (None) Globals.gopti in

    let w = { Model.
       m;
       width = width;
       height = height;
       
       orig_coord_width = 0.;
       orig_coord_height = 0.;
       width_text_etalon_normalized_coord = 0.;
    } in

    ignore
      {unit { View_matrix_codegraph.paint %w }};
    Lwt.return
      (H.html 
          (H.head (H.title (H.pcdata "CodeGraph")) [ 
          ])
          (H.body [
            H.div 
              ~a:[H.a_id "output";] [];
            H.canvas
              ~a:[H.a_id "main_canvas"; H.a_width width; H.a_height height] [];
          ]))
  )
