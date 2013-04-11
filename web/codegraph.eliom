open Common
module H = Eliom_content.Html5.D

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* App *)
(*****************************************************************************)
module App = Eliom_registration.App (struct
    let application_name = "app"
end)

(*****************************************************************************)
(* Shared *)
(*****************************************************************************)

{shared{
let width = 1000
let height = 1000
module DM = Dependencies_matrix_code

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

    (* TODO: compute config based on path *)
    let config = DM.basic_config_opti Globals.gopti in
    let m, _gopti =
      DM.build config (None) Globals.gopti in
    

    ignore
      {unit { View_matrix_codegraph.paint_weirrrrd %m }};
    Lwt.return
      (H.html 
          (H.head (H.title (H.pcdata "CodeGraph")) [ 
          ])
	  (H.body [
            (* used by runtime1.js, useful to see exceptions thrown *)
            H.div ~a:[H.a_id "output";] [];

            H.canvas
              ~a:[H.a_id "main_canvas"; H.a_width width; H.a_height height]
              [];
          ]))
  )
