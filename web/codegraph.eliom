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

    ignore
      {unit { () }};
    Lwt.return
      (H.html 
          (H.head (H.title (H.pcdata "CodeGraph")) [ 
          ])
	  (H.body [
            H.canvas
              ~a:[H.a_id "main_canvas"; H.a_width width; H.a_height height]
              []
          ]))
  )
