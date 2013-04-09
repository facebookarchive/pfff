open Common
module H = Eliom_content.Html5.D

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

{client{
open Common
open Common_client

(* from jflo slides *)
let unopt x =
  Js.Opt.get x (fun () -> raise Not_found)
let retrieve id =
  unopt (Dom_html.document ## getElementById (Js.string id))
}}

(*****************************************************************************)
(* Test *)
(*****************************************************************************)

module App = Eliom_registration.App (struct
    let application_name = "app"
end)

{client{
let test_draw ctx =
  pr2 "TODO"
}}

let main_service =
  App.register_service 
    ~path:["test"] 
    ~get_params:(Eliom_parameter.unit)
    (fun () () ->

    ignore
      {unit{
        let canvas = 
          retrieve "main_canvas" +> 
          Dom_html.CoerceTo.canvas +>
          unopt
        in
        let ctx = canvas##getContext (Dom_html._2d_) in
        test_draw ctx
      }};

    Lwt.return
      (H.html (H.head (H.title (H.pcdata "test")) []) (H.body [
        H.canvas
          ~a:[H.a_id "main_canvas"; H.a_width 500; H.a_height 500]
          []
      ]
      ))
    )
