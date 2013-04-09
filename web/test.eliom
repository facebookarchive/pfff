open Common
module H = Eliom_content.Html5.D

(*****************************************************************************)
(* Test *)
(*****************************************************************************)

module App = Eliom_registration.App (struct
    let application_name = "app"
end)

let main_service =
  App.register_service 
    ~path:["test"] 
    ~get_params:(Eliom_parameter.unit)
    (fun () () ->

    ignore
      {unit{ (* *) () }};

    Lwt.return
      (H.html (H.head (H.title (H.pcdata "test")) []) (H.body [
        H.canvas
          ~a:[]
          []
      ]
      ))
    )
