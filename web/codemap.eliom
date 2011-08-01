module H = HTML5.M

module Link = Eliom_output.Html5

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)
let main_service =
  Server.App.register_service ~path:["codemap"] 
    ~get_params:Eliom_parameters.unit
  (fun () () ->
    Eliom_services.onload
      {{ (* *) () }};
    Lwt.return
      (H.html
	  (H.head
	    (H.title (H.pcdata "Codemap"))
 	    [ H.link ~rel:[ `Stylesheet ] 
                ~href:(H.uri_of_string"css/app.css")();
              H.link ~rel:[ `Stylesheet ]
                ~href:(H.uri_of_string"css/closure/common.css")();
              H.unique
                (H.script
                    ~a:[H.a_src (H.uri_of_string "app_oclosure.js")]
                    (H.pcdata ""))
            ])
	  (H.body [
            H.h1 [H.pcdata "coucou"];
          ]))
  )
