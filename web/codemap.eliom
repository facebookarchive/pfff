open Common
module H = HTML5.M

module Flag = Flag_web

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* App *)
(*****************************************************************************)
(* Without this, get some "sitedata" not defined js error.
 * Maybe some headers sent back by an eliom apps contains
 * some important information.
 *)
module App = Eliom_output.Eliom_appl (struct
    let application_name = "app"
end)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)
let main_service =
  App.register_service 
    ~path:["codemap"] 
    ~get_params:(Eliom_parameters.string "path")
  (fun path () ->
    let path = Common.relative_to_absolute path in
    let rects =
      Server.treemap_generator [path]
    in
    Eliom_services.onload
      {{ Client.draw_treemap_rendering %rects }};
    Lwt.return
      (H.html
	  (H.head
	    (H.title (H.pcdata "Codemap"))
 	    [ 
              H.unique
                (H.script
                    ~a:[H.a_src (H.uri_of_string "app.js")]
                    (H.pcdata ""))
            ])
	  (H.body [
            H.h1 [H.pcdata "coucou"];
          ]))
  )
