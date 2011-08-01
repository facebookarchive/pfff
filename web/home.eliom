module H = HTML5.M
module Link = Eliom_output.Html5

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)
let main_service =
  Eliom_output.Html5.register_service 
  ~path:[""]
  ~get_params:Eliom_parameters.unit
  (fun () () ->
    Eliom_services.onload
      {{ (* *) () }};
    Lwt.return
      (H.html (H.head (H.title (H.pcdata "Home")) []) (H.body [

        H.h1 [Link.a Dump_server.main_service [H.pcdata "dumper"]  ()];
        H.h1 [Link.a Lxr_server.main_service  [H.pcdata "lxr"]     ""];
        H.h1 [Link.a Codemap.main_service     [H.pcdata "codemap"] "."];

      ]))
  )
