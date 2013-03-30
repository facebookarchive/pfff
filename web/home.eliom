module H = Eliom_content.Html5.D
module Link = Eliom_content.Html5.D

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)
let main_service =
  Eliom_registration.Html5.register_service 
  ~path:[""]
  ~get_params:Eliom_parameter.unit
  (fun () () ->
    ignore
      {unit{ (* *) () }};
    Lwt.return
      (H.html (H.head (H.title (H.pcdata "Home")) []) (H.body [

        H.h1 [Link.a Dump_server.main_service [H.pcdata "dumper"]  ()];
(*        H.h1 [Link.a Lxr_server.main_service  [H.pcdata "lxr"]     ""]; *)
        H.h1 [Link.a Codemap.main_service     [H.pcdata "codemap"] 
                 "/home/pad/pfff/facebook/tests/mini_www"];
        H.h1 [Link.a Codemap.main_service     [H.pcdata "overlay"] 
                 "/home/pad/overlays/www"];

      ]))
  )
