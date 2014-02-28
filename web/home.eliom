
module H = Eliom_content.Html5.D

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)
let main_service =
  Eliom_registration.Html5.register_service 
  ~path:[""]
  ~get_params:Eliom_parameter.unit
  (fun () () ->
    let size = "small" in
    ignore
      {unit{ (* *) () }};
    Lwt.return
      (H.html (H.head (H.title (H.pcdata "Pfff Web Interface")) []) (H.body [
        (* used by runtime1.js, useful to see exceptions thrown *)
        H.div ~a:[H.a_id "output";] [];

        H.h1 [H.a Dump_server.main_service [H.pcdata "dumper"]  
                 ()];

        H.h1 [H.a Codemap.main_service     [H.pcdata "codemap"] 
                 (size, ("pfff", ""))];
        H.h1 [H.a Codegraph.main_service     [H.pcdata "codegraph"] 
                 (size, ("pfff", ""))];

        H.h1 [H.a Codegraph.main_service     [H.pcdata "fbobjc deps"] 
                 ("large", ("fbobjc", ""))];
        H.h1 [H.a Codemap.main_service     [H.pcdata "hack dashboard"] 
                 (size, ("hack", ""))];

(*      H.h1 [H.a Lxr_server.main_service  [H.pcdata "lxr"]     ""]; *)
(*
        H.h1 [H.a Codemap.main_service     [H.pcdata "overlay"] 
                 "/home/pad/overlays/www"];
*)
        H.h1 [H.a Test.main_service [H.pcdata "test"] ()];

        H.h1 [H.a Codemap.test_codemap_micro     [H.pcdata "test_micro"] ()];

      ]))
  )
