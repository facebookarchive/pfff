open Common

module H = XHTML.M
module Link = Eliom_output.Xhtml

let top = Eliom_services.service [""] (Eliom_parameters.unit) ()

let _ = Eliom_output.Xhtml.register top
 (fun () () ->
   let html =
     H.html (*~a:[H.a_xmlns `W3_org_1999_xhtml; H.a_xml_lang "en"]*)
       (H.head
           (H.title (H.pcdata "Pfff Web UI"))
           [])
       (H.body [
         H.h1 [H.pcdata "coucou"];
         H.h1 [Link.a Lxr.lxr [H.pcdata "lxr"] ""];
         H.h1 [Link.a Graffiti_server.main_service [H.pcdata "graffiti"] ()];
       ]
       )
   in
   Lwt.return html
 )

