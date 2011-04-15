(*
 * Please imagine a long and boring gnu-style copyright notice 
 * appearing just here.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Default Page *)
(*****************************************************************************)

{shared{
}}

{client{
}}

module H = XHTML5.M

module App = Eliom_output.Eliom_appl (struct
  let application_name = "client"
  let params = { Eliom_output.default_appl_params with
	  
    Eliom_output.ap_headers_before = [
      H.link ~rel:[`Stylesheet] ~href:(H.uri_of_string "css/default.css")  ();

      H.link ~rel:[`Stylesheet] ~href:(H.uri_of_string "css/goog/common.css")();
      H.link ~rel:[`Stylesheet] ~href:(H.uri_of_string "css/goog/menu.css")();
      H.link ~rel:[`Stylesheet] ~href:(H.uri_of_string "css/goog/menuitem.css")();
      H.link ~rel:[`Stylesheet] ~href:(H.uri_of_string "css/goog/menuseparator.css")();

      H.link ~rel:[`Stylesheet] ~href:(H.uri_of_string "css/codemap.css")  ();

      H.script ~a:[H.a_src (H.uri_of_string "js/goog/base.js")] (H.pcdata "");

      H.script ~a:[H.a_src (H.uri_of_string "client_req.js")] (H.pcdata "");
    ];
  }
end)

let main_service = Eliom_services.service ~path:["codemap"]
  ~get_params:(Eliom_parameters.unit) ()

(*****************************************************************************)
(* main entry point *)
(*****************************************************************************)
let () = App.register ~service:main_service
  (fun () () ->
    Eliom_services.onload {{
      Codemap_client.onload ();
    }};
    Lwt.return [
      H.h1 [H.pcdata "Welcome to CodemapWeb"];
    ])
