
module H = XHTML5.M
module Shared = Codemap_shared

module App = Eliom_output.Eliom_appl (struct
  let application_name = "client"
  let params = { Eliom_output.default_appl_params with
	  
    Eliom_output.ap_headers_before = [
      H.link ~rel:[`Stylesheet] ~href:(H.uri_of_string "css/graffiti.css") ();
      H.link ~rel:[`Stylesheet] ~href:(H.uri_of_string "css/common.css")  ();
      H.link ~rel:[`Stylesheet] ~href:(H.uri_of_string "css/hsvpalette.css")();
      H.link ~rel:[`Stylesheet] ~href:(H.uri_of_string "css/slider.css") ();
     (*"http://closure-library.googlecode.com/svn/trunk/closure/goog/base.js"*)
      H.script ~a:[H.a_src (H.uri_of_string "js/goog/base.js")] (H.pcdata "");

      H.script ~a:[H.a_src (H.uri_of_string "client_req.js")] (H.pcdata "");
    ];
  }
end)

let h_graffiti_info = Hashtbl.create 11
let bus_image (name:string) = 
(* create a new bus and image_string function only if it did not exists *)
 Lib.memoized h_graffiti_info name (fun () ->
  let bus = Eliom_bus.create Json.t<Shared.messages> in
  
  let draw_server, image_string =
    let surface = 
      Cairo.image_surface_create Cairo.FORMAT_ARGB32 
        ~width:Shared.width ~height:Shared.height in
    let ctx = Cairo.create surface in
    (fun ((color : string), size, (x1, y1), (x2, y2)) ->

      (* Set thickness of brush *)
      Cairo.set_line_width ctx (float size) ;
      Cairo.set_line_join ctx Cairo.LINE_JOIN_ROUND ;
      Cairo.set_line_cap ctx Cairo.LINE_CAP_ROUND ;
      let red, green, blue =  Lib.rgb_from_string color in
      Cairo.set_source_rgb ctx ~red ~green ~blue ;

      Cairo.move_to ctx (float x1) (float y1) ;
      Cairo.line_to ctx (float x2) (float y2) ;
      Cairo.close_path ctx ;
      
      (* Apply the ink *)
      Cairo.stroke ctx ;
     ),
     (fun () ->
       let b = Buffer.create 10000 in
       (* Output a PNG in a string *)
       Cairo_png.surface_write_to_stream surface (Buffer.add_string b);
       Buffer.contents b
     )
  in
  let _ = Lwt_stream.iter draw_server (Eliom_bus.stream bus) in
  bus, image_string
 )

let main_service = Eliom_services.service ~path:["codemap"]
  ~get_params:(Eliom_parameters.unit) ()

(* this service is defined in app.eliom *)
let service = Eliom_services.coservice ~fallback:main_service
  ~get_params:(Eliom_parameters.string "name") ()

(*****************************************************************************)
(* main entry point *)
(*****************************************************************************)
let () = App.register ~service:main_service
  (fun () () ->
    Lwt.return [
      H.h1 [H.pcdata "Welcome to Multigraffiti"];
      App.get_form ~service:service
        (fun (name) ->
          [H.p [
            H.pcdata "drawing name: ";
            App.string_input ~input_type:`Text ~name ();
            H.br ();
            App.string_input ~input_type:`Submit ~value:"Go" ()
          ]])
    ])
