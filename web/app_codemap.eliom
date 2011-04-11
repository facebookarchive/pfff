{shared{
}}

{client{
}}

module H = XHTML5.M

(* pad: could remove the need to pass the canvas_box and retrive it instead
 * via a getElementById in client.ml.
 *)
let include_canvas (name:string) (canvas_box:[ Xhtml5types.div ] XHTML5.M.elt) =

  let bus, image_string = 
    Codemap_server.bus_image name in

  let imageservice =
    Eliom_output.Text.register_coservice'
      ~timeout:10.
      (* the service is available fo 10 seconds only, but it is long
	 enouth for the browser to do its request. *)
      ~get_params:Eliom_parameters.unit
      (fun () () -> 
        Lwt.return (image_string (), "image/png")
      )
  in

  Eliom_services.onload {{
    Codemap_client.launch_client_canvas %bus %imageservice %canvas_box
  }}

let () = Codemap_server.App.register ~service:Codemap_server.service
  (fun name () ->
    (* the page element in wich we will include the canvas *)

    (* this div is qualified with original module name otherwise one get
     * an error message when used above in the '%canvas_box'
     *)
    let canvas_box = XHTML5.M.div [] in

    include_canvas name canvas_box;
    Lwt.return [
      H.h1 [H.pcdata name];
      (* if want switch to another drawing
       *  Server.choose_drawing_form ();
       *)
      canvas_box;
    ] 
  )
