{shared{
  open XHTML5.M
  open Shared
}}
{client{
  open Client
}}
open Server

let include_canvas (name:string) (canvas_box:[ Xhtml5types.div ] XHTML5.M.elt) =

  let bus,image_string = get_bus_image name in

  let imageservice =
    Eliom_output.Text.register_coservice'
      ~timeout:10.
      (* the service is available fo 10 seconds only, but it is long
	 enouth for the browser to do its request. *)
      ~get_params:Eliom_parameters.unit
      (fun () () -> Lwt.return (image_string (), "image/png"))
  in

  Eliom_services.onload
    {{
      launch_client_canvas %bus %imageservice %canvas_box
    }}

let () = My_appl.register ~service:multigraffiti_service
  ( fun name () ->
    (* the page element in wich we will include the canvas *)
    let canvas_box = div [] in
    include_canvas name canvas_box;
    Lwt.return [
      h1 [pcdata name];
      choose_drawing_form ();
      canvas_box;] )
