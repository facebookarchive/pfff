open Common
module H = Eliom_content.Html5.D

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

{client{
open Common
open Common2.ArithFloatInfix
open Common_client
module CanvasH = Canvas_helpers

}}

{shared{
let width = 500
let height = 500
}}

(*****************************************************************************)
(* Logging *)
(*****************************************************************************)
let log str = 
  Lwt_io.write_line Lwt_io.stdout str
let rpc_log = 
  Eliom_pervasives.server_function Json.t<string> log

(*****************************************************************************)
(* Test *)
(*****************************************************************************)

(* Without this, I get some "sitedata not defined" js error.
 * Maybe some headers sent back by eliom apps contain
 * some important information.
 *)
module App = Eliom_registration.App (struct
    let application_name = "app"
end)

{client{
(* imitate Main_codemap.test_draw but using canvas instead of cairo *)
let test_draw ctx =

  (* ugly hack because html5 canvas does not handle using float size for fonts
   * when printing text in a scaled context.
   *)
  ctx##font <- Js.string (spf "bold 12 px serif" );
  let text = "MM" in
  let metric = ctx##measureText (Js.string text) in
  let width_text_etalon_orig_coord = metric##width / 2.0 in
  pr2 (spf "width text orig coord = %f" width_text_etalon_orig_coord);

  let orig_coord_width = 500. in
  let normalized_coord_width = 1. in

  let width_text_etalon_normalized_coord = 
    (normalized_coord_width * width_text_etalon_orig_coord) /
      orig_coord_width
  in
  pr2 (spf "width text normalized coord = %f" width_text_etalon_normalized_coord);

  let fill_text_scaled ctx str ~x ~y ~size =
    ctx##save ();
    ctx##setTransform (1.,0.,0.,1.,0.,0.);
    ctx##translate (x * orig_coord_width, y * orig_coord_width);


    let scale_factor = size / width_text_etalon_normalized_coord in

    ctx##scale (scale_factor, scale_factor);
    ctx##fillText (Js.string str, 0., 0.);
    let metric = ctx##measureText (Js.string str) in
    pr2 (spf "width = %f" metric##width);
    ctx##restore ();
    metric##width * scale_factor / orig_coord_width
  in

  ctx##setTransform (1.,0.,0.,1.,0.,0.);
  ctx##scale (float_of_int width, float_of_int height);

(*
  Cairo.set_source_rgb cr ~red:0.5 ~green:0.5 ~blue:0.5;
  Cairo.set_line_width cr 0.001;
  Cairo.move_to cr 0.5 0.5;
  Cairo.line_to cr 0.6 0.6;
  Cairo.stroke cr;
*)
  let (r, g, b) = 0.5, 0.5, 0.5 in
  let alpha = 0.5 in
  ctx##strokeStyle <- Js.string (CanvasH.css_color_of_rgbf (r,g,b) alpha);
  ctx##lineWidth <- 0.001;
  ctx##moveTo (0.5, 0.5);
  ctx##lineTo (0.6, 0.6);
  ctx##stroke();

(*
  Cairo.select_font_face cr "serif"
    Cairo.FONT_SLANT_NORMAL Cairo.FONT_WEIGHT_BOLD;
  Cairo.set_font_size cr 0.1;
  Cairo.move_to cr 0.1 0.1;
  Cairo.show_text cr "THIS IS SOME TEXT";
  Cairo.move_to cr 0.1 0.2;
  Cairo.show_text cr "THIS IS SOME TEXT";
  Cairo.move_to cr 0.1 0.3;
  Cairo.show_text cr "THIS IS SOME TEXT";
*)
  ctx##fillStyle <- Js.string (CanvasH.css_color_of_rgbf (r,g,b) alpha);
  ignore(fill_text_scaled ctx "THIS IS" ~x:0.1 ~y:0.1 ~size:0.1);
  let width = fill_text_scaled ctx "THIS IS" ~x:0.1 ~y:0.2 ~size:0.1 in
  ignore(fill_text_scaled ctx " AFTER" ~x:(0.1 + width) ~y:0.2 ~size:0.1);
  ignore(fill_text_scaled ctx "THIS IS" ~x:0.1 ~y:0.3 ~size:0.05);

(*
  Cairo.set_source_rgb cr ~red:0.1 ~green:0.1 ~blue:0.1;
  Cairo.move_to cr 0.1 0.1;
  Cairo.line_to cr 0.1 0.2;
  Cairo.stroke cr;
*)
  ctx##fillStyle <- Js.string (CanvasH.css_color_of_rgbf (0.1,0.1,0.1) 1.0);
  ctx##moveTo (0.1, 0.1);
  ctx##lineTo (0.1, 0.2);
  ctx##stroke();

  ctx##beginPath();
  ctx##moveTo (0.3, 0.3);
  ctx##lineTo (0.4, 0.3);
  ctx##lineTo (0.4, 0.4);
  ctx##lineTo (0.3, 0.4);
  ctx##closePath();
  ctx##fill();

}}

let main_service =
  App.register_service 
    ~path:["test"] 
    ~get_params:(Eliom_parameter.unit)
    (fun () () ->

    ignore
      {unit{
        let canvas = 
          retrieve "main_canvas" +> 
          Dom_html.CoerceTo.canvas +>
          unopt
        in
        let ctx = canvas##getContext (Dom_html._2d_) in
        test_draw ctx;

        let f x = 
          Dom_html.window##alert(x);
          %rpc_log (spf "CONNECTION Test.eliom: %s" (Js.to_string x))
        in

        Js.Unsafe.fun_call (Js.Unsafe.variable "getUserName") 
          [| Js.Unsafe.inject f|];
      }};

    Lwt.return
      (H.html (H.head (H.title (H.pcdata "test")) 
         (* this is now included by default
         H.js_script 
            ~uri:(H.make_uri  (Eliom_service.static_dir ())["app.js"]) ();
         *)
         (if !Globals.use_facebook
         then 
           [H.js_script 
            ~uri:(H.make_uri  (Eliom_service.static_dir ())
                    ["app_facebook.js"]) ();
           ]
         else []
         )
      ) (H.body [
        (* used by runtime1.js, useful to see exceptions thrown *)
        H.div ~a:[H.a_id "output";] [];

        H.canvas
          ~a:[H.a_id "main_canvas"; H.a_width width; H.a_height height]
          []
      ]
      ))
    )
