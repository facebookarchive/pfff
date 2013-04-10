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

(* from jflo slides *)
let unopt x =
  Js.Opt.get x (fun () -> raise Not_found)
let retrieve id =
  unopt (Dom_html.document ## getElementById (Js.string id))
}}

{shared{
let width = 500
let height = 500
}}

(*****************************************************************************)
(* Test *)
(*****************************************************************************)

module App = Eliom_registration.App (struct
    let application_name = "app"
end)

{client{
(* imitate Main_codemap.test_draw but using canvas instead of cairo *)
let test_draw ctx =

  ctx##font <- Js.string (spf "20 px serif" );
  let text = "f" in
  let metric = ctx##measureText (Js.string text) in
  let width_text = metric##width in
  pr2 (spf "width text = %f" width_text);

  let fill_text_scaled ctx str ~x ~y ~size =
    let orig = 500. in
(*
    ctx##save;
    ctx##translate (x * orig) (y * orig);
    ctx##restore;
*)
    pr2_gen orig
  in

  (* 500 -> width_text *)
  (* 1 -> *)
  (* 0.1 -> ? *)

  ctx##setTransform (1.,0.,0.,1.,0.,0.);
  ctx##scale (float_of_int width, float_of_int height);

  let (r, g, b) = 0.5, 0.5, 0.5 in
  let alpha = 0.5 in
  ctx##fillStyle <- Js.string (CanvasH.rgba_of_rgbf (r,g,b) alpha);

  ctx##lineWidth <- 0.001;
  ctx##moveTo (0.5, 0.5);
  ctx##lineTo (0.6, 0.6);
  ctx##stroke();
(*
  Cairo.set_source_rgb cr ~red:0.5 ~green:0.5 ~blue:0.5;
  Cairo.set_line_width cr 0.001;
  Cairo.move_to cr 0.5 0.5;
  Cairo.line_to cr 0.6 0.6;
  Cairo.stroke cr;
*)

  fill_text_scaled ctx "THIS IS SOME TEXT" ~x:0.1 ~y:0.1 ~size:0.1;


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

(*
  Cairo.set_source_rgb cr ~red:0.1 ~green:0.1 ~blue:0.1;
  Cairo.move_to cr 0.1 0.1;
  Cairo.line_to cr 0.1 0.2;
  Cairo.stroke cr;
*)

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
        test_draw ctx
      }};

    Lwt.return
      (H.html (H.head (H.title (H.pcdata "test")) []) (H.body [
        H.canvas
          ~a:[H.a_id "main_canvas"; H.a_width width; H.a_height height]
          []
      ]
      ))
    )
