(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: aargb.ml 1385 2007-09-27 02:12:42Z garrigue $ *)

let init () =
  List.iter Gl.enable [`line_smooth; `blend];
  GlFunc.blend_func ~src:`src_alpha ~dst:`one_minus_src_alpha;
  GlMisc.hint `line_smooth `dont_care;
  GlDraw.line_width 4.;
  GlClear.color (0., 0., 0.)

let rot_angle = ref 0.

let display ~area () =
  GlClear.clear [`color];
  GlDraw.color (0., 1., 0.);
  GlMat.push ();
  GlMat.rotate ~angle:(-. !rot_angle) ~z:0.1 ();
  GlDraw.begins `lines;
    GlDraw.vertex2 (-0.5, 0.5);
    GlDraw.vertex2 (0.5, -0.5);
  GlDraw.ends ();
  GlMat.pop ();
  GlDraw.color (0., 0., 1.);
  GlMat.push ();
  GlMat.rotate ~angle:(!rot_angle) ~z:0.1 ();
  GlDraw.begins `lines;
    GlDraw.vertex2 (0.5, 0.5);
    GlDraw.vertex2 (-0.5, -0.5);
  GlDraw.ends ();
  GlMat.pop ();
  Gl.flush ();
  area#swap_buffers ()

let reshape ~width:w ~height:h =
  GlDraw.viewport ~x:0 ~y:0 ~w ~h;
  GlMat.mode `projection;
  GlMat.load_identity();
  if w < h then
    GluMat.ortho2d ~x:(-1., 1.) ~y:(-.float w /. float h, float w /. float h)
  else
    GluMat.ortho2d ~y:(-1., 1.) ~x:(-.float w /. float h, float w /. float h);
  GlMat.mode `modelview;
  GlMat.load_identity()

open GdkKeysyms

let main () =
  let w = GWindow.window ~title:"Antialiasing/Gtk" () in
  w#connect#destroy ~callback:GMain.quit;
  let area =
    GlGtk.area [`RGBA;`DOUBLEBUFFER]
      ~width:500 ~height:500 ~packing:w#add () in
  area#connect#realize ~callback:init;
  area#connect#reshape ~callback:reshape;
  area#connect#display ~callback:(display ~area);
  w#event#connect#key_press ~callback:
    begin fun ev ->
      let key = GdkEvent.Key.keyval ev in
      if key = _r || key = _R then begin
	rot_angle := !rot_angle +. 20.;
	if !rot_angle > 360. then rot_angle := 0.;
	display ~area ()
      end;
      true
    end;
  
  w#show ();
  GMain.main ()

let _ = main ()
