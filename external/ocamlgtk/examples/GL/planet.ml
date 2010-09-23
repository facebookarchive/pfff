(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: planet.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

open StdLabels

class planet area = object (self)
  val area : GlGtk.area = area
  val mutable year = 0.0
  val mutable day = 0.0
  val mutable eye = 0.0
  val mutable time = 0.0

  method tick new_time =
    if time = 0. then time <- new_time else
    let diff = new_time -. time in
    time <- new_time;
    day <- mod_float (day +. diff *. 200.) 360.0;
    year <- mod_float (year +. diff *. 20.) 360.0
  method day_add () =
    day <- mod_float (day +. 10.0) 360.0
  method day_subtract () =
    day <- mod_float (day -. 10.0) 360.0
  method year_add () =
    year <- mod_float (year +. 5.0) 360.0
  method year_subtract () =
    year <- mod_float (year -. 5.0) 360.0
  method eye x =
    eye <- x; self#display ()

  method display () =
    GlClear.clear [`color;`depth];

    GlDraw.color (1.0, 1.0, 1.0);
    GlMat.push();
    GlMat.rotate ~angle:eye ~x:1. ();
(*	draw sun	*)
    GlLight.material ~face:`front (`specular (1.0,1.0,0.0,1.0));
    GlLight.material ~face:`front (`shininess 5.0);
    GluQuadric.sphere ~radius:1.0 ~slices:32 ~stacks:32 ();
(*	draw smaller planet	*)
    GlMat.rotate ~angle:year ~y:1.0 ();
    GlMat.translate ~x:3.0 ();
    GlMat.rotate ~angle:day ~y:1.0 ();
    GlDraw.color (0.0, 1.0, 1.0);
    GlDraw.shade_model `flat;
    GlLight.material ~face:`front(`shininess 128.0);
    GluQuadric.sphere ~radius:0.2 ~slices:10 ~stacks:10 ();
    GlDraw.shade_model `smooth;
    GlMat.pop ();
    Gl.flush ();
    area#swap_buffers ()
end

let myinit () =
  let light_ambient = 0.5, 0.5, 0.5, 1.0
  and light_diffuse = 1.0, 0.8, 0.2, 1.0
  and light_specular = 1.0, 1.0, 1.0, 1.0
  (*  light_position is NOT default value	*)
  and light_position = 1.0, 1.0, 1.0, 0.0
  in
  List.iter ~f:(GlLight.light ~num:0)
    [ `ambient light_ambient; `diffuse light_diffuse;
      `specular light_specular; `position light_position ];
  GlFunc.depth_func `less;
  List.iter ~f:Gl.enable [`lighting; `light0; `depth_test];
  GlDraw.shade_model `smooth


let my_reshape ~width:w ~height:h =
  GlDraw.viewport ~x:0 ~y:0 ~w ~h;
  GlMat.mode `projection;
  GlMat.load_identity();
  GluMat.perspective ~fovy:60.0 ~aspect:(float w /. float h) ~z:(1.0,20.0);
  GlMat.mode `modelview;
  GlMat.load_identity();
  GlMat.translate ~z:(-5.0) ()

(*  Main Loop
 *  Open window with initial window size, title bar, 
 *  RGBA display mode, and handle input events.
 *)
open GMain
open GdkKeysyms

let main () =
  let w = GWindow.window ~title:"Planet" () in
  w#connect#destroy ~callback:(fun () -> Main.quit (); exit 0);
  w#set_resize_mode `IMMEDIATE;
  let hb = GPack.hbox ~packing:w#add () in
  let area = GlGtk.area [`DOUBLEBUFFER;`RGBA;`DEPTH_SIZE 1]
      ~width:700 ~height:500 ~packing:hb#add () in
  area#event#add [`KEY_PRESS];

  let planet = new planet area in
  let adjustment = GData.adjustment ~value:0. ~lower:(-90.) ~upper:90.
      ~step_incr:1. ~page_incr:5. ~page_size:5. () in
  let scale = GRange.scale `VERTICAL ~adjustment ~draw_value:false
      ~packing:hb#pack () in
  adjustment#connect#value_changed
    ~callback:(fun () -> planet#eye adjustment#value);
  w#event#connect#key_press ~callback:
    begin fun ev ->
      let key = GdkEvent.Key.keyval ev in
      if key = _Left then planet#year_subtract () else
      if key = _Right then planet#year_add () else
      if key = _Up then planet#day_add () else
      if key = _Down then planet#day_subtract () else
      if key = _Escape then w#destroy ();
      planet#display ();
      true
    end;
  
  Timeout.add ~ms:20 ~callback:
    begin fun () ->
      planet#tick (Sys.time ()); planet#display (); true
    end;
  area#connect#display ~callback:planet#display;
  area#connect#reshape ~callback:my_reshape;

  area#connect#realize ~callback:
    begin fun () ->
      myinit ();
      my_reshape ~width:700 ~height:500
    end;
  w#show ();
  Main.main ()

let _ = Printexc.print main ()
