(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: texturesurf.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

open StdLabels

let texpts =
  [|[|0.0; 0.0;  0.0; 1.0|];
    [|1.0; 0.0;  1.0; 1.0|]|]

let ctrlpoints =
  [|[|-1.5; -1.5; 4.9;  -0.5; -1.5; 2.0;  0.5; -1.5; -1.0; 1.5; -1.5; 2.0|];
    [|-1.5; -0.5; 1.0;  -0.5; -0.5; 3.0;  0.5; -0.5; 0.0;  1.5; -0.5; -1.0|];
    [|-1.5; 0.5; 4.0;   -0.5; 0.5; 0.0;   0.5; 0.5; 3.0;   1.5; 0.5; 4.0|];
    [|-1.5; 1.5; -2.0;  -0.5; 1.5; -2.0;  0.5; 1.5; 0.0;   1.5; 1.5; -1.0|]|]

let image_width = 64
and image_height = 64

let pi = acos (-1.0)

let display togl =
  GlClear.clear [`color;`depth];
  GlDraw.color (1.0,1.0,1.0);
  GlMap.eval_mesh2 ~mode:`fill ~range1:(0,20) ~range2:(0,20);
  Gl.flush ();
  togl#swap_buffers ()

let make_image () =
  let image =
    GlPix.create `ubyte ~height:image_height ~width:image_width ~format:`rgb in
  let raw = GlPix.to_raw image
  and pos = GlPix.raw_pos image in
  for i = 0 to image_width - 1 do
    let ti = 2.0 *. pi *. float i /. float image_width in
    for j = 0 to image_height - 1 do
      let tj = 2.0 *. pi *. float j /. float image_height in
      Raw.sets raw ~pos:(pos ~x:j ~y:i)
	(Array.map ~f:(fun x -> truncate (127.0 *. (1.0 +. x)))
	   [|sin ti; cos (2.0 *. ti); cos (ti +. tj)|]);
      done;
  done;
  image

let read_to_glpix myfile =
  let pix = GdkPixbuf.from_file myfile in
  let pix =
    if GdkPixbuf.get_has_alpha pix then pix else
    GdkPixbuf.add_alpha pix
  in
  let src = GdkPixbuf.get_pixels pix in
  let raw = Raw.create `ubyte ~len:(Gpointer.length src) in
  Gpointer.blit ~src ~dst:(GlGtk.region_of_raw raw);
  GlPix.of_raw raw ~format:`rgba ~width:(GdkPixbuf.get_width pix)
    ~height:(GdkPixbuf.get_height pix)

(* You may use your own texture, but its size must be 2**m x 2**n *)
(* let make_image () = read_to_glpix "sa15-crop.jpg" *)

let myinit () =
  let ctrlpoints = Raw.of_matrix ~kind:`double ctrlpoints
  and texpts = Raw.of_matrix ~kind:`double texpts in
  GlMap.map2 ~target:`vertex_3
    (0.0, 1.0) ~order:4 (0.0, 1.0) ~order:4 ctrlpoints;
  GlMap.map2 ~target:`texture_coord_2
    (0.0,1.0) ~order:2 (0.0,1.0) ~order:2 texpts;
  Gl.enable `map2_texture_coord_2;
  Gl.enable `map2_vertex_3;
  GlMap.grid2 ~n1:20 ~range1:(0.0,1.0) ~n2:20 ~range2:(0.0,1.0);
  let image = make_image () in
  GlTex.env (`mode `decal);
  List.iter ~f:(GlTex.parameter ~target:`texture_2d)
    [ `wrap_s `repeat;
      `wrap_t `repeat;
      `mag_filter `nearest;
      `min_filter `nearest ];
  GlTex.image2d image;
  List.iter ~f:Gl.enable [`texture_2d;`depth_test;`normalize];
  GlDraw.shade_model `flat

let my_reshape ~width ~height =
  GlDraw.viewport ~x:0 ~y:0 ~w:width ~h:height;
  GlMat.mode `projection;
  GlMat.load_identity ();
  let r = float height /. float width in
  if width <= height then
    GlMat.ortho ~x:(-4.0, 4.0) ~y:(-4.0 *. r, 4.0 *. r) ~z:(-4.0, 4.0)
  else
    GlMat.ortho ~x:(-4.0 /. r, 4.0 /. r) ~y:(-4.0, 4.0) ~z:(-4.0, 4.0);
  GlMat.mode `modelview;
  GlMat.load_identity ();
  GlMat.rotate ~angle:85. ~x:1. ~y:1. ~z:1. ()

open GdkKeysyms

let main () =
  let w = GWindow.window ~title:"Texture Surf" () in
  let togl =
    GlGtk.area [`RGBA;`DOUBLEBUFFER;`DEPTH_SIZE 1]
      ~width:300 ~height:300 ~packing:w#add ()
  in
  togl#misc#connect#realize myinit;
  togl#connect#reshape my_reshape;
  togl#connect#display (fun () -> display togl);
  w#event#connect#key_press ~callback:
    begin fun ev ->
      let k = GdkEvent.Key.keyval ev in
      if k = _Up then
        (GlMat.rotate ~angle:(-5.) ~z:1.0 (); display togl)
      else if k = _Down then
        (GlMat.rotate ~angle:(5.) ~z:1.0 (); display togl)
      else if k = _Left then
        (GlMat.rotate ~angle:(5.) ~x:1.0 (); display togl)
      else if k = _Right then
        (GlMat.rotate ~angle:(-5.) ~x:1.0 (); display togl)
      else if k = _Escape then
        w#destroy ();
      true
    end;
  w#connect#destroy ~callback:GMain.quit;
  w#show ();
  GMain.main ()

let _ = main ()
