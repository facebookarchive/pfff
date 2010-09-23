(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

let zoom_changed canvas adj () =
  canvas#set_pixels_per_unit adj#value

type 'p item_state = {
    item : 'p GnoCanvas.item ;
    mutable dragging : bool ;
    mutable x : float ;
    mutable y : float ;
  }

let affine_rotate angle =
  let rad_angle = angle /. 180. *. acos (-1.) in
  let cos_a = cos rad_angle in
  let sin_a = sin rad_angle in
  [| cos_a ; sin_a ; ~-. sin_a ; cos_a ; 0. ; 0. |]

let affine_apply a x y =
  ( a.(0) *. x +. a.(2) *. y +. a.(4) ,
    a.(1) *. x +. a.(3) *. y +. a.(5) )

let affine_compose a1 a2 =
  [| a1.(0) *. a2.(0) +. a1.(1) *. a2.(2) ;
     a1.(0) *. a2.(1) +. a1.(1) *. a2.(3) ;
     a1.(2) *. a2.(0) +. a1.(3) *. a2.(2) ;
     a1.(2) *. a2.(1) +. a1.(3) *. a2.(3) ;
     a1.(4) *. a2.(0) +. a1.(5) *. a2.(2) +. a2.(4) ;
     a1.(4) *. a2.(1) +. a1.(5) *. a2.(3) +. a2.(5) ; |]

let affine_invert a =
  let r_det = 1. /. (a.(0) *. a.(3) -. a.(1) *. a.(2)) in
  [| a.(3) *. r_det ;
     ~-. (a.(1)) *. r_det ;
     ~-. (a.(2)) *. r_det ;
     a.(0) *. r_det ;
     (a.(2) *. a.(5) -. a.(3) *. a.(4)) *. r_det ;
     (a.(1) *. a.(4) -. a.(0) *. a.(5)) *. r_det ; |]

let affine_transl x y =
  [| 1. ; 0. ; 0. ; 1. ; x ; y |]

let affine_rotate_around_point x y angle =
  affine_compose
    (affine_compose
       (affine_transl (~-. x) (~-. y))
       (affine_rotate angle))
    (affine_transl x y)

let d_theta = 15.

let item_event_button_press config ev =
  let state = GdkEvent.Button.state ev in
  match GdkEvent.Button.button ev with
  | 1 when Gdk.Convert.test_modifier `SHIFT state ->
      config.item#destroy ()
  | 1 when Gdk.Convert.test_modifier `CONTROL state ->
      let (x, y) = config.item#w2i (GdkEvent.Button.x ev) (GdkEvent.Button.y ev) in
      config.item#affine_relative
	(affine_rotate_around_point x y d_theta) ;
  | 3 when Gdk.Convert.test_modifier `CONTROL state ->
      let (x, y) = config.item#w2i (GdkEvent.Button.x ev) (GdkEvent.Button.y ev) in
      config.item#affine_relative 
	(affine_rotate_around_point x y (~-. d_theta)) ;
  | 1 ->
      let x = GdkEvent.Button.x ev in
      let y = GdkEvent.Button.y ev in
      let (p_x, p_y) = config.item#parent#w2i x y in
      config.x <- p_x ;
      config.y <- p_y ;
      config.dragging <- true
  | 2 when Gdk.Convert.test_modifier `SHIFT state ->
      config.item#lower_to_bottom ()
  | 2 ->
      config.item#lower 1
  | 3 when Gdk.Convert.test_modifier `SHIFT state ->
      config.item#raise_to_top ()
  | 3 ->
      config.item#raise 1
  | _ -> ()

let item_event_motion config ev = 
  if config.dragging && Gdk.Convert.test_modifier `BUTTON1 (GdkEvent.Motion.state ev)
  then
    let x = GdkEvent.Motion.x ev in
    let y = GdkEvent.Motion.y ev in
    let (p_x, p_y) = config.item#parent#w2i x y in
    let aff = affine_invert 
      ( match config.item#xform with 
        | `AFFINE a -> a 
	| `IDENTITY -> affine_transl 0. 0. 
	| `TRANSL a -> affine_transl a.(0) a.(1) ) in
    let (apx, apy) = affine_apply aff p_x p_y in
    let (acx, acy) = affine_apply aff config.x config.y in
    config.item#move (apx -. acx) (apy -. acy) ;
    config.x <- p_x ;
    config.y <- p_y
    
let item_event config ev = 
  begin match ev with
  | `BUTTON_PRESS ev ->
      item_event_button_press config ev
  | `BUTTON_RELEASE _ ->
      config.dragging <- false
  | `MOTION_NOTIFY ev ->
      item_event_motion config ev
  | _ -> () end ;
  false

let setup_item (it : 'a #GnoCanvas.item) =
  let config = { 
    item = (it : 'a #GnoCanvas.item :> 'a GnoCanvas.item) ;
    dragging = false ;
    x = 0. ; y = 0. } in
  it#connect#event (item_event config)



let setup_div root =
  let grp = GnoCanvas.group root ~x:0. ~y:0. in
  GnoCanvas.rect grp 
    ~props:[ `X1 0.; `Y1 0.; `X2 600.; `Y2 450. ;
	     `OUTLINE_COLOR "black" ; `WIDTH_UNITS 4. ] ;
  List.map
    (fun p ->
      GnoCanvas.line grp
	~props:[ `FILL_COLOR "black"; `WIDTH_UNITS 4. ;
		 `POINTS p ])
    [ [| 0.; 150.; 600.; 150. |] ;
      [| 0.; 300.; 600.; 300. |] ;
      [| 200.; 0.; 200.; 450. |] ;
      [| 400.; 0.; 400.; 450. |] ; ] ;

  List.map
    (fun (text, pos) ->
      GnoCanvas.text grp
	~props:[ `TEXT text ;
		 `X (float (pos mod 3 * 200 + 100)) ;
		 `Y (float (pos / 3 * 150 + 5)) ;
		 `FONT "Sans 12" ; `ANCHOR `NORTH ;
		 `FILL_COLOR "black" ])
    [ ("Rectangles", 0);
      ("Ellipses", 1);
      ("Texts", 2);
      ("Images", 3);
      ("Lines", 4);
      ("Curves", 5);
      ("Arcs", 6);
      ("Polygons", 7);
      ("Widgets", 8); ] ;
  ()

let setup_rectangles root =
  setup_item
    (GnoCanvas.rect root
       ~props:[ `X1 20.; `Y1 30.; `X2 70.; `Y2 60.;
		`OUTLINE_COLOR "red" ; `WIDTH_PIXELS 8 ]) ;
  
  setup_item
    (GnoCanvas.rect root
       ~props:( [ `X1 90.; `Y1 40.; `X2 180.; `Y2 100.;
		  `OUTLINE_COLOR "black" ;
		  `WIDTH_UNITS 4. ] @
		if root#canvas#aa
		then [ `FILL_COLOR_RGBA (Int32.of_int 0x3cb37180) ]
		else [ `FILL_COLOR "mediumseagreen" ;
		       `FILL_STIPPLE (Gdk.Bitmap.create_from_data ~width:2 ~height:2 "\002\001")
		     ] )) ;
     
  setup_item
    (GnoCanvas.rect root
       ~props:[ `X1 10.; `Y1 80.; `X2 80.; `Y2 140.;
		`FILL_COLOR "steelblue" ])


let setup_ellipses root =
  setup_item
    (GnoCanvas.ellipse root
       ~props:[ `X1 220.; `Y1 30.; `X2 270.; `Y2 60. ;
		`OUTLINE_COLOR "goldenrod" ;
		`WIDTH_PIXELS 8 ]) ;
  setup_item
    (GnoCanvas.ellipse root
       ~props:[ `X1 290.; `Y1 40.; `X2 380.; `Y2 100. ;
		`FILL_COLOR "wheat" ;
		`OUTLINE_COLOR "midnightblue" ;
		`WIDTH_UNITS 4. ]) ;
  setup_item
    (GnoCanvas.ellipse root
       ~props:( [ `X1 210.; `Y1 80.; `X2 280.; `Y2 140.;
		  `OUTLINE_COLOR "black" ;
		  `WIDTH_PIXELS 0 ] @
		if root#canvas#aa
		then [ `FILL_COLOR_RGBA (Int32.of_int 0x5f9ea080) ]
		else [ `FILL_COLOR "cadetblue" ;
		       `FILL_STIPPLE (Gdk.Bitmap.create_from_data ~width:2 ~height:2 "\002\001")
		     ] ))

let make_anchor root ~x ~y =
  let grp = GnoCanvas.group ~x ~y root in
  setup_item grp ;
  GnoCanvas.rect grp
    ~props:[ `X1 (-2.); `Y1 (-2.); `X2 2.; `Y2 2. ;
	     `OUTLINE_COLOR "black" ; `WIDTH_PIXELS 0 ] ;
  grp

let setup_texts root =
  GnoCanvas.text (make_anchor root ~x:420. ~y:20.)
    ~props:([ `TEXT "Anchor NW" ;`ANCHOR `NW ; 
	      `X 0. ; `Y 0. ; `FONT "Sans Bold 24" ; ] @
	    if root#canvas#aa
	    then [ `FILL_COLOR_RGBA (Int32.of_int 0x0000ff80) ]
	    else [ `FILL_COLOR "blue" ;
		       `FILL_STIPPLE (Gdk.Bitmap.create_from_data ~width:2 ~height:2 "\002\001")
		 ] ) ;
  GnoCanvas.text (make_anchor root ~x:470. ~y:75.)
    ~props:[ `TEXT "Anchor center\nJustify center\nMultiline text" ;
	     `X 0. ; `Y 0. ; `FONT "Sans monospace bold 14" ;
	     `ANCHOR `CENTER ; `JUSTIFICATION `CENTER ;
	     `FILL_COLOR "firebrick" ] ;
	    
  GnoCanvas.text (make_anchor root ~x:590. ~y:140.)
    ~props:[ `TEXT "Clipped text\nClipped text\nClipped text\nClipped text\nClipped text\nClipped text" ;
	     `X 0. ; `Y 0. ; `FONT "Sans 12" ;
	     `ANCHOR `SE ; 
	     `CLIP true ; `CLIP_WIDTH 50. ; `CLIP_HEIGHT 55. ;
	     `X_OFFSET 10. ; `FILL_COLOR "darkgreen" ] ;
  ()
	     
let plant_flower root x y =
  let im = GdkPixbuf.from_file "flower.png" in
  setup_item
    (GnoCanvas.pixbuf root ~pixbuf:im ~x ~y 
       ~props:[ `ANCHOR `CENTER] ) ;
  ()

let setup_images root =
  let im = GdkPixbuf.from_file "toroid.png" in
  setup_item
    (GnoCanvas.pixbuf ~x:100. ~y:225. ~pixbuf:im 
       ~props:[ `ANCHOR `CENTER ] root) ;

  plant_flower root  20. 170. ;
  plant_flower root 180. 170. ;
  plant_flower root  20. 280. ;
  plant_flower root 180. 280.


let polish_diamond root =
  let grp = GnoCanvas.group ~x:270. ~y:230. root in
  setup_item grp ;
  let p = Array.make 4 0. in
  let vertices, radius = (10, 60.) in
  for i=0 to pred vertices do
    let a = 8. *. atan 1. *. (float i) /. (float vertices) in
    p.(0) <- radius *. cos a ;
    p.(1) <- radius *. sin a ;
    for j=i+1 to pred vertices do
      let a = 8. *. atan 1. *. (float j) /. (float vertices) in
      p.(2) <- radius *. cos a ;
      p.(3) <- radius *. sin a ;
      GnoCanvas.line grp
	~props:[ `POINTS p; `FILL_COLOR "black" ;
		 `WIDTH_UNITS 1. ; `CAP_STYLE `ROUND ] ;
      ()
    done
  done

let make_hilbert root =
  let scale = 7. in
  let hilbert = "urdrrulurulldluuruluurdrurddldrrruluurdrurddldrddlulldrdldrrurd" in 
  let points = Array.make (2 * (String.length hilbert + 1)) 0. in
  points.(0) <- 340. ; points.(1) <- 290. ;
  for i=1 to String.length hilbert do
    let (dx, dy) = 
      match hilbert.[pred i] with
      | 'd' -> (0., scale)
      | 'u' -> (0., ~-. scale)
      | 'l' -> (~-. scale, 0.)
      | 'r' -> (scale, 0.) 
      | _ -> failwith "pb" in
    points.(2 * i) <- points.(2 * (pred i)) +. dx ;
    points.(2 * i + 1) <- points.(2 * (pred i) + 1) +. dy
  done ;
  setup_item
    (GnoCanvas.line root
       ~props:( [ `POINTS points ; `WIDTH_UNITS 4. ;
		  `CAP_STYLE `PROJECTING ; `JOIN_STYLE `MITER ] @
		if root#canvas#aa
		then [ `FILL_COLOR_RGBA 0xff000080l ]
		else [ `FILL_COLOR "red" ;
		       `FILL_STIPPLE (Gdk.Bitmap.create_from_data ~width:2 ~height:2 "\002\001")
		     ] ) ) ;
  ()
	
let setup_lines root =
  polish_diamond root ;
  make_hilbert root ;
  let points = [| 340.; 170.; 340.; 230.; 390.; 230.; 390.; 170. |] in
  setup_item
    (GnoCanvas.line root
       ~props:[ `POINTS points ; `FILL_COLOR "midnightblue" ; `WIDTH_UNITS 3. ; 
		`FIRST_ARROWHEAD true ; `LAST_ARROWHEAD true ; 
		`ARROW_SHAPE_A 8. ; `ARROW_SHAPE_B 12. ; `ARROW_SHAPE_C 4. ]) ;

  let points = [| 356.; 180.; 374.; 220.; |] in
  setup_item
    (GnoCanvas.line root
       ~props:[ `POINTS points ; `FILL_COLOR "blue" ; `WIDTH_PIXELS 0 ; 
		`FIRST_ARROWHEAD true ; `LAST_ARROWHEAD true ; 
		`ARROW_SHAPE_A 6. ; `ARROW_SHAPE_B 6. ; `ARROW_SHAPE_C 4. ]) ;

  let points = [| 356.; 220.; 374.; 180.; |] in
  setup_item
    (GnoCanvas.line root
       ~props:[ `POINTS points ; `FILL_COLOR "blue" ; `WIDTH_PIXELS 0 ; 
		`FIRST_ARROWHEAD true ; `LAST_ARROWHEAD true ; 
		`ARROW_SHAPE_A 6. ; `ARROW_SHAPE_B 6. ; `ARROW_SHAPE_C 4. ]) ;
  ()

let setup_curves root =
  let p = GnomeCanvas.PathDef.new_path () in
  GnomeCanvas.PathDef.moveto p 500. 175. ;
  GnomeCanvas.PathDef.curveto p 550. 175. 550. 275. 500. 275. ;
  setup_item
    (GnoCanvas.bpath root
       ~props:[ `BPATH p ; `OUTLINE_COLOR "black" ; `WIDTH_PIXELS 4 ]) ;
  ()

let setup_polygons root =
  let points = [| 210. ; 320.; 210.; 380.; 260.; 350.; |] in
  setup_item
    (GnoCanvas.polygon ~points root
       ~props:( (`OUTLINE_COLOR "black") ::
		if root#canvas#aa
		then [ `FILL_COLOR_RGBA (Int32.of_int 0x0000ff80) ]
		else [ `FILL_COLOR "blue" ; 
		       `FILL_STIPPLE (Gdk.Bitmap.create_from_data ~width:2 ~height:2 "\002\001") ] )) ;
  let points = [|
	270.0; 330.0; 270.0; 430.0;
	390.0; 430.0; 390.0; 330.0;
	310.0; 330.0; 310.0; 390.0;
	350.0; 390.0; 350.0; 370.0;
	330.0; 370.0; 330.0; 350.0;
	370.0; 350.0; 370.0; 410.0;
	290.0; 410.0; 290.0; 330.0; |] in
  setup_item
    (GnoCanvas.polygon ~points root
       ~props:[ `FILL_COLOR "tan" ; `OUTLINE_COLOR "black" ; `WIDTH_UNITS 3. ]) ;
  ()


let setup_widgets root =
  let w = GButton.button ~label:"Hello world!" () in
  setup_item
    (GnoCanvas.widget root ~widget:w ~x:420. ~y:330.
       ~props:[ `ANCHOR `NW ; `SIZE_PIXELS false ;
		`WIDTH 100. ; `HEIGHT 40. ]) ;
  ()

let key_press (canvas : GnoCanvas.canvas) ev =
  let (x, y) = canvas#get_scroll_offsets in
  match GdkEvent.Key.keyval ev with
  | k when k = GdkKeysyms._Up -> canvas#scroll_to x (y-20) ; true
  | k when k = GdkKeysyms._Down -> canvas#scroll_to x (y+20) ; true
  | k when k = GdkKeysyms._Left -> canvas#scroll_to (x-10) y ; true
  | k when k = GdkKeysyms._Right -> canvas#scroll_to (x+10) y ; true
  | _ -> false

let focus canvas ev = 
  if GdkEvent.Focus.focus_in ev
  then prerr_endline "focus in"
  else prerr_endline "focus out" ;
  false

let create_canvas_primitives window ~aa =
  let vbox = GPack.vbox ~border_width:4 ~spacing:4 ~packing:window#add () in
  GMisc.label 
    ~text:"Drag an item with button 1.  Click button 2 on an item to lower it,\n\
           or button 3 to raise it.  Shift+click with buttons 2 or 3 to send\n\
           an item to the bottom or top, respectively.  Control+click with \n\
           button 1 or button 3 to rotate an item."
    ~packing:vbox#pack () ;
  let hbox = GPack.hbox ~spacing:4 ~packing:vbox#pack () in
  GtkBase.Widget.push_colormap (Gdk.Rgb.get_cmap ()) ;
  let canvas = GnoCanvas.canvas ~aa ~width:600 ~height:450 () in
  canvas#set_center_scroll_region false ;
  let root = canvas#root in
  setup_div root ;
  setup_rectangles root ;
  setup_ellipses root ;
  setup_texts root ;
  setup_images root ;
  setup_lines root ;
  setup_polygons root ;
  setup_curves root ;
  setup_widgets root ;  
  (* root#affine_relative [| 1.5; 0.; 0.; 0.7; 0.; 0.; |] ; *)
  GtkBase.Widget.pop_colormap () ;
  
  GMisc.label ~text:"Zoom:" ~packing:hbox#pack () ;
  let adj = GData.adjustment 
      ~value:1. ~lower:0.05 ~upper:5. 
      ~step_incr:0.05 ~page_incr:0.5 ~page_size:0.5 () in
  adj#connect#value_changed (zoom_changed canvas adj) ;
  let w = GEdit.spin_button ~adjustment:adj ~rate:0. ~digits:2 ~width:50 ~packing:hbox#pack () in
  let table = GPack.table ~rows:2 ~columns:2 ~row_spacings:4 ~col_spacings:4 ~packing:vbox#pack () in
  let frame = GBin.frame ~shadow_type:`IN () in
  table#attach ~left:0 ~right:1 ~top:0 ~bottom:1
    ~expand:`BOTH ~fill:`BOTH ~shrink:`BOTH ~xpadding:0 ~ypadding:0
    frame#coerce ;
  canvas#set_scroll_region 0. 0. 600. 450. ;
  frame#add canvas#coerce ;
  canvas#event#connect#after#key_press (key_press canvas) ;
  canvas#event#connect#enter_notify (fun _ -> canvas#misc#grab_focus () ; false) ;
  let w = GRange.scrollbar `HORIZONTAL ~adjustment:canvas#hadjustment () in
  table#attach ~left:0 ~right:1 ~top:1 ~bottom:2
    ~expand:`X ~fill:`BOTH ~shrink:`X ~xpadding:0 ~ypadding:0
    w#coerce ;
  let w = GRange.scrollbar `VERTICAL ~adjustment:canvas#vadjustment () in
  table#attach ~left:1 ~right:2 ~top:0 ~bottom:1
    ~expand:`Y ~fill:`BOTH ~shrink:`Y ~xpadding:0 ~ypadding:0 
    w#coerce ;
  canvas#misc#set_can_focus true ;
  canvas#misc#grab_focus ()


let main_1 () =
  let aa = 
    if Array.length Sys.argv > 1 
    then try bool_of_string Sys.argv.(1) 
         with Invalid_argument _ -> false
    else false in
  let window = GWindow.window () in
  create_canvas_primitives window ~aa ;
  window#connect#destroy ~callback:GMain.Main.quit ;
  window#show () ;
  GMain.Main.main ()

let _ = 
   main_1 ()


(* Local Variables: *)
(* compile-command: "ocamlopt -w s -i -I ../../src lablgtk.cmxa gtkInit.cmx lablgnomecanvas.cmxa canvas-primitives.ml" *)
(* End: *)
