(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: canvas-arrowhead.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

type config = {
    mutable width   : int ;
    mutable shape_a : int ;
    mutable shape_b : int ;
    mutable shape_c : int ;
  }
type data = {
    big_arrow : GnoCanvas.line ;
    outline   : GnoCanvas.line ;
    width_drag_box     : GnoCanvas.rect ;
    shape_a_drag_box   : GnoCanvas.rect ;
    shape_b_c_drag_box : GnoCanvas.rect ;
    width_items   : GnoCanvas.line * GnoCanvas.text ;
    shape_a_items : GnoCanvas.line * GnoCanvas.text ;
    shape_b_items : GnoCanvas.line * GnoCanvas.text ;
    shape_c_items : GnoCanvas.line * GnoCanvas.text ;
    width_info    : GnoCanvas.text ;
    shape_a_info  : GnoCanvas.text ;
    shape_b_info  : GnoCanvas.text ;
    shape_c_info  : GnoCanvas.text ;
    samples : GnoCanvas.line list
  }

let global_data = ref None

let left = 50.
let right = 350.
let middle = 150.

let config = {
  width   = 2 ;
  shape_a = 8 ;
  shape_b = 10 ;
  shape_c = 3 ;
}

let set_dimension (arrow, text) ~x1 ~y1 ~x2 ~y2 ~tx ~ty dim =
  let points = [| x1; y1; x2; y2 |] in
  arrow#set [ `POINTS points ] ;
  text#set  [ `TEXT (string_of_int dim); `X tx; `Y ty]



let move_drag_box item ~x ~y =
  item#set [ `X1 (x -. 5.) ; `Y1 (y -. 5.) ;
	     `X2 (x +. 5.) ; `Y2 (y +. 5.) ; ]



let set_arrow_shape c =
  let d = match !global_data with
  | None -> failwith "argl"
  | Some v -> v in
  d.big_arrow#set [ `WIDTH_PIXELS (10 * c.width) ;
		    `ARROW_SHAPE_A (float c.shape_a *. 10.) ;
		    `ARROW_SHAPE_B (float c.shape_b *. 10.) ;
		    `ARROW_SHAPE_C (float c.shape_c *. 10.) ; ] ;
  let p = [| right -. 10. *. float c.shape_a ; middle ;
	     right -. 10. *. float c.shape_b ; 
	     middle -. 10. *. (float c.shape_c +. float c.width /. 2.) ;
	     right ; middle ;
	     right -. 10. *. float c.shape_b ; 
	     middle +. 10. *. (float c.shape_c +. float c.width /. 2.) ;
	     right -. 10. *. float c.shape_a ; middle ; |] in
  d.outline#set [ `POINTS p ] ;

  move_drag_box d.width_drag_box ~x:left ~y:(middle -. 10. *. float c.width /. 2.) ;
  move_drag_box d.shape_a_drag_box ~x:(right -. 10. *. float c.shape_a) ~y:middle ;
  move_drag_box d.shape_b_c_drag_box 
    ~x:(right -. 10. *. float c.shape_b) 
    ~y:(middle -. 10. *. (float c.shape_c +. float c.width /. 2.)) ;

  set_dimension d.width_items 
    ~x1:(left -. 10.) ~y1:(middle -. 10. *. (float c.width /. 2.))
    ~x2:(left -. 10.) ~y2:(middle +. 10. *. (float c.width /. 2.))
    ~tx:(left -. 15.) ~ty:middle 
    c.width ;

  set_dimension d.shape_a_items
    ~x1:(right -. 10. *. float c.shape_a) 
    ~y1:(middle +. 10. *. (float c.width /. 2. +. float c.shape_c) +. 10.)
    ~x2:right 
    ~y2:(middle +. 10. *. (float c.width /. 2. +. float c.shape_c) +. 10.)
    ~tx:(right -. 10. *. float c.shape_a /. 2.) 
    ~ty:(middle +. 10. *. (float c.width /. 2. +. float c.shape_c) +. 15.)
    c.shape_a ;

  set_dimension d.shape_b_items
    ~x1:(right -. 10. *. float c.shape_b) 
    ~y1:(middle +. 10. *. (float c.width /. 2. +. float c.shape_c) +. 35.)
    ~x2:right 
    ~y2:(middle +. 10. *. (float c.width /. 2. +. float c.shape_c) +. 35.)
    ~tx:(right -. 10. *. float c.shape_b /. 2.) 
    ~ty:(middle +. 10. *. (float c.width /. 2. +. float c.shape_c) +. 40.)
    c.shape_b ;

  set_dimension d.shape_c_items
    ~x1:(right +. 10.) ~y1:(middle -. 10. *. (float c.width /. 2.))
    ~x2:(right +. 10.) 
    ~y2:(middle -. 10. *. (float c.width /. 2. +. float c.shape_c))
    ~tx:(right +. 15.) 
    ~ty:(middle -. 10. *. (float (c.width + c.shape_c) /. 2.))
    c.shape_c ;
  
  d.width_info#set [ `TEXT (Printf.sprintf "width: %d" c.width) ] ;
  d.shape_a_info#set [ `TEXT (Printf.sprintf "arrow_shape_a: %d" c.shape_a) ] ;
  d.shape_b_info#set [ `TEXT (Printf.sprintf "arrow_shape_b: %d" c.shape_b) ] ;
  d.shape_c_info#set [ `TEXT (Printf.sprintf "arrow_shape_c: %d" c.shape_c) ] ;
			
  List.iter 
    (fun i -> i#set [ `WIDTH_PIXELS c.width ;
		      `ARROW_SHAPE_A (float c.shape_a) ;
		      `ARROW_SHAPE_B (float c.shape_b) ;
		      `ARROW_SHAPE_C (float c.shape_c) ; ] )
    d.samples

  
let highlight_box item ev = 
  begin match ev with
  | `ENTER_NOTIFY _ ->
      item#set [ `FILL_COLOR "red" ]
  | `LEAVE_NOTIFY ev ->
      let state = GdkEvent.Crossing.state ev in
      if not (Gdk.Convert.test_modifier `BUTTON1 state)
      then item#set [ `NO_FILL_COLOR ]
  | `BUTTON_PRESS ev ->
      let curs = Gdk.Cursor.create `FLEUR in
      item#grab [`POINTER_MOTION; `BUTTON_RELEASE] curs 
	(GdkEvent.Button.time ev)
  | `BUTTON_RELEASE ev ->
      item#ungrab (GdkEvent.Button.time ev)
  | _ -> ()
  end ;
  false

let create_drag_box grp cb =
  let box = GnoCanvas.rect 
      ~props:[ `NO_FILL_COLOR ; `OUTLINE_COLOR "black" ; `WIDTH_PIXELS 0 ]
      grp in
  let sigs = box#connect in
  sigs#event (highlight_box box) ;
  sigs#event cb ;
  box


let width_event c ev =
  begin match ev with
  | `MOTION_NOTIFY ev ->
      let state = GdkEvent.Motion.state ev in
      let width = int_of_float ((middle -. GdkEvent.Motion.y ev) /. 5.) in
      if Gdk.Convert.test_modifier `BUTTON1 state && width >= 0
      then begin
	c.width <- width ;
	set_arrow_shape c
      end
  | _ -> ()
  end ; false

let shape_a_event c ev =
  begin match ev with
  | `MOTION_NOTIFY ev ->
      let state = GdkEvent.Motion.state ev in
      let shape_a = int_of_float ((right -. GdkEvent.Motion.x ev) /. 10.) in
      if Gdk.Convert.test_modifier `BUTTON1 state && 
	0 <= shape_a && shape_a <= 30
      then begin
	c.shape_a <- shape_a ;
	set_arrow_shape c
      end
  | _ -> ()
  end ; false

let shape_b_c_event c ev =
  begin match ev with
  | `MOTION_NOTIFY ev ->
      let state = GdkEvent.Motion.state ev in
      let change = ref false in
      let shape_b = int_of_float ((right -. GdkEvent.Motion.x ev) /. 10.) in
      let shape_c = 
	int_of_float (((middle -. 5. *. float c.width) -.
			 (GdkEvent.Motion.y ev)) /. 10.) in
      if Gdk.Convert.test_modifier `BUTTON1 state
      then begin
	if 0 <= shape_b && shape_b <= 30
	then begin
	  c.shape_b <- shape_b ;
	  change := true
	end ;
	if 0 <= shape_c
	then begin
	  c.shape_c <- shape_c ;
	  change := true
	end ;
	if !change then set_arrow_shape c
      end
  | _ -> ()
  end ; false

let create_dimension grp anchor =
  let a = 
    GnoCanvas.line 
      ~props:[ `FILL_COLOR "black" ;
	       `FIRST_ARROWHEAD true ;
	       `LAST_ARROWHEAD true ;
	       `ARROW_SHAPE_A 5. ;
	       `ARROW_SHAPE_B 5. ;
	       `ARROW_SHAPE_C 3. ; ] 
      grp in
  let t = GnoCanvas.text 
      ~props:[ `FILL_COLOR "black" ;
	       `FONT "Sans 12" ;
	       `ANCHOR anchor ] grp in
  (a, t)
  

let create_info grp ~x ~y =
  GnoCanvas.text
    ~props:[ `X x; `Y y;
	     `FILL_COLOR "black" ;
	     `FONT "Sans 14" ;
	     `ANCHOR `NW ]
    grp


let create_sample_arrow grp p =
  GnoCanvas.line
    ~props:[ `POINTS p ; `FILL_COLOR "black" ;
	     `FIRST_ARROWHEAD true ; `LAST_ARROWHEAD true ]
    grp


let create_canvas_arrowhead window =
  let vbox = GPack.vbox ~border_width:4 ~packing:window#add () in
  GMisc.label
    ~text:"This demo allows you to edit arrowhead shapes.  Drag the little boxes\n\
	   to change the shape of the line and its arrowhead.  You can see the\n\
           arrows at their normal scale on the right hand side of the window."
    ~packing:vbox#add () ;
  let align = GBin.alignment ~packing:vbox#add () in
  let frame = GBin.frame ~shadow_type:`IN ~packing:align#add () in

  let canvas = GnoCanvas.canvas ~width:500 ~height:350 ~packing:frame#add () in
  canvas#set_scroll_region 0. 0. 500. 350. ;
  let root = canvas#root in

  let p = [| left; middle; right; middle |] in
  let big_arrow = GnoCanvas.line root
      ~props:[ `POINTS p ; `FILL_COLOR "mediumseagreen" ;
	       `WIDTH_PIXELS (config.width * 10) ;
	       `LAST_ARROWHEAD true ] in
  let outline = GnoCanvas.line root
      ~props:[ `FILL_COLOR "black" ;
	       `CAP_STYLE `ROUND ; `JOIN_STYLE `ROUND ;
	       `WIDTH_PIXELS 2; ] in
  
  let width_drag_box = create_drag_box root (width_event config) in
  let shape_a_drag_box = create_drag_box root (shape_a_event config) in
  let shape_b_c_drag_box = create_drag_box root (shape_b_c_event config) in

  let width_items = create_dimension root `EAST in
  let shape_a_items = create_dimension root `NORTH in
  let shape_b_items = create_dimension root `NORTH in
  let shape_c_items = create_dimension root `WEST in

  let width_info = create_info root ~x:left ~y:260. in
  let shape_a_info = create_info root ~x:left ~y:280. in
  let shape_b_info = create_info root ~x:left ~y:300. in
  let shape_c_info = create_info root ~x:left ~y:320. in

  let p = [| right +. 50.; 0.; right +. 50.; 1000. |] in
  GnoCanvas.line root
    ~props:[ `POINTS p; `FILL_COLOR "black" ;
	     `WIDTH_PIXELS 2 ] ;

  let samples = 
    List.map (create_sample_arrow root)
      [ [| right +. 100.; 30.; right +. 100. ; middle -. 30. |] ;
	[| right +. 70. ; middle; right +. 130. ; middle |] ;
	[| right +. 70. ; middle +. 30. ; right +. 130. ; middle +. 120. |] ]
  in
  
  global_data := Some
      { big_arrow = big_arrow ;
	outline = outline ;
	width_drag_box = width_drag_box ;
	shape_a_drag_box = shape_a_drag_box ;
	shape_b_c_drag_box = shape_b_c_drag_box ;
	width_items = width_items ;
	shape_a_items = shape_a_items ;
	shape_b_items = shape_b_items ;
	shape_c_items = shape_c_items ;
	width_info = width_info ;
	shape_a_info = shape_a_info ;
	shape_b_info = shape_b_info ;
	shape_c_info = shape_c_info ;
	samples = samples ; } ;

  set_arrow_shape config




let main_1 () =
  let window = GWindow.window () in

  create_canvas_arrowhead window ;

  window#connect#destroy ~callback:GMain.Main.quit ;

  window#show () ;
  GMain.Main.main ()

let _ = 
   main_1 ()



(* Local Variables: *)
(* compile-command: "ocamlopt -w s -i -I ../../src lablgtk.cmxa gtkInit.cmx lablgnomecanvas.cmxa canvas-arrowhead.ml" *)
(* End: *)
