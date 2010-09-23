(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

type state =
  | INIT
  | FIRST_PRESS
  | FIRST_RELEASE
  | SECOND_PRESS
  | FINISHED

class curve parent cb = object (self)
  val mutable state = INIT
  val points = Array.make 8 0.
  val item = 
    GnoCanvas.bpath parent
      ~props:[ `OUTLINE_COLOR "blue" ;
	       `WIDTH_PIXELS 5 ; 
	       `CAP_STYLE `ROUND ]

  method click ev =
    let x = GdkEvent.Button.x ev in
    let y = GdkEvent.Button.y ev in
    match GdkEvent.get_type ev with
    | `BUTTON_PRESS when state = INIT ->
	points.(0) <- x ;
	points.(1) <- y ;
	state <- FIRST_PRESS
    | `BUTTON_RELEASE when state = FIRST_PRESS ->
	points.(2) <- x ;
	points.(3) <- y ;
	let path = GnomeCanvas.PathDef.new_path () in
	GnomeCanvas.PathDef.moveto path points.(0) points.(1) ;
	GnomeCanvas.PathDef.lineto path points.(2) points.(3) ;
	item#set [ `BPATH path ] ;
	item#show () ; 
	state <- FIRST_RELEASE
    | `BUTTON_PRESS when state = FIRST_RELEASE ->
	points.(4) <- x ;
	points.(5) <- y ;
	let path = GnomeCanvas.PathDef.new_path () in
	GnomeCanvas.PathDef.moveto path points.(0) points.(1) ;
	GnomeCanvas.PathDef.curveto path 
	  points.(4) points.(5) 
	  points.(4) points.(5) 
	  points.(2) points.(3) ;
	item#set [ `BPATH path ] ;
	state <- SECOND_PRESS
    | `BUTTON_PRESS when state = SECOND_PRESS ->
	points.(6) <- x ;
	points.(7) <- y ;
	let path = GnomeCanvas.PathDef.new_path () in
	GnomeCanvas.PathDef.moveto path points.(0) points.(1) ;
	GnomeCanvas.PathDef.curveto path 
	  points.(4) points.(5) 
	  points.(6) points.(7) 
	  points.(2) points.(3) ;
	item#set [ `BPATH path ] ;
    	state <- FINISHED
    | _ -> ()

  method motion ev =
    let x = GdkEvent.Motion.x ev in
    let y = GdkEvent.Motion.y ev in
    if state = FIRST_PRESS
    then begin
      points.(2) <- x ;
      points.(3) <- y ;
      let path = GnomeCanvas.PathDef.new_path () in
      GnomeCanvas.PathDef.moveto path points.(0) points.(1) ;
      GnomeCanvas.PathDef.lineto path points.(2) points.(3) ;
      item#set [ `BPATH path ] ;
    end

  method is_not_complete = 
    state <> FINISHED

  method kill () = 
    item#destroy () ;
    state <- FINISHED

  initializer 
    let _ = item#connect#event (cb self) in
    ()
end

      
let item_event curve ev =
  match ev with
  | `BUTTON_PRESS ev ->
      if GdkEvent.Button.button ev = 1 &&
	Gdk.Convert.test_modifier `SHIFT (GdkEvent.Button.state ev)
      then (curve#kill () ; true)
      else false
  | _ -> false



let canvas_event curves root ev =
  match ev with
  | `BUTTON_PRESS ev when GdkEvent.Button.button ev = 1 ->
      let curve = 
	match !curves with
	| Some b when b#is_not_complete -> b
	| _ -> 
	    let c = new curve root item_event in
	    curves := Some c ; c
      in
      curve#click ev ;
      false
  | `BUTTON_RELEASE ev when GdkEvent.Button.button ev = 1 -> 
      begin match !curves with
      | Some b when b#is_not_complete -> 
	  b#click ev
      | _ -> ()
      end ; 
      false
  | `MOTION_NOTIFY ev -> 
      begin match !curves with
      | Some b when b#is_not_complete -> 
	  b#motion ev ; true
      | _ -> false
      end
  | _ -> false



let create_canvas ~aa cont =
  let frame = GBin.frame ~shadow_type:`IN ~packing:cont#add () in
  let canvas = GnoCanvas.canvas ~aa ~width:600 ~height:250 ~packing:frame#add () in
  canvas#set_scroll_region 0. 0. 600. 250. ;
  let r = GnoCanvas.rect canvas#root
      ~props:[ `OUTLINE_COLOR "black" ;
	       `FILL_COLOR "white" ;
	       `X1 0.; `Y1 0. ; `X2 600. ; `Y2 250. ] in
  let t = GnoCanvas.text canvas#root
      ~props:[ `TEXT (if aa then "AntiAlias" else "Non-AntiAlias") ;
	       `X 270. ; `Y 5. ;
	       `FONT "Sans 12" ;
	       `ANCHOR `NORTH ;
	       `FILL_COLOR "black" ] in
  r#connect#event (canvas_event (ref None) canvas#root)



let create_canvas_bezier_curve window =
  let vbox = GPack.vbox ~border_width:4 ~packing:window#add () in
  GMisc.label 
    ~text:"Drag a line with button 1. Then mark 2 control points with\n\
           button 1. Shift+click with button 1 to destroy the curve.\n"
    ~packing:vbox#add () ;
  create_canvas ~aa:false vbox ;
  create_canvas ~aa:true vbox



let main_1 () =
  let window = GWindow.window () in
  create_canvas_bezier_curve window ;
  window#connect#destroy ~callback:GMain.Main.quit ;
  window#show () ;
  GMain.Main.main ()

let _ = 
   main_1 ()



(* Local Variables: *)
(* compile-command: "ocamlopt -w s -i -I ../../src lablgtk.cmxa gtkInit.cmx lablgnomecanvas.cmxa canvas-curve.ml" *)
(* End: *)
