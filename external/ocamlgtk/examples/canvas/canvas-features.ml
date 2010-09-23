(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

let pad = 4

let item_callback it (p1, p2) = function
  | `BUTTON_PRESS ev when GdkEvent.Button.button ev = 1 ->
        if it#parent#get_oid = p1#get_oid
	then it#reparent p2
	else it#reparent p1 ;
	true
  | _ -> false


let create_canvas_features window =
  let vbox = GPack.vbox ~border_width:pad ~spacing:pad ~packing:window#add () in
  GMisc.label 
    ~text:"Reparent test:  click on the items to switch them between parents" 
    ~packing:vbox#add () ;
  let align = GBin.alignment ~packing:vbox#add () in
  let frame = GBin.frame ~shadow_type:`IN ~packing:align#add () in

  let canvas = GnoCanvas.canvas ~width:400 ~height:200 ~packing:frame#add () in
  canvas#set_scroll_region 0. 0. 400. 200. ;

  let parent_1 = GnoCanvas.group canvas#root ~x:0. ~y:0. in
  GnoCanvas.rect parent_1
    ~props:[ `X1 0.; `Y1 0.; 
	     `X2 200.; `Y2 200.; 
	     `FILL_COLOR "tan" ] ;
  let parent_2 = GnoCanvas.group canvas#root ~x:200. ~y:0. in
  GnoCanvas.rect parent_2
    ~props:[ `X1 0.; `Y1 0.; 
	     `X2 200.; `Y2 200.; 
	     `FILL_COLOR "#204060" ] ;

  let item = GnoCanvas.ellipse parent_1
      ~props:[ `X1 10.; `Y1 10.; 
	       `X2 190.; `Y2 190.; 
	       `OUTLINE_COLOR "black" ;
	       `FILL_COLOR "mediumseagreen" ;
	       `WIDTH_UNITS 3. ] in
  item#connect#event (item_callback item (parent_1, parent_2)) ;
  
  let group = GnoCanvas.group parent_2 ~x:100. ~y:100. in
  GnoCanvas.ellipse group 
    ~props:[ `X1 (-50.); `Y1 (-50.); 
	     `X2 50.; `Y2 50.; 
	     `OUTLINE_COLOR "black" ;
	     `FILL_COLOR "wheat" ;
	     `WIDTH_UNITS 3. ] ;
  GnoCanvas.ellipse group 
    ~props:[ `X1 (-25.); `Y1 (-25.); 
	     `X2 25.; `Y2 25.; 
	     `FILL_COLOR "steelblue" ] ;
  group#connect#event (item_callback group (parent_1, parent_2)) ;
  
  vbox


let main_1 () =
  let window = GWindow.window () in
  let truc = create_canvas_features window in

  window#connect#destroy ~callback:GMain.Main.quit ;

  window#show () ;
  GMain.Main.main ()

let _ = 
   main_1 ()

(* Local Variables: *)
(* compile-command: "ocamlopt -w s -i -I ../../src lablgtk.cmxa gtkInit.cmx lablgnomecanvas.cmxa canvas-features.ml" *)
(* End: *)
