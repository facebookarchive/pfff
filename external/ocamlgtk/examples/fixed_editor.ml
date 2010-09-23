(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

open Gdk  
open Gtk
open GObj
open GMain

let dnd_source_window () =
  let window = GWindow.window ~position:`MOUSE () in
  let vbx = GPack.vbox ~border_width:10 ~packing:window#add ()
  in   
  let evb = GBin.event_box ~border_width:0 ~packing:vbx#add () in
  let frm = GBin.frame ~shadow_type:`OUT ~packing:evb#add () in
  let lbl = GMisc.label ~text:"hello" ~packing:frm#add () in
  let lbl2 = GMisc.label ~text:"drag from here!" ~packing:vbx#add () in
  let targets = [ { target = "STRING"; flags = []; info = 0} ] in
  begin
    window#show ();
    evb#drag#source_set targets ~modi:[`BUTTON1] ~actions:[`COPY];
    evb#drag#connect#data_get ~callback: begin
      fun _ sel ~info ~time ->
      	sel#return "hello! "
    end
  end

let corner_width  = 7  
let corner_height = 7

type drag_action_type =
    GB_DRAG_NONE
  | GB_MIDDLE
  | GB_TOP
  | GB_BOTTOM
  | GB_LEFT
  | GB_RIGHT
  | GB_TOP_LEFT
  | GB_TOP_RIGHT
  | GB_BOTTOM_LEFT
  | GB_BOTTOM_RIGHT

let get_position_in_widget w ~x ~y ~width ~height =
  if (x <= corner_width) then
    if (y <= corner_height) then
      GB_TOP_LEFT
    else if (y >= height-corner_width) then
      GB_BOTTOM_LEFT
    else GB_LEFT
  else if (x >= width-corner_width) then
    if (y <= corner_height) then
      GB_TOP_RIGHT
    else if (y >= height-corner_width) then
      GB_BOTTOM_RIGHT
    else GB_RIGHT
  else if (y <= corner_height) then
      GB_TOP
    else if (y >= height-corner_width) then
      GB_BOTTOM
    else GB_MIDDLE
    
class drag_info = object
  val mutable drag_action = GB_DRAG_NONE
  val mutable drag_offset = (0, 0)
  val mutable toimen      = (0, 0)
  val mutable drag_widget = None
  method drag_action = drag_action
  method drag_offset = drag_offset
  method toimen = toimen (* coord. of opposite corner *)
  method set_drag_widget (w : GObj.widget) = begin
    match drag_widget with
      None -> begin
    	GMain.Grab.add w;
    	drag_widget <- Some w;
	()
      end
    | Some w -> ()
  end
  method unset_drag_widget () = begin
    match drag_widget with
      Some w -> begin
    	GMain.Grab.remove w;
	drag_widget <- None;
	()
      end
    | None -> ()
  end
  method set_drag_offset ~x ~y = drag_offset <- (x, y)
  method set_drag_action (w : Gdk.window) ~x ~y =
    begin
      let (x0, y0) = Window.get_position w in
      let (width, height) = Drawable.get_size w in
      drag_action <- get_position_in_widget w ~x ~y ~width ~height;
      let (x1, y1) = (x0+width, y0+height) in
      toimen <-
	match drag_action with
	  GB_TOP_LEFT     -> (x1, y1)
      	| GB_BOTTOM_LEFT  -> (x1, y0)
      	| GB_TOP_RIGHT    -> (x0, y1)
      	| GB_BOTTOM_RIGHT -> (x0, y0)
	| GB_TOP          -> (x0, y1)
	| GB_BOTTOM       -> (x0, y0)
	| GB_LEFT	  -> (x1, y0)
	| GB_RIGHT	  -> (x0, y0)
	|  _              -> (-1, -1) 
    end
  method unset_drag_action () = drag_action <- GB_DRAG_NONE
end

    
let to_grid g x = x - (x mod g)
  
let to_grid2 g (x, y) = (to_grid g x, to_grid g y)

class fix_editor ~width ~height ~packing =
  let info = new drag_info in
  let fix = GPack.fixed ~has_window:true ~width ~height ~packing () in
  let _ = fix#misc#realize () in
  let fix_window = fix#misc#window in
  let fix_drawing = new GDraw.drawable fix_window in

  object (self)
    inherit GObj.widget fix#as_widget
    val mutable grid = 1
    method set_grid g =
      if (grid != g) then begin
      	let pix =
          GDraw.pixmap ~window:fix ~width:g ~height:g ~mask:true () in
	let c = fix#misc#style#bg `NORMAL in
	pix#set_foreground (`COLOR c);
	pix#rectangle ~filled:true ~x:0 ~y:0 ~width:g ~height:g ();
	pix#set_foreground `BLACK;
      	pix#point ~x:0 ~y:0;
      	Gdk.Window.set_back_pixmap fix_window (`PIXMAP pix#pixmap)
      end;
      grid <- g

    method new_child ~name ~x ~y ~width ~height ~callback =
      let evb = GBin.event_box ~border_width:0 ~packing:fix#add () in
      let lbl = GMisc.label ~text:name ~width ~height ~packing:evb#add () in
      evb#misc#realize ();
      fix#move evb#coerce ~x ~y;
      self#connect_signals ~ebox:evb ~widget:lbl#coerce ~callback;
      ()

    method private connect_signals
      ~ebox:(ebox : GBin.event_box) ~widget:(widget : widget) ~callback:cbfun =
      let drawing = new GDraw.drawable (ebox#misc#window) in
      let draw_id = ref None in
      let exps_id = ref None in
      let on_paint _ =
      	let (width, height) = Drawable.get_size (ebox#misc#window) in begin
      	  drawing#set_foreground `BLACK;
      	  drawing#rectangle ~filled:true ~x:0 ~y:0
	    ~width:corner_width ~height:corner_height ();
      	  drawing#rectangle ~filled:true ~x:(width-corner_width) ~y:0
	    ~width:corner_width ~height:corner_height ();
      	  drawing#rectangle ~filled:true
	    ~x:(width-corner_width)
	    ~y:(height-corner_height)
	    ~width:corner_width ~height:corner_height ();
      	  drawing#rectangle ~filled:true
	    ~x:0
	    ~y:(height-corner_height)
	    ~width:corner_width ~height:corner_height ();
      	  drawing#rectangle ~filled:false
	    ~x:0 ~y:0 ~width:(width-1) ~height:(height-1) ();
	end
      in
      ebox#event#connect#button_press ~callback:
      	begin fun ev -> 
	  let bx = int_of_float (GdkEvent.Button.x ev) in
	  let by = int_of_float (GdkEvent.Button.y ev) in
	  info#set_drag_action (ebox#misc#window) ~x:bx ~y:by;
	  info#set_drag_offset ~x:bx ~y:by;
	  true
      	end;
      ebox#event#connect#motion_notify ~callback:
      	begin fun ev ->
	  info#set_drag_widget ebox#coerce;
	  let action = info#drag_action in
	  let (mx, my) = fix#misc#pointer in
	  let (ox, oy) = info#drag_offset in
	  begin match action with
	    GB_MIDDLE ->
	      let (nx, ny) = to_grid2 grid (mx-ox, my-oy) in
	      fix#move ebox#coerce ~x:nx ~y:ny;
	      if cbfun ~x:nx ~y:ny ~width:(-2) ~height:(-2) then
	      	()
	      else (* should we undo ? *) ()
	  | GB_DRAG_NONE -> () (* do nothing *)
	  | GB_TOP_LEFT | GB_BOTTOM_LEFT
	  | GB_TOP_RIGHT | GB_BOTTOM_RIGHT ->
	      let (toi_x, toi_y) =  info#toimen in
	      let (mx, my) = to_grid2 grid (mx, my) in
	      let (lx, rx) =
	      	if mx<toi_x then (mx, toi_x) else (toi_x, mx) in
	      let (ty, by) =
	      	if my<toi_y then (my, toi_y) else (toi_y, my) in
	      let (w, h) = (rx-lx, by-ty) in
	      ebox#misc#set_size_request ~width:w ~height:h ();
              fix#move ebox#coerce ~x:lx ~y:ty;
	      if cbfun ~x:lx ~y:ty ~width:w ~height:h then
	      	()
	      else (* should we undo ? *) ()
	  | GB_TOP | GB_BOTTOM ->
	      let (lx, toi_y) = info#toimen in
	      let my = to_grid grid my in
	      let (ty, by) = if my<toi_y then (my, toi_y) else (toi_y, my) in
	      let h = by-ty in
	      fix#move ebox#coerce ~x:lx ~y:ty;
              ebox#misc#set_size_request ~height:h ();
	      if cbfun ~x:lx ~y:ty ~width:(-2) ~height:h then
	      	()
	      else (* should we undo ? *) ()
	  | GB_LEFT | GB_RIGHT ->
	      let (toi_x, ty) = info#toimen in
	      let mx = to_grid grid mx in
	      let (lx, rx) = if mx<toi_x then (mx, toi_x) else (toi_x, mx) in
	      let w = rx-lx in 
              fix#move ebox#coerce ~x:lx ~y:ty;
	      ebox#misc#set_size_request ~width:w ();
	      if cbfun ~x:lx ~y:ty ~width:w ~height:(-2) then
	      	()
	      else (* should we undo ? *) ()
	  end;
	  true
      	end;
      ebox#event#connect#button_release ~callback:
      	begin fun ev -> 
	  info#unset_drag_action ();
	  info#unset_drag_widget ();
	  true
      	end;
      exps_id := Some (ebox#event#connect#after#expose
                         ~callback:(fun _ -> on_paint(); false));
      (* draw_id := Some (ebox#misc#connect#draw ~callback:on_paint); *)
      ()
    initializer
      fix#drag#dest_set ~actions:[`COPY]
      	[ { target = "STRING"; flags = []; info = 0} ];
      fix#drag#connect#data_received ~callback: begin
	fun context ~x ~y data ~info ~time ->
	  let name = data#data in
	  let _ = self#new_child ~name ~x ~y ~width:32 ~height:32
	      ~callback:(fun ~x ~y ~width ~height -> true) in
(*		  Printf.printf "%s %d %d\n" (data#data) x y;
		  flush stdout; *)
	  context#finish ~success:true ~del:false ~time;
      end;
      ()
  end
    
(* the following is for test only *)
let window1 () =    
  let window = GWindow.window () in
  let _ = window#connect#destroy ~callback: Main.quit in
  let fix = new fix_editor ~width:640 ~height:480 ~packing:window#add in
  fix#set_grid 5;
  let setter = fix#new_child ~name:"hello" ~x:100 ~y:200 ~width:32 ~height:32
      ~callback:begin fun ~x ~y ~width ~height ->
	(* Printf.printf "name=%s, x=%d, y=%d, width=%d, height=%d\n"
	              "hello" x y width height;
	flush stdout; *)
	true
      end in
   window#show ();
  ()


    
let main () =
  window1 ();
  dnd_source_window ();
  Main.main ()
  
let _ = main ()

(* Todo
   
   change mouse cursor
   resize fixed itself
   remove_child
   (drag and) drop
   
*)
