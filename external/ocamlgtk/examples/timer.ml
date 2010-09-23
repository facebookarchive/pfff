(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: timer.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

open StdLabels

let check_cache ~cond ~create ~destroy = function
    Some pm ->
      if cond pm then pm else begin
        destroy pm;
        create ()
      end
  | None -> create ()

class timer ?packing ?show () =
  let da = GMisc.drawing_area ~width:200 ~height:200 ?packing ?show () in
  let context = da#misc#create_pango_context in
  object (self)
    inherit GObj.widget_full da#as_widget
    val mutable talk = 25 * 60
    val mutable buffer = 5 * 60
    val mutable questions = 5 * 60
    val mutable start = 0.
    val mutable stop = 0.
    val mutable timer = None
    val mutable size = 0, 0
    val mutable pixmap = None
    method set_talk x = talk <- x * 60
    method set_buffer x = buffer <- x * 60
    method set_questions x = questions <- x * 60
    method private to_angle t =
      let total = float (talk + buffer + questions) in
      float t /. total *. 360.
    method draw =
      let current =
        if start = 0. then 0 else truncate (Unix.time () -. start) in
      let {Gtk.x=x0; y=y0; width=width; height=height} =
        da#misc#allocation in
      let size = (min width height) * 49 / 50 in
      let x = (width - size) / 2
      and y = (height - size) / 2 in
      let dr = check_cache pixmap
          ~cond:(fun pm -> pm#size = (width, height))
          ~destroy:(fun pm -> Gdk.Pixmap.destroy pm#pixmap)
          ~create:
          (fun () ->
            context#set_font_by_name ("sans " ^ string_of_int (size*2/13));
            GDraw.pixmap ~width ~height ~window:da ())
      in
      pixmap <- Some dr;
      dr#set_foreground `WHITE;
      dr#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
      let draw_arc ~color ~start ~stop =
        dr#set_foreground (`NAME color);
        dr#arc  ~x ~y ~width:size ~height:size ~filled:true
          ~start:(450. -. self#to_angle stop)
          ~angle:(self#to_angle (stop - start) +. 1.) ()
      in
      draw_arc ~color:"blue" ~start:(-60)
        ~stop:(min current (talk+buffer+questions));
      if current < talk then
        draw_arc ~color:"green" ~start:current ~stop:talk;
      if current < talk + buffer then
        draw_arc ~color:"yellow"
          ~start:(max talk current) ~stop:(talk+buffer);
      if current < talk + buffer + questions then
        draw_arc ~color:"red"
          ~start:(max (talk+buffer) current) ~stop:(talk+buffer+questions);
      dr#set_foreground `WHITE;
      let size' = size * 3 / 5 in
      dr#arc ~x:((width - size') / 2) ~y:((height - size') / 2)
        ~width:size' ~height:size' ~filled:true ();
      let layout = context#create_layout in
      Pango.Layout.set_text layout
        (Printf.sprintf "%02d:%02d" (current/60) (current mod 60));
      let (w,h) = Pango.Layout.get_pixel_size layout in
      dr#put_layout ~x:((width-w)/2) ~y:((height-h)/2) ~fore:`BLACK layout;
      (new GDraw.drawable da#misc#window)#put_pixmap ~x:0 ~y:0 dr#pixmap
    method start =
      self#stop;
      if start = 0. then start <- Unix.time ()
      else start <- start +. Unix.time () -. stop;
      stop <- 0.;
      timer <-
        Some(GMain.Timeout.add ~ms:1000 ~callback:(fun () -> self#draw; true));
      self#draw
    method stop =
      if stop = 0. then stop <- Unix.time ();
      match timer with None -> ()
      | Some id ->
          GMain.Timeout.remove id; timer <- None
    method reset =
      self#stop;
      start <- 0.;
      stop <- 0.;
      self#draw
    initializer
      da#event#connect#expose ~callback:(fun _ -> self#draw; true); ()
  end

let () =
  let w = GWindow.window () in
  w#connect#destroy ~callback:GMain.quit;
  let hbox = GPack.hbox ~packing:w#add () in
  let fr = GBin.frame ~border_width:3 ~shadow_type:`IN ~packing:hbox#add () in
  let timer = new timer ~packing:fr#add () in
  let vbox = GPack.vbox ~border_width:3 ~spacing:4 ~packing:hbox#pack () in
  let make_spin ~label ~value ~callback =
    GMisc.label ~text:label ~xalign:0. ~packing:vbox#pack ();
    let x = GEdit.spin_button ~digits:0 ~packing:vbox#pack () in
    x#adjustment#set_bounds ~lower:0. ~upper:999. ~step_incr:1. ();
    x#adjustment#set_value (float value);
    x#connect#value_changed ~callback:
      (fun () -> callback x#value_as_int; timer#draw);
    x
  in
  let talk = make_spin ~label:"Talk" ~value:25 ~callback:timer#set_talk
  and buffer = make_spin ~label:"Buffer" ~value:5 ~callback:timer#set_buffer
  and questions =
    make_spin ~label:"Questions" ~value:5 ~callback:timer#set_questions in
  let total =
    make_spin ~label:"Total" ~value:35 ~callback:
      (fun v ->
        talk#set_value
          (float (v - buffer#value_as_int - questions#value_as_int)))
  in
  let set_total () =
    total#set_value (talk#value +. buffer#value +. questions#value) in
  List.iter [talk;buffer;questions] ~f:
    (fun (x:GEdit.spin_button) ->
      ignore(x#connect#value_changed ~callback:set_total));
  let start = GButton.button ~label:"Start" ~packing:vbox#pack () in
  let stop = GButton.button ~label:"Stop" ~packing:vbox#pack () in
  let reset = GButton.button ~label:"Reset" ~packing:vbox#pack () in
  start#connect#clicked ~callback:(fun () -> timer#start);
  stop#connect#clicked ~callback:(fun () -> timer#stop);
  reset#connect#clicked ~callback:(fun () -> timer#reset);
  w#show ();
  GMain.main ()
