(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* this is a translation in Caml of the gtk+ example testdnd.c  *)


open Gaux
open Gtk
open GObj
open GMain

(* GtkThread.start() *)

let drag_icon_xpm = [|
"36 48 9 1";
" 	c None";
".	c #020204";
"+	c #8F8F90";
"@	c #D3D3D2";
"#	c #AEAEAC";
"$	c #ECECEC";
"%	c #A2A2A4";
"&	c #FEFEFC";
"*	c #BEBEBC";
"               .....................";
"              ..&&&&&&&&&&&&&&&&&&&.";
"             ...&&&&&&&&&&&&&&&&&&&.";
"            ..&.&&&&&&&&&&&&&&&&&&&.";
"           ..&&.&&&&&&&&&&&&&&&&&&&.";
"          ..&&&.&&&&&&&&&&&&&&&&&&&.";
"         ..&&&&.&&&&&&&&&&&&&&&&&&&.";
"        ..&&&&&.&&&@&&&&&&&&&&&&&&&.";
"       ..&&&&&&.*$%$+$&&&&&&&&&&&&&.";
"      ..&&&&&&&.%$%$+&&&&&&&&&&&&&&.";
"     ..&&&&&&&&.#&#@$&&&&&&&&&&&&&&.";
"    ..&&&&&&&&&.#$**#$&&&&&&&&&&&&&.";
"   ..&&&&&&&&&&.&@%&%$&&&&&&&&&&&&&.";
"  ..&&&&&&&&&&&.&&&&&&&&&&&&&&&&&&&.";
" ..&&&&&&&&&&&&.&&&&&&&&&&&&&&&&&&&.";
"................&$@&&&@&&&&&&&&&&&&.";
".&&&&&&&+&&#@%#+@#@*$%$+$&&&&&&&&&&.";
".&&&&&&&+&&#@#@&&@*%$%$+&&&&&&&&&&&.";
".&&&&&&&+&$%&#@&#@@#&#@$&&&&&&&&&&&.";
".&&&&&&@#@@$&*@&@#@#$**#$&&&&&&&&&&.";
".&&&&&&&&&&&&&&&&&&&@%&%$&&&&&&&&&&.";
".&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&.";
".&&&&&&&&$#@@$&&&&&&&&&&&&&&&&&&&&&.";
".&&&&&&&&&+&$+&$&@&$@&&$@&&&&&&&&&&.";
".&&&&&&&&&+&&#@%#+@#@*$%&+$&&&&&&&&.";
".&&&&&&&&&+&&#@#@&&@*%$%$+&&&&&&&&&.";
".&&&&&&&&&+&$%&#@&#@@#&#@$&&&&&&&&&.";
".&&&&&&&&@#@@$&*@&@#@#$#*#$&&&&&&&&.";
".&&&&&&&&&&&&&&&&&&&&&$%&%$&&&&&&&&.";
".&&&&&&&&&&$#@@$&&&&&&&&&&&&&&&&&&&.";
".&&&&&&&&&&&+&$%&$$@&$@&&$@&&&&&&&&.";
".&&&&&&&&&&&+&&#@%#+@#@*$%$+$&&&&&&.";
".&&&&&&&&&&&+&&#@#@&&@*#$%$+&&&&&&&.";
".&&&&&&&&&&&+&$+&*@&#@@#&#@$&&&&&&&.";
".&&&&&&&&&&$%@@&&*@&@#@#$#*#&&&&&&&.";
".&&&&&&&&&&&&&&&&&&&&&&&$%&%$&&&&&&.";
".&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&.";
".&&&&&&&&&&&&&&$#@@$&&&&&&&&&&&&&&&.";
".&&&&&&&&&&&&&&&+&$%&$$@&$@&&$@&&&&.";
".&&&&&&&&&&&&&&&+&&#@%#+@#@*$%$+$&&.";
".&&&&&&&&&&&&&&&+&&#@#@&&@*#$%$+&&&.";
".&&&&&&&&&&&&&&&+&$+&*@&#@@#&#@$&&&.";
".&&&&&&&&&&&&&&$%@@&&*@&@#@#$#*#&&&.";
".&&&&&&&&&&&&&&&&&&&&&&&&&&&$%&%$&&.";
".&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&.";
".&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&.";
".&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&.";
"...................................." |]



let trashcan_closed_xpm = [|
"64 80 17 1";
" 	c None";
".	c #030304";
"+	c #5A5A5C";
"@	c #323231";
"#	c #888888";
"$	c #1E1E1F";
"%	c #767677";
"&	c #494949";
"*	c #9E9E9C";
"=	c #111111";
"-	c #3C3C3D";
";	c #6B6B6B";
">	c #949494";
",	c #282828";
"'	c #808080";
")	c #545454";
"!	c #AEAEAC";
"                                                                ";
"                                                                ";
"                                                                ";
"                                                                ";
"                                                                ";
"                                                                ";
"                                                                ";
"                                                                ";
"                                                                ";
"                                                                ";
"                                                                ";
"                                                                ";
"                                                                ";
"                                                                ";
"                                                                ";
"                                                                ";
"                                                                ";
"                                                                ";
"                                                                ";
"                                                                ";
"                                                                ";
"                                                                ";
"                       ==......=$$...===                        ";
"                 ..$------)+++++++++++++@$$...                  ";
"             ..=@@-------&+++++++++++++++++++-....              ";
"          =.$$@@@-&&)++++)-,$$$$=@@&+++++++++++++,..$           ";
"         .$$$$@@&+++++++&$$$@@@@-&,$,-++++++++++;;;&..          ";
"        $$$$,@--&++++++&$$)++++++++-,$&++++++;%%'%%;;$@         ";
"       .-@@-@-&++++++++-@++++++++++++,-++++++;''%;;;%*-$        ";
"       +------++++++++++++++++++++++++++++++;;%%%;;##*!.        ";
"        =+----+++++++++++++++++++++++;;;;;;;;;;;;%'>>).         ";
"         .=)&+++++++++++++++++;;;;;;;;;;;;;;%''>>#>#@.          ";
"          =..=&++++++++++++;;;;;;;;;;;;;%###>>###+%==           ";
"           .&....=-+++++%;;####''''''''''##'%%%)..#.            ";
"           .+-++@....=,+%#####'%%%%%%%%%;@$-@-@*++!.            ";
"           .+-++-+++-&-@$$=$=......$,,,@;&)+!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           =+-++-+++-+++++++++!++++!++++!+++!++!+++=            ";
"            $.++-+++-+++++++++!++++!++++!+++!++!+.$             ";
"              =.++++++++++++++!++++!++++!+++!++.=               ";
"                 $..+++++++++++++++!++++++...$                  ";
"                      $$=.............=$$                       ";
"                                                                ";
"                                                                ";
"                                                                ";
"                                                                ";
"                                                                ";
"                                                                ";
"                                                                ";
"                                                                ";
"                                                                "  |]

let trashcan_open_xpm = [|
"64 80 17 1";
" 	c None";
".	c #030304";
"+	c #5A5A5C";
"@	c #323231";
"#	c #888888";
"$	c #1E1E1F";
"%	c #767677";
"&	c #494949";
"*	c #9E9E9C";
"=	c #111111";
"-	c #3C3C3D";
";	c #6B6B6B";
">	c #949494";
",	c #282828";
"'	c #808080";
")	c #545454";
"!	c #AEAEAC";
"                                                                ";
"                                                                ";
"                                                                ";
"                                                                ";
"                                                                ";
"                                                                ";
"                                      .=.==.,@                  ";
"                                   ==.,@-&&&)-=                 ";
"                                 .$@,&++;;;%>*-                 ";
"                               $,-+)+++%%;;'#+.                 ";
"                            =---+++++;%%%;%##@.                 ";
"                           @)++++++++;%%%%'#%$                  ";
"                         $&++++++++++;%%;%##@=                  ";
"                       ,-++++)+++++++;;;'#%)                    ";
"                      @+++&&--&)++++;;%'#'-.                    ";
"                    ,&++-@@,,,,-)++;;;'>'+,                     ";
"                  =-++&@$@&&&&-&+;;;%##%+@                      ";
"                =,)+)-,@@&+++++;;;;%##%&@                       ";
"               @--&&,,@&)++++++;;;;'#)@                         ";
"              ---&)-,@)+++++++;;;%''+,                          ";
"            $--&)+&$-+++++++;;;%%'';-                           ";
"           .,-&+++-$&++++++;;;%''%&=                            ";
"          $,-&)++)-@++++++;;%''%),                              ";
"         =,@&)++++&&+++++;%'''+$@&++++++                        ";
"        .$@-++++++++++++;'#';,........=$@&++++                  ";
"       =$@@&)+++++++++++'##-.................=&++               ";
"      .$$@-&)+++++++++;%#+$.....................=)+             ";
"      $$,@-)+++++++++;%;@=........................,+            ";
"     .$$@@-++++++++)-)@=............................            ";
"     $,@---)++++&)@===............................,.            ";
"    $-@---&)))-$$=..............................=)!.            ";
"     --&-&&,,$=,==...........................=&+++!.            ";
"      =,=$..=$+)+++++&@$=.............=$@&+++++!++!.            ";
"           .)-++-+++++++++++++++++++++++++++!++!++!.            ";
"           .+-++-+++++++++++++++++++++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!+++!!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           .+-++-+++-+++++++++!++++!++++!+++!++!++!.            ";
"           =+-++-+++-+++++++++!++++!++++!+++!++!+++=            ";
"            $.++-+++-+++++++++!++++!++++!+++!++!+.$             ";
"              =.++++++++++++++!++++!++++!+++!++.=               ";
"                 $..+++++++++++++++!++++++...$                  ";
"                      $$==...........==$$                       ";
"                                                                ";
"                                                                ";
"                                                                ";
"                                                                ";
"                                                                ";
"                                                                ";
"                                                                ";
"                                                                ";
"                                                                "  |]

let window = GWindow.window ~title:"DnD Test" ()
let _ = window#misc#realize ()

let drag_icon =
  GDraw.pixmap_from_xpm_d ~data:drag_icon_xpm ~window ()

let trashcan_open =
  GDraw.pixmap_from_xpm_d ~data:trashcan_open_xpm ~window ()

let trashcan_closed =
  GDraw.pixmap_from_xpm_d ~data:trashcan_closed_xpm ~window ()

let targets = [
  { target = "STRING"; flags = []; info = 0};
  { target = "text/plain"; flags = []; info = 0};
  { target = "text/uri-list"; flags = []; info = 2};
  { target = "application/x-rootwin-drop"; flags = []; info = 1}
]

class drag_handler = object
  method private beginning (_ : drag_context) = ()
  method private data_delete (_ : drag_context) = ()
  method private data_get (_ : drag_context) (_ : selection_context)
      ~(info : int) ~(time : int32) = ()
  method private data_received (_ : drag_context) ~(x : int) ~(y : int)
      (_ : selection_data) ~(info : int) ~(time : int32) = ()
  method private drop (_ : drag_context) ~(x : int) ~(y : int)
      ~(time : int32) = false
  method private ending (_ : drag_context) = ()
  method private leave (_ : drag_context) ~(time : int32) = ()
  method private motion (_ : drag_context) ~(x : int) ~(y : int)
      ~(time : int32) = false
end


class target_drag ?packing ?show () =
  let pixmap = GMisc.pixmap trashcan_closed ?packing ?show () in
object (self)
  inherit widget pixmap#as_widget
  inherit drag_handler
  val mutable have_drag = false

  method leave _ ~time =
    print_endline "leave"; flush stdout;
    have_drag <- false;
    pixmap#set_pixmap trashcan_closed

  method motion context ~x ~y ~time =
    if not have_drag then begin
      have_drag <- true;
      pixmap#set_pixmap trashcan_open
    end;
    let source_typename =
      try
	context#source_widget#misc#get_type
      with Gpointer.Null -> "unknown"
    in
    Printf.printf "motion, source %s\n" source_typename; flush stdout;
    context#status (Some context#suggested_action) ~time;
    true

  method drop context ~x ~y ~time =
    prerr_endline "drop"; flush stdout;
    have_drag <- false;
    pixmap#set_pixmap trashcan_closed;
    match context#targets with
    | [] -> false
    | d :: _ -> pixmap#drag#get_data ~target:d ~time context; true

  method data_received context ~x ~y data ~info ~time =
    if data#format = 8 then begin
      Printf.printf "Received \"%s\" in trashcan\n" data#data;
      flush stdout;
      context#finish ~success:true ~del:false ~time
    end
    else context#finish ~success:false ~del:false ~time

  initializer
    pixmap#drag#dest_set targets ~actions:[`COPY;`MOVE];
    pixmap#drag#connect#leave ~callback:self#leave;
    pixmap#drag#connect#motion ~callback:self#motion;
    pixmap#drag#connect#drop ~callback:self#drop;
    pixmap#drag#connect#data_received ~callback:self#data_received;
    ()
end

class label_drag ?packing ?show () =
  let label = GMisc.label ~text:"Drop Here\n" ?packing ?show () in
object (self)
  inherit widget label#as_widget
  inherit drag_handler
  method data_received context ~x ~y data ~info ~time =
    if data#format = 8 then  begin
      Printf.printf "Received \"%s\" in label\n" data#data;
      flush stdout;
      context#finish ~success:true ~del:false ~time
    end
    else context#finish ~success:false ~del:false ~time

  initializer
    label#drag#dest_set targets ~actions:[`COPY; `MOVE ];
    label#drag#connect#data_received ~callback:self#data_received;
    ()
end

class source_drag ?packing ?show () =
  let button = GButton.button ~label:"Drag Here\n" ?packing ?show () in
object (self)
  inherit widget button#as_widget
  inherit drag_handler
  method data_get _ sel ~info ~time =
    if info = 1 then begin
      print_endline "I was dropped on the rootwin"; flush stdout
    end
    else if info = 2 then
      sel#return "file:///home/otaylor/images/weave.png"
    else
      sel#return "I'm Data!"

  method data_delete _ =
    print_endline "Delete the data!"; flush stdout

  initializer
    button#drag#source_set targets
      ~modi:[`BUTTON1; `BUTTON3 ] ~actions:[`COPY; `MOVE ];
    button#drag#source_set_icon drag_icon;
    button#drag#connect#data_get ~callback:self#data_get;
    button#drag#connect#data_delete ~callback:self#data_delete;
    ()
end

class popup () = object (self)
  inherit drag_handler
  val mutable popup_window = (None : #GWindow.window option)
  val mutable popped_up = false
  val mutable in_popup = false
  val mutable popdown_timer = None
  val mutable popup_timer = None

  method timer = popup_timer
  method remove_timer () =
    may popup_timer
      ~f:(fun pdt -> Timeout.remove pdt; popup_timer <- None)
  method add_timer time ~callback =
    popup_timer <- Some (Timeout.add ~ms:time ~callback)

  method popdown () =
    popdown_timer <- None;
    may popup_window ~f:(fun w -> w#misc#hide ());
    popped_up <- false;
    false

  method motion (_ : drag_context) ~x ~y ~time =
    if not in_popup then begin
      in_popup <- true;
      may popdown_timer ~f:
	begin fun pdt ->
	  print_endline "removed popdown"; flush stdout;
	  Timeout.remove pdt;
	  popdown_timer <- None
	end
    end;
    true

  method leave (_ : drag_context) ~time =
    if in_popup then begin
      in_popup <- false;
      if popdown_timer = None then begin
	print_endline "added popdown"; flush stdout;
	popdown_timer <- Some (Timeout.add ~ms:500 ~callback:self#popdown)
      end
    end

  method popup () =
    if not popped_up then begin
      if popup_window = None then begin
	let w = GWindow.window ~kind:`POPUP ~position:`MOUSE () in
	popup_window <- Some w;
	let table = GPack.table ~rows:3 ~columns:3 ~packing:w#add () in
	for i = 0 to 2 do
	  for j = 0 to 2 do
	    let button =
	      GButton.button ~label:(string_of_int i ^ "," ^ string_of_int j)
		~packing:(table#attach ~left:i ~top:j ~expand:`BOTH) ()
	    in
	    button#drag#dest_set targets ~actions:[`COPY; `MOVE ];
	    button#drag#connect#motion ~callback:self#motion;
	    button#drag#connect#leave ~callback:self#leave;
	  done
	done
      end;
      may popup_window ~f:(fun w -> w#show ());
      popped_up <- true
    end;
    popdown_timer <- Some (Timeout.add ~ms:500 ~callback:self#popdown);
    print_endline "added popdown"; flush stdout;
    self#remove_timer ();
    false
end

class popsite ?packing ?show () =
  let label = GMisc.label ~text:"Popup\n" ?packing ?show ()
  and popup = new popup () in
object (self)
  inherit widget label#as_widget
  inherit drag_handler
  method motion _ ~x ~y ~time =
    if popup#timer = None then begin
      print_endline "added popdown"; flush stdout;
      popup#add_timer 500 ~callback:popup#popup
    end;
    true

  method leave _ ~time =
    popup#remove_timer ()

  initializer
    label#drag#dest_set targets ~actions:[`COPY; `MOVE ];
    label#drag#connect#motion ~callback:self#motion;
    label#drag#connect#leave ~callback:self#leave;
    ()
end

let main () =
  window#connect#destroy ~callback: Main.quit;
  let table = GPack.table ~rows:2 ~columns:2 ~packing:window#add () in
  let attach = table#attach ~expand:`BOTH in
  new label_drag ~packing:(attach ~left:0 ~top:0) ();
  new target_drag ~packing:(attach ~left:1 ~top:0) ();
  new source_drag ~packing:(attach ~left:0 ~top:1) ();
  new popsite ~packing:(attach ~left:1 ~top:1) ();

  window#show ();
  Main.main ()

let _ =
  main ()
