(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(** The XML UI description for the popup menu. Here it is a strig in the program,
   it could also be a .xml file in $prefix/share/gnome-2.0/ui
*)
let xml_descr =
  String.concat "\n" [
  "<popup name=\"button3\">" ;
  "   <menuitem name=\"About LablGTK\" verb=\"Verb_LablGTK\" _label=\"About LablGTK ...\"" ;
  "             pixtype=\"stock\" pixname=\"gtk-help\"/>" ;
  "   <menuitem name=\"Background info\" verb=\"Verb_background\" _label=\"Background info\"/>" ;
  "</popup>" ;
]

(** The callback executed when the appropriate entry in the pop menu
   is selected.
*)
let popup_callback vrb =
  prerr_endline ("popup callback with verb " ^ vrb) ;
  let d = GWindow.message_dialog
      ~message:"LablGTK:\nblahblahblah\npatatipatata"
      ~message_type:`INFO
      ~buttons:GWindow.Buttons.close
      ~title:"About LablGTK"
      ~show:true () in
  d#connect#response (function `CLOSE | `DELETE_EVENT -> d#destroy ()) ;
  ()
  
(** A (regular) callback activated on GtkButton::clicked *)
let popup_dialog () =
  let d = GWindow.message_dialog
      ~message:"Yeah ! \\o/\nA GNOME panel applet writen in Caml !\nAin't it cool ?"
      ~message_type:`INFO
      ~buttons:GWindow.Buttons.close
      ~title:"applet in caml"
      ~show:true () in
  d#connect#response (function `CLOSE | `DELETE_EVENT -> d#destroy ()) ;
  ()

(** A trick so that a 3rd mouse button click on our button widget
   is ignored by the button and received by the applet widget. 
   The latter will then display the popup menu.
*)
let do_not_eat_button_press ev =
  if GdkEvent.Button.button ev <> 1
  then GtkSignal.stop_emit () ;
  false

(** Some dumb callbacks to test things a bit. *)
let background_info = function
  | `NO_BACKGROUND ->
      Format.eprintf "No background@."
  | `COLOR_BACKGROUND c ->
      Format.eprintf "Color backgound (%x, %x, %x)@."
	(Gdk.Color.red c) (Gdk.Color.blue c) (Gdk.Color.green c)
  | `PIXMAP_BACKGROUND p ->
      Format.eprintf "Pixmap background@."

let size_info s =
  Format.eprintf "Size change: %d@." s

let orient_info d =
  Format.eprintf "Orientation change: %s@." 
    (match d with | `UP -> "up" | `DOWN -> "down"
                  | `LEFT -> "left" | `RIGHT -> "right")



(** Our main `factory' callback. We are given a Panel.applet object that 
   we have to fill with widgets.
*)

let fill_applet (applet : Panel.applet) =
  applet#set_flags [ `HAS_HANDLE ; `EXPAND_MINOR ] ;

  let box = GPack.hbox ~packing:applet#add () in
  GMisc.image ~stock:(`STOCK "gnome-stock-about") ~packing:box#pack () ;
  let button =
    GButton.button 
      ~label:"LablGTK Applet"
      ~relief:`NONE
      ~packing:box#pack () in
  button#event#connect#button_press
    ~callback:do_not_eat_button_press ;
  button#connect#clicked popup_dialog ;
  
  let tips = GData.tooltips () in
  tips#set_tip ~text:"A sample applet written in Objective Caml" applet#coerce ;

  (* connecting this signal induces bonobo leaks, don't know why. *)
  (* applet#connect#change_background background_info ; *)
  applet#connect#change_size size_info ;
  applet#connect#change_orient orient_info ;

  applet#setup_menu 
    ~xml:xml_descr
    [ "Verb_LablGTK", popup_callback ;
      "Verb_background", (fun _ -> background_info applet#get_background) ] ;
  
  applet#misc#show () ;

  prerr_endline "filled applet"


(** Just make sure the panel do not think we're somebody else
   and call our applet-filling function
*)
let factory applet ~iid =
  prerr_endline "factory called" ;
  if iid <> "OAFIID:LablGTK_TestApplet"
  then false
  else (try fill_applet applet ; true with _ -> false)


(** The `main' of our executable is entirely handled by the library. 
   A return value of [false] either indicate that the factory could not
   register itself with the activation server, or that the shutdown process
   detected some resources leaks. Either way, there's hardly anything to do 
   about it.
*)
let _ =
  let res = 
    Panel.factory_main 
      ~iid:"OAFIID:LablGTK_TestApplet_Factory"
      factory in
  Printf.eprintf "applet registration/shutdown : %b\n" res
