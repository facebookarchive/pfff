(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

let ui_info = "<ui>\
  <menubar name='MenuBar'>\
    <menu action='FileMenu'>\
      <menuitem action='New'/>\
      <menuitem action='Open'/>\
      <menuitem action='Save'/>\
      <menuitem action='SaveAs'/>\
      <separator/>\
      <menuitem action='Quit'/>\
    </menu>\
    <menu action='PreferencesMenu'>\
      <menu action='ColorMenu'>\
	<menuitem action='Red'/>\
	<menuitem action='Green'/>\
	<menuitem action='Blue'/>\
      </menu>\
      <menu action='ShapeMenu'>\
        <menuitem action='Square'/>\
        <menuitem action='Rectangle'/>\
        <menuitem action='Oval'/>\
      </menu>\
      <menuitem action='Bold'/>\
    </menu>\
    <menu action='HelpMenu'>\
      <menuitem action='About'/>\
    </menu>\
  </menubar>\
  <toolbar name='ToolBar'>\
    <toolitem action='Open'/>\
    <toolitem action='Quit'/>\
    <separator/>\
    <toolitem action='Logo'/>\
  </toolbar>\
</ui>"

let activ_action ac =
  Printf.printf "Action '%s' activated\n" ac#name ;
  flush stdout


let setup_stock () = 
  let id = "demo-gtk-logo" in
  let logo = { GtkStock.stock_id = id ;
	       GtkStock.label = "_GTK!" ;
	       GtkStock.modifier = [] ;
	       GtkStock.keyval= 0 ; } in
  GtkStock.Item.add logo ;
  if Sys.file_exists "/usr/share/gtk-2.0/demo/gtk-logo-rgb.gif"
  then begin
    let pb = GdkPixbuf.from_file "/usr/share/gtk-2.0/demo/gtk-logo-rgb.gif" in
    let pb = GdkPixbuf.add_alpha ~transparent:(0xff, 0xff, 0xff) pb in
    GtkStock.make_icon_factory ~default:true
      ~icons:[ `STOCK id, GtkStock.make_icon_set ~pixbuf:pb [] ] () ;
    ()
  end


let setup_ui window = 
  let a = GAction.add_action in
  let ta = GAction.add_toggle_action in
  let radio = GAction.group_radio_actions in
  let ra = GAction.add_radio_action in

  let actions = GAction.action_group ~name:"Actions" () in
  GAction.add_actions actions
    [ a "FileMenu" ~label:"_File" ;
      a "PreferencesMenu" ~label:"_Preferences" ;
      a "ColorMenu" ~label:"_Color" ;
      a "ShapeMenu" ~label:"_Shape" ;
      a "HelpMenu" ~label:"_Help" ;

      a "New" ~stock:`NEW ~tooltip:"Create a new file"
	~callback:activ_action ;
      a "Open" ~stock:`OPEN ~tooltip:"Open a file"
	~callback:activ_action ;
      a "Save" ~stock:`SAVE ~tooltip:"Save current file"
	~callback:activ_action ;
      a "SaveAs" ~stock:`SAVE_AS
	~tooltip:"Save to a file" ~callback:activ_action ;
      a "Quit" ~stock:`QUIT ~tooltip:"Quit"
	~callback:activ_action ;
      a "About" ~label:"_About" ~accel:"<control>A" ~tooltip:"About"
	~callback:activ_action ;
      a "Logo" ~stock:(`STOCK "demo-gtk-logo") ~tooltip:"GTK+"
	~callback:activ_action ;

      ta "Bold" ~stock:`BOLD ~label:"_Bold"
	~accel:"<control>B" ~tooltip:"Bold"
	~callback:activ_action ~active:true ;

      radio ~init_value:0 ~callback:(fun n -> Printf.printf "radio action %d\n%!" n)
	 [ ra "Red" 0 ~label:"_Red" 
	     ~tooltip:"Blood" ~accel:"<control>R" ;
	   ra "Green" 1 ~label:"_Green"
	     ~tooltip:"Grass" ~accel:"<control>G" ;
	   ra "Blue" 2 ~label:"_Blue"
	     ~tooltip:"Sky" ~accel:"<control>B" ;
	 ] ;

      radio ~init_value:2 ~callback:(fun n -> Printf.printf "radio action %d\n%!" n)
	[ ra "Square" 0 ~label:"_Square"
	    ~tooltip:"Square" ~accel:"<control>S" ;
	  ra "Rectangle" 1 ~label:"_Rectangle"
	    ~tooltip:"Rectangle" ~accel:"<control>R" ;
	  ra "Oval" 2 ~label:"_Oval"
	    ~tooltip:"Egg" ~accel:"<control>O" ;
	]
    ] ;

  let ui_m = GAction.ui_manager () in
  ui_m#insert_action_group actions 0 ;
  window#add_accel_group ui_m#get_accel_group ;
  ui_m#add_ui_from_string ui_info ;
  
  let box1 = GPack.vbox ~packing:window#add () in
  box1#pack (ui_m#get_widget "/MenuBar") ;
  box1#pack (ui_m#get_widget "/ToolBar") ;
  GMisc.label ~text:"Type\n<alt>\nto start" 
    ~xalign:0.5 ~yalign:0.5 
    ~width:200 ~height:200
    ~packing:box1#pack () ;

  GMisc.separator `HORIZONTAL ~packing:box1#pack () ;

  let b = GButton.button ~stock:`CLOSE ~packing:box1#pack () in
  b#connect#clicked window#destroy ;
  b#misc#set_can_default true ;
  b#misc#grab_default ()

let main () = 
  let w = GWindow.window ~title:"UI Manager" () in
  w#connect#destroy GMain.quit ;
  setup_stock () ;
  setup_ui w ;
  w#show () ;
  GMain.main ()

let _ = main ()


(* Local Variables: *)
(* compile-command: "ocamlc -I ../src -w s lablgtk.cma gtkInit.cmo action.ml" *)
(* End: *)

