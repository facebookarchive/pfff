(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

let changed_and_get_active (combo : #GEdit.combo_box) column cb =
  combo#connect#changed
    (fun () ->
      match combo#active_iter with
      | None -> ()
      | Some row -> 
	  let data = combo#model#get ~row ~column in
	  cb data)

let setup_combobox_demo_grid packing =
  let tmp = GBin.frame ~label:"GtkComboBox (grid mode)" ~packing () in
  let box = GPack.vbox ~border_width:5 ~packing:tmp#add () in

  let model, column = GTree.store_of_list Gobject.Data.string
      [ "red"; "green" ; "blue" ; 
	"yellow" ; "black" ; "white" ;
	"gray" ; "snow" ; "magenta" ] in
  let combo = GEdit.combo_box ~model ~wrap_width:3 ~packing:box#pack () in
  let cell = GTree.cell_renderer_pixbuf [ `WIDTH 16 ; `HEIGHT 16 ] in
  combo#pack ~expand:true cell ;
  combo#add_attribute cell "cell-background" column ;
  combo#set_active 1 ;
  changed_and_get_active combo column prerr_endline ;
  ()

let create_model () =
  GTree.store_of_list GtkStock.conv 
    [ `DIALOG_WARNING ;
      `STOP ;
      `NEW ;
      `CLEAR ]

let setup_combobox_demo packing =
  let tmp = GBin.frame ~label:"GtkComboBox" ~packing () in
  let box = GPack.vbox ~border_width:5 ~packing:tmp#add () in

  let model, column = create_model () in
  let combobox = GEdit.combo_box ~model ~packing:box#pack () in
    
  begin
    let renderer = GTree.cell_renderer_pixbuf [ `STOCK_SIZE `BUTTON ] in
    combobox#pack renderer ;
    combobox#add_attribute renderer "stock_id" column
  end ;
  begin
    let renderer = GTree.cell_renderer_text [ `XPAD 5 ] in
    combobox#pack renderer ;
    combobox#add_attribute renderer "text" column
  end ;
  combobox#set_active 1 ;
  changed_and_get_active combobox column 
    (fun id -> prerr_endline (GtkStock.convert_id id))

let setup_combobox_text packing =
  let tmp = GBin.frame ~label:"GtkComboBox (text-only)" ~packing () in
  let box = GPack.vbox ~border_width:5 ~packing:tmp#add () in
  let (combo, (_, column)) = 
    GEdit.combo_box_text ~packing:box#pack 
      ~strings:[ "Jan" ; "Feb" ; "Mar" ; "Apr" ; "May" ; "Jun" ; 
		 "Jul" ; "Aug" ; "Sep" ; "Oct" ; "Nov" ; "Dec" ] () in
  combo#set_active 0 ;
  changed_and_get_active combo column prerr_endline ;
  ()

let setup_combobox_entry packing =
  let tmp = GBin.frame ~label:"GtkComboBoxEntry" ~packing () in
  let box = GPack.vbox ~border_width:5 ~packing:tmp#add () in
  let model, text_column = 
    GTree.store_of_list Gobject.Data.string
      [ "Paris" ; "Grenoble" ; "Toulouse" ] in
  let combo = GEdit.combo_box_entry ~text_column ~model ~packing:box#pack () in
  combo#entry#connect#changed 
    (fun () -> match combo#entry#text with "" -> () | s -> prerr_endline s) ;
  ()

let setup_combobox_entry_text packing =
  let tmp = GBin.frame ~label:"GtkComboBoxEntry (text-only)" ~packing () in
  let box = GPack.vbox ~border_width:5 ~packing:tmp#add () in
  let (combo, _) = 
    GEdit.combo_box_entry_text ~packing:box#pack 
      ~strings:[ "Paris" ; "Grenoble" ; "Toulouse" ] () in
  combo#entry#connect#changed 
    (fun () -> match combo#entry#text with "" -> () | s -> prerr_endline s) ;
  ()

let setup_combobox_separator packing =
  let tmp = GBin.frame ~label:"GtkComboBox (separators)" ~packing () in
  let box = GPack.vbox ~border_width:5 ~packing:tmp#add () in
  let (combo, (_, column)) = 
    GEdit.combo_box_text ~packing:box#pack 
      ~strings:[ "Paris" ; "Grenoble" ; "Toulouse" ; "--" ; "New York"; "владивосток"] () in
  combo#set_row_separator_func
    (Some (fun m row -> m#get ~row ~column = "--")) ;
  ()

let main () =
  let window = GWindow.window ~border_width:5 () in
  window#connect#destroy GMain.quit ;
  
  let mainbox = GPack.vbox ~spacing:2 ~packing:window#add () in
  setup_combobox_demo mainbox#pack ;
  setup_combobox_demo_grid mainbox#pack ;
  setup_combobox_text mainbox#pack ;
  setup_combobox_entry mainbox#pack ;
  setup_combobox_entry_text mainbox#pack ;
  setup_combobox_separator mainbox#pack ;
  
  window#show () ;
  GMain.main ()

let _ = main ()

(* Local Variables: *)
(* compile-command: "ocamlc -I ../src -w s lablgtk.cma gtkInit.cmo combobox.ml" *)
(* End: *)
