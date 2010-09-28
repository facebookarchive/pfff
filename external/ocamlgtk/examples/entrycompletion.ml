(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

let model_of_list conv l =
  let cols = new GTree.column_list in
  let column = cols#add conv in
  let model = GTree.list_store cols in
  List.iter
    (fun data ->
      let row = model#append () in
      model#set ~row ~column data)
    l ;
  (model, column)

let string_completion_list = [ "hello" ; "hello world" ; "abcdef" ; "abcxyz" ]

let stock_completion_list = [ `HOME ; `GO_BACK ; `GO_FORWARD ; `DIALOG_WARNING ; `DIALOG_ERROR ]

let setup packing (make_entry : (GObj.widget -> unit) -> GEdit.entry) =
  let box = GPack.hbox ~packing () in
  let entry = make_entry box#pack in
  let button = GButton.button ~stock:`JUMP_TO ~packing:box#pack () in
  button#connect#clicked
    (fun () -> prerr_endline entry#text) ;
  ()
  
let make_simple_entry packing =
  let entry = GEdit.entry ~text:"text" ~packing () in
  let (model, col) = model_of_list Gobject.Data.string string_completion_list in
  let c = GEdit.entry_completion ~model ~entry () in
  c#set_text_column col ;
  c#insert_action_markup 0 "<span foreground='blue'>action 0</span>" ;
  c#insert_action_markup 1 "<span foreground='red' >action 1</span>" ;
  entry

let is_prefix s1 s2 =
  (String.length s1 <= String.length s2) && (String.sub s2 0 (String.length s1) = s1)

let make_complex_entry packing =
  let entry = GEdit.entry ~text:"pick a stock id" ~packing () in
  let (model, column) = model_of_list GtkStock.conv stock_completion_list in
  let completion = GEdit.entry_completion ~model ~entry () in
  begin
    let renderer = GTree.cell_renderer_pixbuf [ `STOCK_SIZE `BUTTON ] in
    completion#pack renderer ;
    completion#add_attribute renderer "stock_id" column
  end ;
  begin
    let renderer = GTree.cell_renderer_text [ `XPAD 5 ] in
    completion#pack renderer ;
    completion#add_attribute renderer "text" column
  end ;
  completion#set_match_func 
    (fun key row ->
      let column = { column with GTree.conv = Gobject.Data.string } in
      let str = model#get ~row ~column in
      is_prefix key str) ;
  completion#connect#match_selected
    (fun model row ->
(* Unsafe but correct dummy column usage 
      let column = { column with
		     GTree.conv = Gobject.Data.string ;
		     GTree.creator = 0 } in
      entry#set_text (model#get ~row ~column) ;
*)
(* Safer but relies on Comment #1 of Gtk bug 555087. *)
  entry#set_text (GtkStock.convert_id (model#child_model#get 
			 ~row:(model#convert_iter_to_child_iter row)
			 ~column));

      true) ;
  entry


let main () = 
  let w = GWindow.window ~title:"GtkEntryCompletion demo" () in
  w#connect#destroy GMain.quit ;
  begin
    let box = GPack.vbox ~packing:w#add () in
    setup box#pack make_simple_entry ;
    setup box#pack make_complex_entry
  end ;
  w#show () ;
  GMain.main ()

let _ = main ()


