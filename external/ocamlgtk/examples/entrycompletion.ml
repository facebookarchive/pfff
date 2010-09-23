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
  let column2 = cols#add conv in
  let column3 = cols#add conv in
  let icon_c = cols#add GtkStock.conv in

  let model = GTree.list_store cols in
  List.iter
    (fun data ->
      let row = model#append () in
      model#set ~row ~column data;
      model#set ~row ~column:column2 data;
      model#set ~row ~column:column3 data;
      model#set ~row ~column:icon_c `YES;
    )
    l ;
  (model, column, icon_c)

let rec build_list n =
  if n = 0 then []
  else ("a" ^ string_of_int n ^ "foobar this is " ^ string_of_int n)::
    build_list (n - 1)

let string_completion_list =
  [ "hello" ; "hello world" ; "abcdef" ; "abcxyz" ] @
  build_list 1000

let setup packing entry =
  let box = GPack.hbox ~packing () in
  let _ = box#add entry#coerce in
  let button = GButton.button ~stock:`JUMP_TO ~packing:box#pack () in
  button#connect#clicked
    (fun () -> prerr_endline entry#text) ;
  ()
  
let make_simple_entry () =
  let entry = GEdit.entry ~width:600 () in
  let c = GEdit.entry_completion () in
  entry#set_completion c;

  let (model, col, icon_c) = 
    model_of_list Gobject.Data.string string_completion_list in
  c#set_text_column col ;
  let renderer = 
    GTree.cell_renderer_pixbuf [ `STOCK_SIZE `BUTTON ] in
  c#pack (renderer :> GTree.cell_renderer);
  c#add_attribute (renderer :> GTree.cell_renderer) "stock_id" icon_c;


  entry#connect#changed (fun () -> 
    let s = entry#text in
    print_string ("changed: " ^ s ^ "\n"); flush stdout;
    let (model, _col, _icon_c) = 
      model_of_list Gobject.Data.string string_completion_list in
    c#set_model (model :> GTree.model);
  );
  entry

let main () = 
  let w = GWindow.window ~title:"GtkEntryCompletion demo" () in
  w#connect#destroy GMain.quit ;
  begin
    let box = GPack.vbox ~packing:w#add () in
    setup box#pack (make_simple_entry ()) ;
  end ;
  w#show () ;
  GMain.main ()

let _ = main ()


