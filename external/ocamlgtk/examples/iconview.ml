(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

let files = [ 
  "gnome-fs-regular.png" ; 
  "gnome-fs-directory.png" ]

let error ?parent message = 
  let w = 
    GWindow.message_dialog ~message 
      ~message_type:`ERROR 
      ~buttons:GWindow.Buttons.close 
      ?parent ~destroy_with_parent:true ~show:true () in
  w#connect#response (fun _ -> w#destroy ()) ;
  ()


let sort_func dir_c name_c (m : #GTree.model) i1 i2 =
  let is_dir_1 = m#get ~column:dir_c ~row:i1 in
  let is_dir_2 = m#get ~column:dir_c ~row:i2 in
  if not is_dir_1 && is_dir_2
  then 1
  else if is_dir_1 && not is_dir_2
  then -1
  else
    let name_1 = m#get ~column:name_c ~row:i1 in
    let name_2 = m#get ~column:name_c ~row:i2 in
    compare name_1 name_2


type data =
    { store : GTree.list_store ;
      path_c : string GTree.column ;
      name_c : string GTree.column ;
      icon_c : GdkPixbuf.pixbuf GTree.column ;
      dir_c  : bool GTree.column ;
      mutable parent : string ;
      file_pb : GdkPixbuf.pixbuf ;
      folder_pb : GdkPixbuf.pixbuf ;
    }


let create_store file_pb folder_pb parent =
  let columns = new GTree.column_list in
  let path_c = columns#add Gobject.Data.string in
  let name_c = columns#add Gobject.Data.string in
  let icon_c = columns#add (Gobject.Data.gobject_by_name "GdkPixbuf") in
  let dir_c  = columns#add Gobject.Data.boolean in
  let store = GTree.list_store columns in
  store#set_sort_func 0 (sort_func dir_c name_c) ;
  store#set_sort_column_id 0 `ASCENDING ;
  { store = store ;  path_c = path_c ;
    name_c = name_c ; icon_c = icon_c ;
    dir_c = dir_c ; parent = parent ; 
    file_pb = file_pb ; folder_pb = folder_pb }

let fill_store d =
  d.store#clear () ;
  Array.iter
    (fun name ->
      if name.[0] <> '.' then begin
	let path = Filename.concat d.parent name in
	let is_dir = (Unix.stat path).Unix.st_kind = Unix.S_DIR in
	let display_name = Glib.Convert.filename_to_utf8 name in
	let row = d.store#append () in
	d.store#set ~row ~column:d.path_c path ;
	d.store#set ~row ~column:d.name_c display_name ;
	d.store#set ~row ~column:d.dir_c  is_dir ;
	d.store#set ~row ~column:d.icon_c (if is_dir then d.folder_pb else d.file_pb)
      end)
    (Sys.readdir d.parent)

let refill_store view d =
  view#set_model None ;
  fill_store d ;
  view#set_model (Some (d.store :> GTree.model))

let up_clicked button view d () =
  d.parent <- Filename.dirname d.parent ;
  refill_store view d ;
  button#misc#set_sensitive (d.parent <> "/")

let home_dir =  
  match Glib.get_home_dir () with
  | None -> exit 2
  | Some s -> s

let home_clicked button view d () =
  d.parent <- home_dir ;
  refill_store view d ;
  button#misc#set_sensitive true

let item_activated button view d path =
  let row = d.store#get_iter path in
  let name = d.store#get ~row ~column:d.path_c in
  Printf.eprintf "tree_path = %s path = %s\n%!" (GTree.Path.to_string path) name ;
  let is_dir = d.store#get ~row ~column:d.dir_c in
  if is_dir then begin
    let path   = d.store#get ~row ~column:d.path_c in
    d.parent <- path ;
    refill_store view d ;
    button#misc#set_sensitive true
  end

let do_iconview window =
  match
    try List.map GdkPixbuf.from_file files
    with exn -> error ~parent:window (Printexc.to_string exn) ; []
  with
  | [ file_pb ; folder_pb ] ->
      let vbox = GPack.vbox ~packing:window#add () in
      let toolbar = GButton.toolbar ~packing:vbox#pack () in
      let up_button =
	GButton.tool_button ~stock:`GO_UP ~packing:toolbar#insert () in
      up_button#set_is_important true ;
      up_button#misc#set_sensitive false ;
      let home_button =
	GButton.tool_button ~stock:`HOME ~packing:toolbar#insert () in
      home_button#set_is_important true ;
      let sw = GBin.scrolled_window 
	  ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
	  ~shadow_type:`ETCHED_IN
	  ~packing:(vbox#pack ~expand:true) () in

      let data = create_store file_pb folder_pb "/" in
      fill_store data ;
      
      let iv = GTree.icon_view 
	  ~model:data.store 
	  ~selection_mode:`MULTIPLE ~packing:sw#add () in
      iv#set_text_column data.name_c ;
      iv#set_pixbuf_column data.icon_c ;

      up_button#connect#clicked   (up_clicked     up_button iv data) ;
      home_button#connect#clicked (home_clicked   up_button iv data) ;
      iv#connect#item_activated   (item_activated up_button iv data) ;

      iv#misc#grab_focus ()

  | _ -> ()
  
let main = 
  let w = GWindow.window ~title:"GtkIconView demo" ~width:650 ~height:400 () in
  w#connect#destroy GMain.quit ;
  do_iconview w ;
  w#show () ;
  GMain.main ()
