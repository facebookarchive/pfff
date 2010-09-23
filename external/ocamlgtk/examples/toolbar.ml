(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

let change_orientation button (table : #GPack.table) toolbar () =
  let toolbar_w = (toolbar :> GObj.widget) in
  let orientation = if button#active then `VERTICAL else `HORIZONTAL in
  table#remove  toolbar_w ;
  toolbar#set_orientation orientation ;
  match orientation with
  | `HORIZONTAL ->
      table#attach ~left:0 ~right:2 ~top:0 ~bottom:1 
	~expand:`X ~fill:`BOTH toolbar_w
  | `VERTICAL ->
      table#attach ~left:0 ~right:1 ~top:0 ~bottom:6
	~expand:`Y ~fill:`BOTH toolbar_w


let change_toolbar_style combo toolbar () =
  toolbar#set_style 
    (List.assoc combo#active 
       [ 0, `ICONS; 1, `TEXT; 2, `BOTH; 3, `BOTH_HORIZ ])

let set_toolbar_style_toggled button combo toolbar () =
  if button#active
  then change_toolbar_style combo toolbar ()
  else toolbar#unset_style () ;
  combo#misc#set_sensitive button#active

    
let change_icon_size combo toolbar () =
  toolbar#set_icon_size
    (List.assoc combo#active
       [ 0, `SMALL_TOOLBAR; 1, `LARGE_TOOLBAR ])

let set_icon_size_toggled button combo toolbar () =
  if button#active
  then change_icon_size combo toolbar ()
  else toolbar#unset_icon_size () ;
  combo#misc#set_sensitive button#active


let create_item_list packing =
  let cols = new GTree.column_list in
  let item_col : Gtk.tool_item Gtk.obj GTree.column = 
    cols#add Gobject.Data.gobject in
  let name_col = cols#add Gobject.Data.string in
  let store = GTree.list_store cols in
  let tree_view = GTree.view ~model:store ~packing () in
  tree_view#append_column
    (GTree.view_column ~title:"Tool Item" 
       ~renderer:(GTree.cell_renderer_text [], [ "text", name_col ]) ()) ;

  let item_property_column ~title ~setter ~getter =
    let cell = GTree.cell_renderer_toggle [] in
    cell#connect#toggled
      (fun path ->
	let item = new GButton.tool_item
	    (store#get ~row:(store#get_iter path) ~column:item_col) in
	setter item (not (getter item))) ;
    let view_column = GTree.view_column ~title () in
    view_column#pack cell ;
    view_column#set_cell_data_func cell 
      (fun model row ->
	let item = new GButton.tool_item (model#get ~row ~column:item_col) in
	cell#set_properties [ `ACTIVE (getter item) ]) ;
    tree_view#append_column view_column 
  in

  item_property_column
    ~title:"Visible (horizontal)"
    ~setter:(fun item -> item#set_visible_horizontal)
    ~getter:(fun item -> item#visible_horizontal) ;
  item_property_column
    ~title:"Visible (vertical)"
    ~setter:(fun item -> item#set_visible_vertical)
    ~getter:(fun item -> item#visible_vertical) ;
  item_property_column
    ~title:"Expand"
    ~setter:(fun item -> item#set_expand)
    ~getter:(fun item -> item#get_expand) ;
  item_property_column
    ~title:"Homogeneous"
    ~setter:(fun item -> item#set_homogeneous)
    ~getter:(fun item -> item#get_homogeneous) ;
  item_property_column
    ~title:"Important"
    ~setter:(fun item -> item#set_is_important)
    ~getter:(fun item -> item#is_important) ;

  (store, name_col, item_col, tree_view)

let context_menu_cb toolbar x y button =
  let menu = GMenu.menu () in
  for i = 1 to 5 do
    let label = Printf.sprintf "Item _%d" i in
    GMenu.menu_item ~label ~use_mnemonic:true ~packing:menu#append ()
  done ;
  menu#popup ~button:0 ~time:(GtkMain.Main.get_current_event_time ()) ;
  true


let targets = [
  { Gtk.target = "application/x-toolbar-item" ; Gtk.flags = [] ; Gtk.info = 0 }
]

(* this doesn't seem to work :( *)
let drag_item = ref None

let toolbar_drag_motion_cb (toolbar : #GButton.toolbar) 
    (ctx : GObj.drag_context) ~x ~y ~time =
  let item =
    match !drag_item with
    | None ->
	let it = GButton.tool_button ~label:"A quite long button" () in
	drag_item := Some it ; it
    | Some it -> it in
  ctx#status ~time (Some `MOVE) ;
  let index = toolbar#get_drop_index x y in
  toolbar#set_drop_highlight_item 
    (Some (item, index)) ;
  true

let toolbar_drag_leave_cb (toolbar : #GButton.toolbar) ctx ~time =
  drag_item := None ;
  toolbar#set_drop_highlight_item None

let toolbar_drag_drop_cb toolbar label ctx ~x ~y ~time =
  let l = string_of_int (toolbar#get_drop_index x y) in
  label#set_label l ;
  true

let main =
  let w = GWindow.window ~title:"Toolbar demo" () in
  w#connect#destroy GMain.quit ;
  
  let table = GPack.table ~rows:5 ~columns:2 ~packing:w#add () in

  let toolbar = GButton.toolbar ~packing:(table#attach ~left:0 ~top:0 ~right:2 
					    ~expand:`X ~fill:`BOTH) () in

  toolbar#connect#popup_context_menu (context_menu_cb toolbar) ;

  begin
    let hbox1 = GPack.hbox ~spacing:3 ~border_width:5 
	~packing:(table#attach ~left:1 ~top:1 ~expand:`X ~fill:`BOTH) () in

    begin
      let checkbox = GButton.check_button ~label:"_Vertical" 
	  ~use_mnemonic:true ~packing:hbox1#pack () in
      checkbox#connect#toggled (change_orientation checkbox table toolbar)
    end ;
    begin
      let checkbox = GButton.check_button ~label:"_Show Arrow" 
	  ~use_mnemonic:true ~packing:hbox1#pack () in
      checkbox#connect#toggled 
	(fun () -> toolbar#set_show_arrow checkbox#active) ;
    end ;
  end ;
  begin
    let hbox2 = GPack.hbox ~spacing:3 ~border_width:5 
	~packing:(table#attach ~left:1 ~top:2 ~expand:`X ~fill:`BOTH) () in
  
    let checkbox = GButton.check_button ~label:"Set _Toolbar Style" 
	~use_mnemonic:true ~packing:hbox2#pack () in
    let (combo, _) = GEdit.combo_box_text
	~strings:[ "icons"; "text"; "both (vertical)"; "both (horizontal)" ] 
	~packing:hbox2#pack () in
    combo#misc#set_sensitive false ;
    combo#set_active
      (List.assoc toolbar#style [ `ICONS, 0; `TEXT, 1;
				  `BOTH, 2; `BOTH_HORIZ, 3 ]) ;
    combo#connect#changed (change_toolbar_style combo toolbar) ;
    checkbox#connect#toggled (set_toolbar_style_toggled checkbox combo toolbar)
  end ;
  begin
    let hbox3 = GPack.hbox ~spacing:3 ~border_width:5 
	~packing:(table#attach ~left:1 ~top:3 ~expand:`X ~fill:`BOTH) () in
    let checkbox = GButton.check_button ~label:"Set _Icon Size"
	~use_mnemonic:true ~packing:hbox3#pack () in
    let (combo, _) = GEdit.combo_box_text
	~strings:[ "small toolbar"; "large toolbar" ]
	~packing:hbox3#pack () in
    combo#misc#set_sensitive false ;
    combo#set_active 
      (List.assoc toolbar#icon_size [ `SMALL_TOOLBAR, 0; `LARGE_TOOLBAR, 1 ]) ;
    combo#connect#changed (change_icon_size combo toolbar) ;
    checkbox#connect#toggled (set_icon_size_toggled checkbox combo toolbar)
  end ;

  begin
    let scrolled_window = GBin.scrolled_window 
	~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC 
	~packing:(table#attach ~left:1 ~top:4 ~expand:`BOTH ~fill:`BOTH) () in
    let (store, name_col, item_col, treeview) = 
      create_item_list scrolled_window#add in

    let add_item name item =
      let row = store#append () in
      store#set ~row ~column:name_col name ;
      store#set ~row ~column:item_col item#as_tool_item ;
      toolbar#insert item
    in
    add_item "New"
      (GButton.tool_button ~stock:`NEW ~expand:true ()) ;
    add_item "Open" 
      (GButton.tool_button ~stock:`OPEN ()) ;
    add_item "-----" 
      (GButton.separator_tool_item ()) ;
    begin
      let item = GButton.tool_button ~stock:`REFRESH () in
      add_item "Refresh" item ;
      item#connect#clicked (fun () -> print_endline "clicked")
    end ;
    begin
      let item = GButton.tool_item () in
      let image = GMisc.image ~stock:`DIALOG_WARNING 
	  ~icon_size:`DIALOG ~packing:item#add () in
      add_item "(Custom Item)" item
    end ;
    add_item "Back" 
      (GButton.tool_button ~stock:`GO_BACK ()) ;
    add_item "-----" 
      (GButton.separator_tool_item ()) ;
    add_item "Forward" 
      (GButton.tool_button ~stock:`GO_FORWARD ()) ;
    begin
      let item = GButton.toggle_tool_button ~stock:`BOLD () in
      item#connect#toggled 
	(fun () -> 
	  Printf.printf "Bold toggled (active=%b)\n" item#get_active ; 
	  flush stdout) ;
      add_item "Bold" item
    end ;
    add_item "-----" 
      (GButton.separator_tool_item ~draw:false ~expand:true ()) ;
    begin
      let item = GButton.radio_tool_button 
	  ~stock:`JUSTIFY_LEFT () in
      add_item "Left" item ;
      add_item "Center"
	(GButton.radio_tool_button ~group:item 
	   ~stock:`JUSTIFY_CENTER ()) ;
      add_item "Right"
	(GButton.radio_tool_button ~group:item 
	   ~stock:`JUSTIFY_RIGHT ())
    end ;
    begin
      let image = GMisc.image ~file:"/usr/share/gtk-2.0/demo/apple-red.png" () in
      let item = GButton.tool_button ~label:"_Apple" 
	  ~use_underline:true () in
      item#set_icon_widget image#coerce ;
      add_item "Apple" item
    end ;
    begin
      let hbox = GPack.hbox ~border_width:5 ~spacing:5 
	  ~packing:(table#attach ~left:1 ~top:5 ~expand:`X ~fill:`BOTH) () in
      let button = GButton.button ~label:"Drag me to the toolbar"
	  ~packing:hbox#pack () in
      let label = GMisc.label ~text:"Drop index:" ~packing:hbox#pack () in
      let label = GMisc.label ~packing:hbox#pack () in
      
      button#drag#source_set ~modi:[`BUTTON1] ~actions:[`MOVE] targets ;
      toolbar#drag#dest_set ~flags:[`DROP] ~actions:[`MOVE] targets ;
      toolbar#drag#connect#motion (toolbar_drag_motion_cb toolbar) ;
      toolbar#drag#connect#leave  (toolbar_drag_leave_cb toolbar) ;
      toolbar#drag#connect#drop   (toolbar_drag_drop_cb toolbar label);
    end ;
  end ;

  w#show () ;
  GMain.main ()
