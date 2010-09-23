(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: tree_store.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

open StdLabels

(* translated from gtk-demo *)

let january =
  [ "New Years Day", true, true, true, true, false, true;
    "Presidential Inauguration", false, true, false, true, false, false;
    "Martin Luther King Jr. day", false, true, false, true, false, false;
  ]

let february =
  [  "Presidents' Day", false, true, false, true, false, false;
     "Groundhog Day", false, false, false, false, false, false;
     "Valentine's Day", false, false, false, false, true, true;
  ]

let march =
  [  "National Tree Planting Day", false, false, false, false, false, false;
     "St Patrick's Day", false, false, false, false, false, true;
  ]
let april =
  [  "April Fools' Day", false, false, false, false, false, true;
     "Army Day", false, false, false, false, false, false;
     "Earth Day", false, false, false, false, false, true;
     "Administrative Professionals' Day", false, false, false,
     false, false, false;
  ]

let may =
  [  "Nurses' Day", false, false, false, false, false, false;
     "National Day of Prayer", false, false, false, false, false, false;
     "Mothers' Day", false, false, false, false, false, true;
     "Armed Forces Day", false, false, false, false, false, false;
     "Memorial Day", true, true, true, true, false, true;
  ]

let june =
  [  "June Fathers' Day", false, false, false, false, false, true;
     "Juneteenth (Liberation of Slaves)",
     false, false, false, false, false, false;
     "Flag Day", false, true, false, true, false, false;
  ]

let july =
  [  "Parents' Day", false, false, false, false, false, true;
     "Independence Day", false, true, false, true, false, false;
  ]

let august =
  [  "Air Force Day", false, false, false, false, false, false;
     "Coast Guard Day", false, false, false, false, false, false;
     "Friendship Day", false, false, false, false, false, false;
  ]

let september =
  [  "Grandparents' Day", false, false, false, false, false, true;
     "Citizenship Day or Constitution Day", false, false, false, false,
     false, false;
     "Labor Day", true, true, true, true, false, true;
  ]

let october =
  [  "National Children's Day", false, false, false, false, false, false;
     "Bosses' Day", false, false, false, false, false, false;
     "Sweetest Day", false, false, false, false, false, false;
     "Mother-in-Law's Day", false, false, false, false, false, false;
     "Navy Day", false, false, false, false, false, false;
     "Columbus Day", false, true, false, true, false, false;
     "Halloween", false, false, false, false, false, true;
  ]

let november =
  [  "Marine Corps Day", false, false, false, false, false, false;
     "Veterans' Day", true, true, true, true, false, true;
     "Thanksgiving", false, true, false, true, false, false;
  ]

let december =
  [  "Pearl Harbor Remembrance Day", false, false, false, false, false, false;
     "Christmas", true, true, true, true, false, true;
     "Kwanzaa", false, false, false, false, false, false;
  ]

let toplevel =
  [ "January", january;
    "February", february;
    "March", march;
    "April", april;
    "May", may;
    "June", june;
    "July", july;
    "August", august;
    "September", september;
    "October", october;
    "November", november;
    "December", december;
  ]

open Gobject.Data

let cols = new GTree.column_list
let name = cols#add string
let alex = cols#add boolean
let havoc = cols#add boolean
let tim = cols#add boolean
let owen = cols#add boolean
let dave = cols#add boolean
let visible = cols#add boolean
let world = cols#add boolean
let bg = cols#add (unsafe_boxed (Gobject.Type.from_name "GdkColor"))

let create_model () =
  let model = GTree.tree_store cols in
  List.iter toplevel ~f:
    begin fun (month_name, month) ->
      let row = model#append () in
      model#set ~row ~column:name month_name;
      List.iter month ~f:
        begin fun (n,a,h,t,o,d,w) ->
          let row = model#append ~parent:row () in
          let set column = model#set ~row ~column in
          set name n;
          set alex a;
          set havoc h;
          set tim t;
          set owen o;
          set dave d;
          set visible true;
          set world w;
          set bg (GDraw.color (`NAME "orange"))
        end;
    end;
  model

let item_toggled ~(model : GTree.tree_store) ~column path =
  let row = model#get_iter path in
  let b = model#get ~row ~column in
  model#set ~row ~column (not b);
  ()

open GtkTree

let add_columns ~(view : GTree.view) ~model =
  let renderer = GTree.cell_renderer_text [`XALIGN 0.] in
  let vc =
    GTree.view_column ~title:"Holiday" ~renderer:(renderer, ["text", name]) ()
  in
  vc#add_attribute renderer "background-gdk" bg;
  view#append_column vc;

  List.iter
    ["Alex",alex,true; "Havoc",havoc,false; "Tim",tim,true;
     "Owen",owen,false; "Dave",dave,false ]
    ~f:
    begin fun (title, column, euro) ->
      let renderer = GTree.cell_renderer_toggle [`XALIGN 0.] in
      renderer#connect#toggled ~callback:(item_toggled ~model ~column);
      let attrs =
        if euro then
          ["active", column; "visible", visible; "activatable", world]
        else ["active", column; "visible", visible]
      in
      let vc = GTree.view_column ~title ~renderer:(renderer, attrs) () in
      view#append_column vc;
      vc#set_sizing `FIXED;
      vc#set_fixed_width 50;
      vc#set_clickable true;
    end

let do_tree_store () =
  let window = GWindow.window ~title:"Card planning sheet" () in
  window#connect#destroy ~callback:GMain.quit;
  let vbox = GPack.vbox ~border_width:8 ~spacing:8 ~packing:window#add () in
  GMisc.label ~text:"Jonathan's Holiday Card Planning Sheet"
    ~packing:vbox#pack ();
  let sw = GBin.scrolled_window ~shadow_type:`ETCHED_IN ~hpolicy:`AUTOMATIC
      ~vpolicy:`AUTOMATIC ~packing:vbox#add () in
  let model = create_model () in
  let treeview = GTree.view ~model ~packing:sw#add () in
  treeview#set_rules_hint true;
  treeview#selection#set_mode `MULTIPLE;
  add_columns ~view:treeview ~model;
  treeview#misc#connect#realize ~callback:treeview#expand_all;
  window#set_default_size ~width:650 ~height:400;
  window#show ();
  GMain.main ()

let () = do_tree_store ()
