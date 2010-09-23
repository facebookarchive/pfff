(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* This file demonstrates how one can use the tooltip API
 * introduced with GTK+ 2.12.
 * Do:
 * ocamlc -c -I +lablgtk2 tooltip.ml
 * ocamlc -o tooltip.tpo -g -I . -I +lablgtk2 lablgtk.cma tooltip.cmo
 * ./main.tpo
 *
 *)

class contact
  ~(name: string)
  () =
  object (self)
	method name = name
  end
class account
  ~(name: string)
  ~(contacts: contact list)
  () =
  object (self)
	method name = name
	method contacts = contacts
  end

let model () =
	let cols = new GTree.column_list in
	let column = cols#add Gobject.Data.caml in
	let model = GTree.tree_store cols in
	List.iter begin fun account ->
		let row = model#append () in
		model#set ~row ~column (`Account account);
		List.iter begin fun contact ->
			let row = model#append ~parent: row () in
			model#set ~row ~column (`Contact contact)
		  end account#contacts
	  end
	  [ new account ()
	      ~name: "Fernand Naudin"
	      ~contacts: [ new contact () ~name: "Maître Folace"
	                 ; new contact () ~name: "Jean" ]
	  ; new account ()
	      ~name: "Raoul Volfoni"
	      ~contacts: [ new contact () ~name: "Paul Volfoni" ]
	  ];
	(model, column)

let window () =
	let (model, column) = model () in
	let window = GWindow.window () ~title: "TreeView" in
	let vbox = GPack.vbox ()
	  ~border_width: 0
	  ~spacing: 8
	  ~packing: window#add in
	let button = GButton.button ()
	  ~label: "Tontons flingueurs"
	  ~packing: vbox#add in
	button#misc#set_tooltip_text "I am a tooltip text";
	let sw = GBin.scrolled_window ()
	  ~shadow_type: `ETCHED_IN
	  ~hpolicy: `NEVER
	  ~vpolicy: `AUTOMATIC
	  ~packing: vbox#add in
	let _ = window#connect#destroy
	  ~callback: GMain.quit in
	
	let treeview = GTree.view ()
	  ~model ~packing: sw#add in
	
	let col = GTree.view_column ()
	  ~title: "Put the mouse over here too" in
	let renderer_name = GTree.cell_renderer_text [] in
	col#set_sizing `FIXED;
	col#set_fixed_width 50;
	col#pack renderer_name;
	col#set_cell_data_func renderer_name
	  begin fun model row ->
		match model#get ~row ~column with
		| `Account account ->
			let text = account#name in
			renderer_name#set_properties
			  [ `TEXT text
			  ; `WEIGHT `BOLD ]
		| `Contact contact ->
			renderer_name#set_properties
			  [ `TEXT contact#name
			  ; `WEIGHT `NORMAL ] end;
	ignore (treeview#append_column col);
	
	let view_col = treeview#get_column 0 in
	let button = new GButton.button
	  (GtkTree.TreeViewColumn.get_button view_col#as_column) in
	button#misc#set_tooltip_text
	  "I am a tooltip on the button of a column header";
	
	treeview#misc#set_has_tooltip true;
	ignore (treeview#misc#connect#query_tooltip
	  ~callback: begin fun ~x ~y ~kbd tooltip ->
		match GtkTree.TreeView.Tooltip.get_context
		  treeview#as_tree_view ~x ~y ~kbd with
		| (x, y, Some (model, path, row)) ->
			let get ~model ~row ~column =
				let v = Gobject.Value.create_empty () in
				GtkTree.TreeModel.get_value
				  model v ~row ~column: column.GTree.index;
				Gobject.Data.of_value column.GTree.conv v in
			let path_string = GtkTree.TreePath.to_string path in
			let name =
			  (* XXX: be careful to do a match on the good thing: no static type checking *)
			  match get ~model ~row ~column with
			  | `Account o -> o#name
			  | `Contact o -> o#name in
			let str = "path=<b>" ^ path_string ^ "</b> name=<b>" ^ name ^ "</b>" in
			GtkBase.Tooltip.set_markup tooltip str;
			GtkTree.TreeView.Tooltip.set_row
			  treeview#as_tree_view tooltip path;
			true
		| _ -> false
	  end);
	let _ = treeview#selection#connect#changed
	  ~callback: begin fun () ->
		GtkBase.Widget.Tooltip.trigger_query treeview#as_tree_view
	  end in
	
	window#set_default_size ~width: 162 ~height: 242;
	window#show ();
	window#move ~x: 10 ~y: 10

let locale = GtkMain.Main.init ()

let main () =
	window ();
	GMain.main ()
;;
main ()

