(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: kaimono.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

open StdLabels
open GMain
open Printf

let file_dialog ~title ~callback ?filename () =
  let sel = GWindow.file_selection ~title ~modal:true ?filename () in
  sel#cancel_button#connect#clicked ~callback:sel#destroy;
  sel#ok_button#connect#clicked ~callback:
    begin fun () ->
      let name = sel#filename in
      sel#destroy ();
      callback name
    end;
  sel#show ()

let w = GWindow.window ~title:"Okaimono" ()
let vb = GPack.vbox ~packing:w#add ()

let menubar = GMenu.menu_bar ~packing:vb#pack ()
let factory = new GMenu.factory menubar
let file_menu = factory#add_submenu "File"
let edit_menu = factory#add_submenu "Edit"

let sw = GBin.scrolled_window ~height:200 ~packing:vb#add
    ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ()
let vp = GBin.viewport ~width:340 ~shadow_type:`NONE ~packing:sw#add ()
let table = GPack.table ~columns:4 ~rows:256 ~packing:vp#add ()
let _ =
  table#focus#set_vadjustment (Some vp#vadjustment)

let top = ref 0
and left = ref 0
let add_to_table  w =
  table#attach ~left:!left ~top:!top ~expand:`X w;
  incr left;
  if !left >= 4 then (incr top; left := 0)

let entry_list = ref []

let add_entry () =
  let entry =
    List.map [40;200;40;60]
      ~f:(fun width -> GEdit.entry ~packing:add_to_table ~width ())
  in entry_list := entry :: !entry_list

let _ =
  List.iter2 ["Number";"Name";"Count";"Price"] [40;200;40;60] ~f:
    begin fun label width ->
      let b = GButton.button ~label ~packing:add_to_table () in
      b#misc#set_size_request ~width ()
    end;
  for i = 1 to 9 do add_entry () done

let split ~sep s =
  let len = String.length s in
  let rec loop pos =
    let next =
      try String.index_from s pos sep with Not_found -> len
    in
    let sub = String.sub s ~pos ~len:(next-pos) in
    if next = len then [sub] else sub::loop (next+1)
  in loop 0

let load name =
  try
    let ic = open_in name in
    List.iter !entry_list
      ~f:(fun l -> List.iter l ~f:(fun e -> e#set_text ""));
    let entries = Stack.create () in
    List.iter !entry_list ~f:(fun x -> Stack.push x entries);
    try while true do
      let line = input_line ic in
      let fields = split ~sep:'\t' line in
      let entry =
	try Stack.pop entries
	with Stack.Empty ->
	  add_entry (); List.hd !entry_list
      in
      List.fold_left fields ~init:entry ~f:
	begin fun acc field ->
	  (List.hd acc)#set_text field;
	  List.tl acc
	end
    done
    with End_of_file -> close_in ic
  with Sys_error _ -> ()
    

let save name =
  try
    let oc = open_out name in
    List.iter (List.rev !entry_list) ~f:
      begin fun entry ->
	let l = List.map entry ~f:(fun e -> e#text) in
	if List.exists l ~f:((<>) "") then
	  let rec loop = function
	      [] -> ()
	    | [x] -> fprintf oc "%s\n" x
	    | x::l -> fprintf oc "%s\t" x; loop l
	  in loop l
      end;
    close_out oc
  with Sys_error _ -> ()

open GdkKeysyms

let _ =
  w#connect#destroy ~callback:Main.quit;
  w#event#connect#key_press ~callback:
    begin fun ev ->
      let key = GdkEvent.Key.keyval ev and adj = vp#vadjustment in
      if key = _Page_Up then
	adj#set_value (adj#value -. adj#page_increment)
      else if key = _Page_Down then
	adj#set_value (min (adj#value +. adj#page_increment)
			 (adj#upper -. adj#page_size));
      false
    end;
  w#add_accel_group factory#accel_group;
  let ff = new GMenu.factory file_menu ~accel_group:factory#accel_group in
  ff#add_item ~key:_O "Open..."
    ~callback:(file_dialog ~title:"Open data file" ~callback:load);
  ff#add_item ~key:_S "Save..."
    ~callback:(file_dialog ~title:"Save data" ~callback:save);
  ff#add_separator ();
  ff#add_item ~key:_Q "Quit" ~callback:w#destroy;
  let ef = new GMenu.factory edit_menu ~accel_group:factory#accel_group in
  ef#add_item ~key:_A "Add line" ~callback:add_entry;
  w#show ();
  Main.main ()
