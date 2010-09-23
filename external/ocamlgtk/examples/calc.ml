(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: calc.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

(* A simple calculator ported from LablTk to LablGtk *)

let mem_string ~char s =
  try
    for i = 0 to String.length s -1 do
      if s.[i] = char then raise Exit
    done; false
  with Exit -> true

let ops = ['+',(+.); '-',(-.); '*',( *.); '/',(/.)]

(* The abstract calculator class. Does not use Gtk *)

class virtual calc = object (calc)
  val mutable x = 0.0
  val mutable op = None
  val mutable displaying = true

  method virtual set : string -> unit
  method virtual get : string
  method virtual quit : unit -> unit
  method insert s = calc#set (calc#get ^ s)
  method get_float = float_of_string (calc#get)

  initializer calc#set "0"

  method command s =
    if s <> "" then match s.[0] with
      '0'..'9' ->
	if displaying then (calc#set ""; displaying <- false);
	calc#insert s
    | '.' ->
	if displaying then
	  (calc#set "0."; displaying <- false)
	else
	  if not (mem_string ~char:'.' calc#get) then calc#insert s
    | '+'|'-'|'*'|'/' as c ->
	displaying <- true;
	begin match op with
	  None ->
	    x <- calc#get_float;
	    op <- Some (List.assoc c ops)
	| Some f ->
	    x <- f x (calc#get_float);
	    op <- Some (List.assoc c ops);
	    calc#set (string_of_float x)
	end
    | '='|'\n'|'\r' ->
	displaying <- true;
	begin match op with
	  None -> ()
	| Some f ->
	    x <- f x (calc#get_float);
	    op <- None;
	    calc#set (string_of_float x)
	end
    | 'q' -> calc#quit ()
    | _ -> ()
end

(* Buttons for the calculator *)

let m =
  [|[|"7";"8";"9";"+"|];
    [|"4";"5";"6";"-"|];
    [|"1";"2";"3";"*"|];
    [|"0";".";"=";"/"|]|]

(* The physical calculator. Inherits from the abstract one *)

open GMain

class calculator ?packing ?show () =
  let table = GPack.table ~rows:5 ~columns:4 ~homogeneous:true ~show:false () in
  object (calc)
    inherit calc

    val label =
      let frame = GBin.frame ~shadow_type:`IN ()
	~packing:(table#attach ~left:0 ~top:0 ~right:4 ~expand:`BOTH) in
      let evbox = GBin.event_box ~packing:frame#add () in
      evbox#misc#set_style evbox#misc#style#copy;
      evbox#misc#style#set_bg [`NORMAL,`WHITE];
      GMisc.label ~justify:`RIGHT ~xalign:0.95 ~packing:evbox#add ()
    val table = table

    method set = label#set_text
    method get = label#text
    method quit = Main.quit

    initializer
      for i = 0 to 3 do for j = 0 to 3 do
	let button =
	  GButton.button ~label:("  " ^ m.(i).(j) ^ "  ")
	    ~packing:(table#attach ~top:(i+1) ~left:j ~expand:`BOTH) () in
	button#connect#clicked ~callback:(fun () -> calc#command m.(i).(j));
      done done;
      ignore (GObj.pack_return table ~packing ~show)
  end

(* Finally start everything *)

let w = GWindow.window ()

let applet = new calculator ~packing: w#add ()

let _ =
  w#connect#destroy ~callback: Main.quit;
  w#event#connect#key_press
    ~callback:(fun ev -> applet#command (GdkEvent.Key.string ev); true);
  w#show ();
  Main.main ()
