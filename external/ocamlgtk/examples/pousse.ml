(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: pousse.ml 1347 2007-06-20 07:40:34Z guesdon $ *)

open StdLabels

(* The game logic *)

type color = [`none|`white|`black]

module type BoardSpec = sig
  type t
  val size : int
  val get : t -> x:int -> y:int -> color
  val set : t -> x:int -> y:int -> color:color -> unit
end

module Board (Spec : BoardSpec) = struct
  open Spec
  let size = size

  let on_board x y =
    x >= 0 && x < size && y >= 0 && y < size

  let rec string board ~x ~y ~dx ~dy ~color l =
    let x = x+dx and y = y+dy in
    if on_board x y then
      let col = get board ~x ~y in 
      if col = (color : [`white|`black] :> color) then l else
      if col = `none then [] else
      string board ~x ~y ~dx ~dy ~color ((x,y)::l)
    else []

  let find_swaps board ~x ~y ~color =
    if get board ~x ~y <> `none then [] else
    List.fold_left [-1,-1; -1,0; -1,1; 0,-1; 0,1; 1,-1; 1,0; 1,1]
      ~init:[]
      ~f:(fun acc (dx,dy) -> string board ~x ~y ~dx ~dy ~color [] @ acc)

  let action board ~x ~y ~color =
    let swaps = find_swaps board ~x ~y ~color in
    if swaps = [] then false else begin
      List.iter ((x,y)::swaps)
	~f:(fun (x,y) -> set board ~x ~y ~color:(color :> color));
      true
    end

  let check_impossible board ~color =
    try
      for x = 0 to size - 1 do for y = 0 to size - 1 do
	if find_swaps board ~x ~y ~color <> [] then raise Exit
      done done;
      true
    with Exit -> false

  let count_cells board =
    let w = ref 0 and b = ref 0 in
    for x = 0 to size - 1 do for y = 0 to size - 1 do
      match get board ~x ~y with
	`white -> incr w
      | `black -> incr b
      | `none -> ()
    done done;
    (!w,!b)
end

(* GUI *)

open GMain

(* Toplevel window *)

let window = GWindow.window ~title:"pousse" ()

(* Create pixmaps *)

let pixdraw =
  GDraw.pixmap ~window ~width:40 ~height:40 ~mask:true ()
let pixdraw1 =
  GDraw.pixmap ~window ~width:40 ~height:40 ~mask:true ()
let pixdraw2 =
  GDraw.pixmap ~window ~width:40 ~height:40 ~mask:true ()

let _ =
  pixdraw1#set_foreground `BLACK;
  pixdraw1#arc ~x:3 ~y:3 ~width:34 ~height:34 ~filled:true ();
  pixdraw2#set_foreground `WHITE;
  pixdraw2#arc ~x:3 ~y:3 ~width:34 ~height:34 ~filled:true ();
  pixdraw2#set_foreground `BLACK;
  pixdraw2#arc ~x:3 ~y:3 ~width:34 ~height:34 ()

(* The cell class: a button with a pixmap on it *)

class cell ?packing ?show () =
  let button = GButton.button ?packing ?show () in
object (self)
  inherit GObj.widget button#as_widget
  method connect = button#connect
  val mutable color : color = `none
  val pm = GMisc.pixmap pixdraw ~packing:button#add ()
  method color = color
  method set_color col =
    if col <> color then begin
      color <- col;
      pm#set_pixmap
	(match col with `none -> pixdraw
	| `black -> pixdraw1
	| `white -> pixdraw2)
    end
end

module RealBoard = Board (
  struct
    type t = cell array array
    let size = 8
    let get (board : t) ~x ~y = board.(x).(y)#color
    let set (board : t) ~x ~y ~color = board.(x).(y)#set_color color
  end
)

(* Conducting a game *)

open RealBoard

class game ~(frame : #GContainer.container) ~(label : #GMisc.label)
    ~(statusbar : #GMisc.statusbar) =
  let table = GPack.table ~columns:size ~rows:size ~packing:frame#add () in
object (self)
  val cells =
    Array.init size
      ~f:(fun i -> Array.init size
	  ~f:(fun j -> new cell ~packing:(table#attach ~top:i ~left:j) ()))
  val label = label
  val turn = statusbar#new_context ~name:"turn"
  val messages = statusbar#new_context ~name:"messages"
  val mutable current_color = `black
  method board = cells
  method table = table
  method player = current_color

  method swap_players () =
    current_color <-
      match current_color with
	`white -> turn#pop (); turn#push "Player is black"; `black
      | `black -> turn#pop (); turn#push "Player is white"; `white

  method finish () =
    turn#pop ();
    let w, b = count_cells cells in
    turn#push
      (if w > b then "White wins" else
       if w < b then "Black wins" else
       "Game is a draw");
    ()

  method update_label () =
    let w, b = count_cells cells in
    label#set_text (Printf.sprintf "White: %d Black: %d " w b)

  method play x y =
    if action cells ~x ~y ~color:current_color then begin
      self#update_label ();
      self#swap_players ();
      if check_impossible cells ~color:current_color then begin
	self#swap_players ();
	if check_impossible cells ~color:current_color then self#finish ()
      end
    end else
      messages#flash "You cannot play there"

  initializer
    for i = 0 to size-1 do for j = 0 to size-1 do
      let cell = cells.(i).(j) in
      cell#connect#enter ~callback:cell#misc#grab_focus;
      cell#connect#clicked ~callback:(fun () -> self#play i j)
    done done;
    List.iter ~f:(fun (x,y,col) -> cells.(x).(y)#set_color col)
      [ 3,3,`black; 4,4,`black; 3,4,`white; 4,3,`white ];
    self#update_label ();
    turn#push "Player is black";
    ()
end

(* Graphical elements *)

let vbox = GPack.vbox ~packing:window#add ()
let frame = GBin.frame ~shadow_type:`IN ~packing:vbox#add ()
let hbox = GPack.hbox ~packing:vbox#pack ()

let bar = GMisc.statusbar ~packing:hbox#add ()

let frame2 = GBin.frame ~shadow_type:`IN ~packing:hbox#pack ()
let label =
  GMisc.label ~justify:`LEFT ~xpad:5 ~xalign:0.0 ~packing:frame2#add ()

let game = new game ~frame ~label ~statusbar:bar

(* Start *)

let _ =
  window#connect#destroy ~callback:Main.quit;
  window#show ();
  Main.main ()
