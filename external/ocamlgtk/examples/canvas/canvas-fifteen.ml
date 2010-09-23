(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

let piece_size = 50

let piece_color nb =
  let y = nb / 4 in
  let x = nb mod 4 in
  let r = (4 - x) * 255 / 4 in
  let g = (4 - y) * 255 / 4 in
  let b = 128 in
  Printf.sprintf "#%02x%02x%02x" r g b

type config = {
    canvas : GnoCanvas.canvas ;
    board : (GnoCanvas.group * GnoCanvas.text) array ;
    pos : int array ;
    mutable hole : int ;
  }
    

let move config num dpos =
  assert(List.mem dpos [ -1; 1; -4; 4]) ;
  let (it, _ ) = config.board.(num) in
  it#move 
    ~x:(float (dpos mod 4 * piece_size))
    ~y:(float (dpos / 4 * piece_size))
    

let item_event config num ev =
  begin match ev with
  | `ENTER_NOTIFY _ ->
      let (_, text) = config.board.(num) in
      text#set [ `FILL_COLOR "white" ]
  | `LEAVE_NOTIFY _ ->
      let (_, text) = config.board.(num) in
      text#set [ `FILL_COLOR "black" ]
  | `BUTTON_PRESS _ ->
      let pos = config.pos.(num) in
      if List.mem (config.hole - pos) [ -1; 1; 4; -4; ]
      then
	let dpos = config.hole - pos in
	config.hole <- config.hole - dpos ;
	config.pos.(num) <- config.pos.(num) + dpos ;
	move config num dpos ;
	config.canvas#update_now ()
  | _ -> ()
  end ;
  false

let scramble_moves = 128

let array_find a v =
  let imax = Array.length a in
  let rec proc = function
    | i when i = imax -> raise Not_found
    | i when a.(i) = v -> i
    | i -> proc (succ i) in
  proc 0

let scramble config () =
  for i = 1 to scramble_moves do
    let new_pos = ref (-1) in
    let ok = ref false in
    while not !ok do
      let dpos = Array.get [| -1; 1; -4; 4|] (Random.int 4) in
      new_pos := config.hole + dpos ;
      if not ((config.hole mod 4 = 0 && dpos = -1) ||
              (config.hole mod 4 = 3 && dpos =  1) ||
              !new_pos < 0 || !new_pos > 15)
      then ok := true
    done ;
    let num = array_find config.pos !new_pos in
    move config num (config.hole - !new_pos) ;
    config.pos.(num) <- config.hole ;
    config.hole <- !new_pos ;
    config.canvas#update_now ()
  done


let create_canvas_fifteen window = 
  let vbox = GPack.vbox ~border_width:4 ~packing:window#add () in
  let align = GBin.alignment ~packing:vbox#add () in
  let frame = GBin.frame ~shadow_type:`IN ~packing:align#add () in
  let dim = piece_size * 4 + 1 in
  let canvas = GnoCanvas.canvas
      ~width:dim ~height:dim
      ~packing:frame#add () in
  canvas#set_scroll_region 0. 0. (float dim) (float dim) ;
  let board = Array.init 15
      (fun i ->
	let x = i mod 4 in
	let y = i / 4 in
	let tile = 
	  GnoCanvas.group 
	    ~x:(float (x * piece_size)) ~y:(float (y * piece_size))
	    canvas#root in
	GnoCanvas.rect tile
	  ~props:[ `X1 0.; `Y1 0. ; `X2 (float piece_size) ; `Y2 (float piece_size) ;
		   `FILL_COLOR (piece_color i) ; `OUTLINE_COLOR "black" ;
		   `WIDTH_PIXELS 0 ] ;
	let text = 
	  GnoCanvas.text tile
	    ~props:[ `TEXT (string_of_int (succ i)) ; 
		     `X (float piece_size /. 2.) ;
		     `Y (float piece_size /. 2.) ;
		     `FONT "Sans bold 24" ;
		     `FILL_COLOR "black" ;
		     `ANCHOR `CENTER ] in
	(tile, text)) in
  let config = {
    canvas = canvas ;
    board = board ;
    pos = Array.init 15 (fun i -> i) ;
    hole = 15 ;
  } in
  Array.iteri
    (fun i ((tile : GnoCanvas.group), _) -> tile#connect#event (item_event config i) ; ())
    config.board ;
  let button = GButton.button ~label:"Scramble" ~packing:vbox#add () in
  button#connect#clicked (scramble config)



let main_1 () =
  Random.self_init () ;
  let window = GWindow.window () in
  create_canvas_fifteen window ;
  window#connect#destroy ~callback:GMain.Main.quit ;
  window#show () ;
  GMain.Main.main ()

let _ = 
   main_1 ()


(* Local Variables: *)
(* compile-command: "ocamlopt -w s -i -I ../../src lablgtk.cmxa gtkInit.cmx lablgnomecanvas.cmxa canvas-fifteen.ml" *)
(* End: *)
