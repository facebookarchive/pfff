
module Html = Dom_html

let js = Js.string
let document = Html.window##document

type config = {
  nbcols  : int ;
  nbrows : int ;
  nbmines : int }

let default_config = { nbcols=10; nbrows=10; nbmines=15 }

type cell = {
  mutable mined : bool ;
  mutable seen : bool ;
  mutable flag : bool ;
  mutable nbm : int
}


type board = cell array array

let iter_on_cell cf f =
  for i=0 to cf.nbcols-1 do for j=0 to cf.nbrows-1 do f (i,j) done done

let random_list_mines lc m =
  let cell_list = ref []
  in while (List.length !cell_list) < m do
      let n = Random.int lc in
      if not (List.mem n !cell_list) then cell_list := n :: !cell_list
    done ;
  !cell_list

let generate_seed () =
  let t = Sys.time () in
  let n = int_of_float (t*.1000.0)
  in Random.init(n mod 100000)


let valid cf (i,j) = i>=0 && i<cf.nbcols && j>=0 && j<cf.nbrows

let neighbours cf (x,y) =
  let ngb = [x-1,y-1; x-1,y; x-1,y+1; x,y-1; x,y+1; x+1,y-1; x+1,y; x+1,y+1]
  in List.filter (valid cf) ngb


let initialize_board cf =
  let cell_init () = { mined=false; seen=false; flag=false; nbm=0 } in
  let copy_cell_init b (i,j) = b.(i).(j) <- cell_init() in
  let set_mined b n = b.(n / cf.nbrows).(n mod cf.nbrows).mined <- true
  in
  let count_mined_adj b (i,j) =
    let x = ref 0 in
    let inc_if_mined (i,j) = if b.(i).(j).mined then incr x
  in List.iter inc_if_mined (neighbours cf (i,j)) ;
  !x
in
let set_count b (i,j) =
  if not b.(i).(j).mined
  then b.(i).(j).nbm <- count_mined_adj b (i,j)
in
let list_mined = random_list_mines (cf.nbcols*cf.nbrows) cf.nbmines in
let board = Array.make_matrix cf.nbcols cf.nbrows (cell_init ())
in iter_on_cell cf (copy_cell_init board) ;
List.iter (set_mined board) list_mined ;
iter_on_cell cf (set_count board) ;
board

let cells_to_see bd cf (i,j) =
  let visited = Array.make_matrix cf.nbcols cf.nbrows false in
  let rec relevant = function
      [] -> ([],[])
    | ((x,y) as c)::l ->
        let cell=bd.(x).(y)
        in if cell.mined || cell.flag || cell.seen || visited.(x).(y)
        then relevant l
          else let (l1,l2) = relevant l
            in visited.(x).(y) <- true ;
          if cell.nbm=0 then (l1,c::l2) else (c::l1,l2)
  in
  let rec cells_to_see_rec = function
      [] -> []
    | ((x,y) as c)::l ->
        if bd.(x).(y).nbm<>0 then c :: (cells_to_see_rec l)
        else let (l1,l2) = relevant (neighbours cf c)
          in  (c :: l1)  @  (cells_to_see_rec (l2 @ l))
  in visited.(i).(j) <- true ;
  cells_to_see_rec [(i,j)]

let b0 = 3
let l1 = 15
let l2 = l1
let l4 = 20 + 2*b0
let l3 = l4*default_config.nbcols + 2*b0
let l5 = 40 + 2*b0


let h1 = l1
let h2 = 30
let h3 = l5+20 + 2*b0
let h4 = h2
let h5 = 20 + 2*b0
let h6 = l5 + 2*b0


type demin_cf =
    { bd : cell array array;
      dom : Html.imageElement Js.t array array;
      cf : config ;
      mutable nb_marked_cells : int;
      mutable nb_hidden_cells : int;
      mutable flag_switch_on : bool }

let draw_cell dom bd =
  dom##src <-
    js (if bd.flag then "sprites/flag.png"
        else if bd.mined then "sprites/bomb.png"
        else if bd.seen then (
          if bd.nbm = 0 then "sprites/empty.png"
          else "sprites/" ^ string_of_int bd.nbm ^ ".png"
        )
        else "sprites/normal.png")

let draw_board d =
  for y = 0 to d.cf.nbrows - 1 do
    for x = 0 to d.cf.nbcols - 1 do
      draw_cell d.dom.(y).(x) d.bd.(x).(y)
    done
  done

let disable_events d =
  for y = 0 to d.cf.nbrows - 1 do
    for x = 0 to d.cf.nbcols - 1 do
      d.dom.(y).(x)##onclick <- Html.handler
        (fun _ -> Html.window##alert (js"GAME OVER"); Js._false)
    done
  done

let mark_cell d i j =
  if d.bd.(i).(j).flag
  then ( d.nb_marked_cells <- d.nb_marked_cells -1;
         d.bd.(i).(j).flag <- false )
  else ( d.nb_marked_cells <- d.nb_marked_cells +1 ;
         d.bd.(i).(j).flag <- true ) ;
  draw_cell d.dom.(j).(i) d.bd.(i).(j)

let reveal d i j =
  let reveal_cell (i,j) =
    d.bd.(i).(j).seen <- true ;
    draw_cell d.dom.(j).(i) d.bd.(i).(j) ;
    d.nb_hidden_cells <- d.nb_hidden_cells -1
  in
  List.iter reveal_cell (cells_to_see d.bd d.cf (i,j)) ;
  if d.nb_hidden_cells = 0 then (
    draw_board d ;
    disable_events d ;
    Html.window##alert (js"YOU WIN")
  )

let create_demin nb_c nb_r nb_m =
  let nbc = max default_config.nbcols nb_c
  and nbr = max default_config.nbrows nb_r in
  let nbm = min (nbc*nbr) (max 1 nb_m) in
  let cf = { nbcols=nbc ; nbrows=nbr ; nbmines=nbm } in
  generate_seed () ;
  { cf = cf ;
    bd = initialize_board cf;
    dom = Array.create nbr [||] ;
    nb_marked_cells = 0;
    nb_hidden_cells = cf.nbrows*cf.nbcols-cf.nbmines;
    flag_switch_on = false }

type mode = Normal | Flag

let init_table d board_div =
  let mode = ref Normal in
  let buf = document##createDocumentFragment() in
  Dom.appendChild buf (document##createTextNode(js"Mode : "));
  let img = Html.createImg document in
  Dom.appendChild buf img;
  img##src <- js"sprites/bomb.png" ;
  img##onclick <- Html.handler
    (fun _ ->
       begin match !mode with
         | Normal -> mode := Flag ; img##src <- js"sprites/flag.png"
         | Flag   -> mode := Normal ; img##src <- js"sprites/bomb.png"
       end;
       Js._false
    ) ;
  Dom.appendChild buf (Html.createBr document);
  for y = 0 to d.cf.nbrows - 1 do
    let imgs = ref [] in
    for x = 0 to d.cf.nbcols - 1 do
      let img = Html.createImg document in
      imgs := img :: !imgs ;
      img##src <- js"sprites/normal.png";
      img##onclick <- Html.handler
        (fun _ ->
          (match !mode with
            | Normal ->
                if d.bd.(x).(y).seen then ()
                else if d.flag_switch_on then mark_cell d x y
                else if d.bd.(x).(y).flag then ()
                else if d.bd.(x).(y).mined then (
                  draw_board d ;
                  disable_events d ;
                  Html.window##alert (js"YOU LOSE")
                ) else reveal d x y
            | Flag ->
                d.bd.(x).(y).flag <- not d.bd.(x).(y).flag ;
                draw_cell img d.bd.(x).(y));
          Js._false);
      Dom.appendChild buf img;
    done ;
    Dom.appendChild buf (Html.createBr document);
    d.dom.(y) <- Array.of_list (List.rev !imgs)
  done ;
  board_div##style##lineHeight <- js"0";
  Dom.appendChild board_div buf

let run div nbc nbr nbm =
  let d = create_demin nbc nbr nbm in
  init_table d div
