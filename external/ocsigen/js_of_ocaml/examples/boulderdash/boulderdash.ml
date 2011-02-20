
let (>>=) = Lwt.bind
module Html = Dom_html
let js = Js.string
let document = Html.window##document

let append_text e s = Dom.appendChild e (document##createTextNode (js s))
let replace_child p n =
  Js.Opt.iter (p##firstChild) (fun c -> Dom.removeChild p c);
  Dom.appendChild p n

let box_style =
  js"border: 1px black solid; background-color: white ; \
     display: inline ; padding-right: .5em; padding-left: .5em;"
let loading_style =
  js"background-color: red; color: white; display:inline; \
     position: absolute; top:0; right:0;"

let loading parent =
  let div = Html.createDiv document in
  div##style##cssText <- loading_style;
  append_text div "LOADING...";
  Dom.appendChild parent div;
  (fun () -> Dom.removeChild parent div)

let clock_div () =
  let t0 = ref (Sys.time ()) in
  let div = Html.createDiv document in
  div##style##cssText <- box_style;
  append_text div "--:--:--";
  let stopped = ref true in
  let rec update_cb () =
    let dt = Sys.time () -. !t0 in
    if not !stopped then begin
      let txt =
	document##createTextNode
	  (let secs = int_of_float dt in
             js (Printf.sprintf "%02d:%02d:%02d"
                   (secs / 3600) ((secs / 60) mod 60) (secs mod 60)))
      in
      replace_child div txt
    end;
    Lwt_js.sleep 1. >>= fun () ->
    update_cb ()
  in
    ignore (update_cb ()) ;
    (div,
     (fun () -> t0 := Sys.time () ; stopped := false),
     (fun () -> stopped := true))

type cell = Empty | Grass | Diamond | Boulder | Door | End | Guy | Wall | Bam
and state = {
  map : cell array array ;      imgs : Html.imageElement Js.t array array ;
  mutable pos : int * int ;     mutable endpos : int * int ;
  mutable rem : int ;            mutable dead : bool ;
  mutable map_mutex : Lwt_mutex.t ; mutable events_mutex : bool ;
  mutable pending_out_cb : (unit -> unit) option ref ;
}
exception Death

let img_assoc v =
  match v with
  | Empty   -> js"sprites/empty.png"
  | Bam     -> js"sprites/bam.png"
  | Grass   -> js"sprites/grass.png"
  | Diamond -> js"sprites/diamond.png"
  | Boulder -> js"sprites/boulder.png"
  | End     -> js"sprites/end.png"
  | Door    -> js"sprites/door.png"
  | Guy     -> js"sprites/guy.png"
  | Wall    -> js"sprites/wall.png"

let set_cell state x y v =
  state.map.(y).(x) <- v ;
  state.imgs.(y).(x)##src <- img_assoc v

let walkable = function | Empty | Grass | Diamond | End -> true | _-> false

let rec fall state =
  (* assumes wall borders *)
  let changed = ref false in
    for y = Array.length state.map - 2 downto 1 do
      for x = 1 to Array.length state.map.(y) - 2 do
	let sustaining = state.map.(y + 1).(x) = Guy && state.map.(y).(x) = Boulder in
	  if (state.map.(y).(x) = Empty
	      && state.map.(y - 1).(x) = Boulder) then (
	    set_cell state x (y - 1) Empty ;
	    set_cell state x y Boulder ;
	    changed := true
	  ) ;
	  if (state.map.(y).(x) = Empty
	      && state.map.(y - 1).(x) = Empty
	      && state.map.(y).(x - 1) = Boulder
	      && state.map.(y - 1).(x - 1) = Boulder) then (
	    set_cell state (x - 1) (y - 1) Empty ;
	    set_cell state x y Boulder ;
	    changed := true
	  ) ;
	  if (state.map.(y).(x) = Empty
	      && state.map.(y - 1).(x) = Empty
	      && state.map.(y).(x + 1) = Boulder
	      && state.map.(y - 1).(x + 1) = Boulder) then (
	    set_cell state (x + 1) (y - 1) Empty ;
	    set_cell state x y Boulder ;
	    changed := true
	  ) ;
	  if (not sustaining) && state.map.(y + 1).(x) = Guy && state.map.(y).(x) = Boulder then (
	    set_cell state x (y + 1) Bam ;
	    raise Death
	  )
      done
    done ;
    if !changed then begin
      Lwt_js.sleep 0.05 >>= fun () ->
      fall state
    end else
      Lwt.return ()

let rec build_interaction state show_rem ((_,_, clock_stop) as clock) =
  Lwt_mutex.lock state.map_mutex >>= fun () ->
    for y = 0 to Array.length state.map - 1 do
      for x = 0 to Array.length state.map.(y) - 1 do
	state.imgs.(y).(x)##onmouseover <- Html.no_handler;
	state.imgs.(y).(x)##onmouseout <- Html.no_handler;
	state.imgs.(y).(x)##onclick <- Html.no_handler
      done
    done ;
    let inhibit f _x =
      if not state.events_mutex then
        ignore
          (state.events_mutex <- true;
           f () >>= fun () ->
           state.events_mutex <- false;
           Lwt.return ());
      Js._false
    in
    let set_pending_out f out () =
      f () >>= fun () -> state.pending_out_cb := Some out; Lwt.return ()
    in
    let with_pending_out f () =
      match !(state.pending_out_cb) with
	| None -> f ()
	| Some out -> out () ; state.pending_out_cb := None ; f ()
    in
    let rec update (x, y) next img over_cont out_cont click_cont =
      if walkable state.map.(y).(x) then (
	let cur_img = state.imgs.(y).(x)##src in
	let over () =
          state.imgs.(y).(x)##src <- img;
          over_cont ()
	and out () =
          state.imgs.(y).(x)##src <- cur_img;
          out_cont ()
	and click' () =
	  click_cont () >>= fun () ->
	  if state.map.(y).(x) = Diamond then state.rem <- state.rem - 1 ;
	  set_cell state x y Guy ;
	  Lwt_js.sleep 0.05 >>= fun () ->
	  fall state >>= fun () ->
	  set_cell state x y Empty;
          Lwt.return ()
	in
	let click () =
	  let gx, gy = state.pos in
	    set_cell state gx gy Empty ;
	    (Lwt.catch (fun () ->
	       click_cont () >>= fun () ->
	       if state.map.(y).(x) = Diamond then state.rem <- state.rem - 1 ;
	       set_cell state x y Guy ;
	       state.pos <- (x,y) ;
	       fall state)
               (fun e ->
                  match e with
	            Death -> state.dead <- true; Lwt.return ()
                  | _     -> Lwt.fail e)) >>= fun () ->
	    build_interaction state show_rem clock
	in
	  state.imgs.(y).(x)##onmouseover <- Html.handler
	    (inhibit (set_pending_out (with_pending_out over) out)) ;
	  state.imgs.(y).(x)##onmouseout <- Html.handler
	    (inhibit (with_pending_out (fun () -> Lwt.return ()))) ;
	  state.imgs.(y).(x)##onclick <- Html.handler
	    (inhibit (with_pending_out click)) ;
	  if state.map.(y).(x) <> End then
	    update (next (x,y)) next img over out click'
      )
    in
    let update_push ((x, y) as pos) next img img_guy=
      let ((x', y') as pos') = next pos in
      let (x'', y'') = next pos' in
	if (try
	      state.map.(y').(x') = Boulder && state.map.(y'').(x'') = Empty
	    with Invalid_argument "index out of bounds" -> false) then (
	  let over () =
	    state.imgs.(y).(x)##src <- img_guy;
	    state.imgs.(y').(x')##src <- img;
            Lwt.return ()
	  in
	  let out () =
	    state.imgs.(y).(x)##src <- js"sprites/guy.png";
	    state.imgs.(y').(x')##src <- js"sprites/boulder.png"
	  in
	  let click () =
	    set_cell state x y Empty ;
	    set_cell state x' y' Guy ;
	    state.pos <- pos' ;
	    set_cell state x'' y'' Boulder ;
	    Lwt.catch
              (fun () -> fall state)
              (fun e ->
                 match e with
                   Death -> state.dead <- true; Lwt.return ()
                 | e     -> Lwt.fail e) >>= fun () ->
	    build_interaction state show_rem clock
	  in
	    state.imgs.(y').(x')##onmouseover <- Html.handler
	      (inhibit (set_pending_out (with_pending_out over) out));
	    state.imgs.(y').(x')##onmouseout <- Html.handler
	      (inhibit (with_pending_out (fun () -> Lwt.return ())));
	    state.imgs.(y').(x')##onclick <- Html.handler
	      (inhibit (with_pending_out click))
	)
    in
      if state.pos = state.endpos then (
	clock_stop () ; Html.window##alert (js"YOU WIN !")
      ) else
	if state.dead then (
	  clock_stop () ; Html.window##alert (js"YOU LOSE !")
	) else (
	  if state.rem = 0 then (
	    let x,y = state.endpos in
	      state.imgs.(y).(x)##src <- js"sprites/end.png";
	      state.map.(y).(x) <- End
	  ) ;
	  let r (x, y) = succ x, y and l (x, y) = pred x, y in
	  let u (x, y) = x, pred y and d (x, y) = x, succ y in
	  let nil_cont () = () in
	  let nil_cont_async () = Lwt.return () in
	    update (r state.pos) r (js"sprites/R.png")
              nil_cont_async nil_cont nil_cont_async ;
	    update (l state.pos) l (js"sprites/L.png")
              nil_cont_async nil_cont nil_cont_async ;
	    update (u state.pos) u (js"sprites/U.png")
              nil_cont_async nil_cont nil_cont_async ;
	    update (d state.pos) d (js"sprites/D.png")
              nil_cont_async nil_cont nil_cont_async ;
	    update_push state.pos r
              (js"sprites/bR.png") (js"sprites/push_r.png") ;
	    update_push state.pos l
              (js"sprites/bL.png") (js"sprites/push_l.png") ;
	    show_rem state.rem
	) ;
      Lwt_mutex.unlock state.map_mutex;
      Lwt.return ()

let opt_style e style =
  match style with Some s -> e##style##cssText <- s | None -> ()

let build_table ?style ?tr_style ?td_style f t =
  let m = Html.createTable document in
  opt_style m style;
  for y = 0 to Array.length t - 1 do
    let tr = m##insertRow (-1) in
    opt_style tr tr_style;
    for x = 0 to Array.length t.(y) - 1 do
      let td = tr##insertCell (-1) in
      opt_style td td_style;
      Dom.appendChild td (f y x t.(y).(x));
      Dom.appendChild tr td
    done ;
    Dom.appendChild m tr
  done;
  m

let http_get url =
  XmlHttpRequest.send_string url >>= fun r ->
  let cod = r.XmlHttpRequest.code in
  let msg = r.XmlHttpRequest.content in
  if cod = 0 || cod = 200
  then Lwt.return msg
  else fst (Lwt.wait ())

let start _ =
  let body =
    Js.Opt.get (document##getElementById(js"boulderdash"))
      (fun () -> assert false)
  in
  let board_div = Html.createDiv document in
  let (clock_div,clock_start,_) as clock = clock_div () in
  let load_data name process =
    let loading_end = loading body in
    http_get name >>= fun data ->
    process data >>= fun res ->
    loading_end ();
    Lwt.return res
  in
  let rem_div, show_rem =
    let div = Html.createDiv document in
    div##style##cssText <- box_style;
    append_text div "--";
    (div,
     fun v ->
       replace_child div
         (document##createTextNode (Js.string (string_of_int v))))
  in
  load_data
    "maps.txt"
    (fun txt ->
       let find_string st =
         let sz = String.length txt in
         let rec find_string_start s =
           if s >= sz then
             failwith "eos"
           else
             if txt.[s] == '"' then
      	 find_string_end (s + 1) (s + 2)
             else
      	 find_string_start (s + 1)
         and find_string_end s e =
           if s >= sz then
             failwith "eos"
           else
             if txt.[e] == '"' then
      	 (String.sub txt s (e - s), e + 1)
             else
      	 find_string_end s (e + 1)
         in find_string_start st
       in
       let rec scan_pairs st acc =
         match
           try
             let fst, st = find_string st in
             let snd, st = find_string st in
      	 Some ((fst, snd), st)
           with Failure "eos" -> None
         with
           | Some (elt, st) -> scan_pairs st (elt :: acc)
           | None -> acc
       in
       Lwt.return (List.rev (scan_pairs 0 []))) >>= fun levels ->
  let load_level file =
    load_data file
      (fun data ->
	 let map, cells =
	   let res = ref [] and row = ref [] in
	     for i = 0 to String.length data - 1 do
	       match data.[i] with
		 | '\n' -> res := List.rev (!row) :: !res ; row := []
		 | '#' -> row := Wall :: !row
		 | '.' -> row := Grass :: !row
		 | ' ' -> row := Empty :: !row
		 | '+' -> row := Diamond :: !row
		 | 'X' -> row := Boulder :: !row
		 | 'W' -> row := Guy :: !row
		 | 'E' -> row := Door :: !row | 'S' -> row := Guy :: !row
		 | _ -> failwith "malformed level"
	     done ;
	     let map = Array.of_list (List.map Array.of_list (List.rev !res)) in
	       map, Array.map (Array.map
				 (fun c ->
                                    let img = Html.createImg document in
                                    img##src <- img_assoc c;
                                    img)) map
	 in 
	 let gx = ref 0 and gy = ref 0 and ex = ref 0 and ey = ref 0 and rem = ref 0 in
         let style =
           js"border-collapse:collapse;line-height: 0; opacity: 0; \
              margin-left:auto; margin-right:auto"
         in
	 let td_style = js"padding: 0; width: 20px; height: 20px;" in
	 let table =
	   build_table ~style ~td_style
	     (fun y x cell ->
		begin match map.(y).(x) with
		| Guy     -> gx := x ; gy := y
	        | Diamond -> incr rem
		| Door    -> ex := x ; ey := y
		| _       -> ()
                end;
                cell)
	     cells
	 in
           replace_child board_div table;
	   build_interaction
	     { map = map; imgs = cells ; pos = (!gx, !gy) ; endpos = (!ex, !ey) ;
	       map_mutex = Lwt_mutex.create () ; events_mutex = false ;
	       dead = false ; rem = !rem ; pending_out_cb = ref None }
	     show_rem clock >>= fun () ->
	   let t0 = Sys.time () in
	   let rec fade () =
	     let t = Sys.time () in
	       if t -. t0 >= 1. then (
		 table##style##opacity <- Js.def (js"1");
                 Lwt.return ()
	       ) else (
		 Lwt_js.sleep 0.05 >>= fun () ->
		 table##style##opacity <- Js.def
		   (js (Printf.sprintf "%g" (t -. t0))) ;
		 fade ()
	       )
	   in fade () >>= fun () -> clock_start (); Lwt.return ()
      )
  in
    body##style##cssText <-
      js"font-family: sans-serif; text-align: center; \
         background-color: #e8e8e8;" ;
    let h1 = Html.createH1 document in
    append_text h1 "Boulder Dash in Ocaml";
    Dom.appendChild body h1;
    let div = Html.createDiv document in
    append_text div "Elapsed time: ";
    Dom.appendChild div clock_div;
    append_text div " Remaining diamonds: ";
    Dom.appendChild div rem_div;
    append_text div " ";
    let select = Html.createSelect document in
    let option = Html.createOption document in
    append_text option "Choose a level";
    Dom.appendChild select option;
    List.iter
      (fun (f, n) ->
         let option = Html.createOption document in
         append_text option n;
(*
         option##onclick <-
           some (fun _ -> ignore (load_level f); Js._false);
*)
         Dom.appendChild select option)
      levels;
    select##onchange <- Html.handler
      (fun _ ->
         let i = select##selectedIndex - 1 in
         if i >= 0 && i < List.length levels then
           ignore (load_level (fst (List.nth levels i)));
         Js._false);
    Dom.appendChild div select;
    Dom.appendChild div (Html.createBr document);
    Dom.appendChild div (Html.createBr document);
    Dom.appendChild div board_div;
    Dom.appendChild body div;
    Lwt.return ()

let _ =
Html.window##onload <- Html.handler (fun _ -> ignore (start ()); Js._false)

