(* Graph viewer
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

type rect = {x : int; y : int; width : int; height: int}

module Html = Dom_html

let create_canvas w h =
  let c = Html.createCanvas Html.document in
  c##width <- w; c##height <- h; c

module Common = Viewer_common.F (struct
  type font = Js.js_string Js.t
  type color = Js.js_string Js.t
  type text = Js.js_string Js.t
  let white = Js.string "white"

  type ctx = Html.canvasRenderingContext2D Js.t

  let save ctx = ctx##save ()
  let restore ctx = ctx##restore ()

  let scale ctx ~sx ~sy = ctx##scale (sx, sy)
  let translate ctx ~tx ~ty = ctx##translate (tx, ty)

  let begin_path ctx = ctx##beginPath ()
  let close_path ctx = ctx##closePath ()
  let move_to ctx ~x ~y = ctx##moveTo (x, y)
  let line_to ctx ~x ~y = ctx##lineTo (x, y)
  let curve_to ctx ~x1 ~y1 ~x2 ~y2 ~x3 ~y3 =
    ctx##bezierCurveTo (x1, y1, x2, y2, x3, y3)
  let arc ctx ~xc ~yc ~radius ~angle1 ~angle2 =
    ctx##arc (xc, yc, radius, angle1, angle2, Js._true)
  let rectangle ctx ~x ~y ~width ~height = ctx##rect (x, y, width, height)

  let fill ctx c = ctx##fillStyle <- c; ctx##fill ()
  let stroke ctx c = ctx##strokeStyle <- c; ctx##stroke ()
  let clip ctx = ctx##clip ()

  let draw_text (ctx:ctx) x y txt font fill_color stroke_color =
     ctx##font <- font;
     ctx##textAlign <- Js.string "center";
     begin match fill_color with
       Some c -> ctx##fillStyle <- c; ctx##fillText (txt, x, y)
     | None   -> ()
     end;
     begin match stroke_color with
       Some c -> ctx##strokeStyle <- c; ctx##strokeText (txt, x, y)
     | None   -> ()
     end

  type window = Html.canvasElement Js.t
  type drawable = window * ctx
  type pixmap = drawable
  let get_drawable w =
    let ctx = w##getContext(Html._2d_) in
    ctx##lineWidth <- 2.;
    (w, ctx)
  let make_pixmap _ width height =
    let c = Html.createCanvas Html.document in
    c##width <- width; c##height <- height;
    get_drawable c
  let drawable_of_pixmap p = p
  let get_context (p, c) = c
  let put_pixmap ~dst:((p, c) :drawable) ~x ~y ~xsrc ~ysrc ~width ~height ((p, _) : pixmap)=
    c##drawImage_fullFromCanvas (p, float xsrc, float ysrc, float width, float height, float x, float y, float width, float height)

  (****)

  type rectangle = rect = {x : int; y : int; width : int; height: int}
  let compute_extents _ = assert false
end)
open Common

let redraw st s h v (canvas : Html.canvasElement Js.t) =
  let width = canvas##width in
  let height = canvas##height in
(*Firebug.console##time (Js.string "draw");*)
  redraw st s h v canvas
    {x = 0; y = 0; width = width; height = height} 0 0 width height;
  begin try
    ignore (canvas##getContext(Html._2d_)##getImageData (0., 0., 1., 1.)) 
  with _ -> () end
(*
;Firebug.console##timeEnd (Js.string "draw")
;Firebug.console##log_2 (Js.string "draw", Js.date##now())
*)

let (>>=) = Lwt.bind

let http_get url =
  XmlHttpRequest.send_string url
    >>= fun {XmlHttpRequest.code = cod; content = msg} ->
  if cod = 0 || cod = 200
  then Lwt.return msg
  else fst (Lwt.wait ())

let json : < parse : Js.js_string Js.t -> 'a> Js.t = Js.Unsafe.variable "JSON"

class adjustment
    ?(value=0.) ?(lower=0.) ?(upper=100.)
    ?(step_incr=1.) ?(page_incr = 10.) ?(page_size = 10.) () =
  object
    val mutable _value = value method value = _value
    val mutable _lower = lower method lower = _lower
    val mutable _upper = upper method upper = _upper
    val mutable _step_incr = step_incr method step_increment = _step_incr
    val mutable _page_incr = page_incr method page_increment = _page_incr
    val mutable _page_size = page_size method page_size = _page_size
    method set_value v = _value <- v
    method set_bounds ?lower ?upper ?step_incr ?page_incr ?page_size () =
      begin match lower with Some v -> _lower <- v | None -> () end;
      begin match upper with Some v -> _upper <- v | None -> () end;
      begin match step_incr with Some v -> _step_incr <- v | None -> () end;
      begin match page_incr with Some v -> _page_incr <- v | None -> () end;
      begin match page_size with Some v -> _page_size <- v | None -> () end
  end

let handle_drag element f =
  let mx = ref 0 in
  let my = ref 0 in
  element##onmousedown <- Html.handler
    (fun ev ->
       mx := ev##clientX; my := ev##clientY;
       element##style##cursor <- Js.string "move";
       let c1 =
         Html.addEventListener Html.document Html.Event.mousemove
           (Html.handler
              (fun ev ->
                 let x = ev##clientX and y = ev##clientY in
                 let x' = !mx and y' = !my in
                 mx := x; my := y;
                 f (x -  x') (y - y');
                 Js._true))
           Js._true
       in
       let c2 = ref Js.null in
       c2 := Js.some
         (Html.addEventListener Html.document Html.Event.mouseup
            (Html.handler
               (fun _ ->
                  Html.removeEventListener c1;
                  Js.Opt.iter !c2 Html.removeEventListener;
                  (* "auto" would be better, but does not seem to work
                     with Opera *)
                  element##style##cursor <- Js.string "default";
                  Js._true))
            Js._true);
       (* We do not want to disable the default action on mouse down
          (here, keyboard focus)
          in this example. *)
       Js._true)

let start () =
  let doc = Html.document in
  let page = doc##documentElement in
  page##style##overflow <- Js.string "hidden";
  doc##body##style##overflow <- Js.string "hidden";
  doc##body##style##margin <- Js.string "0px";

  let started = ref false in
  let p = Html.createP doc in
  p##innerHTML <- Js.string "Loading graph...";
  p##style##display <- Js.string "none";
  Dom.appendChild doc##body p;
  ignore
    (Lwt_js.sleep 0.5 >>= fun () ->
     if not !started then p##style##display <- Js.string "inline";
     Lwt.return ());

(*
  Firebug.console##time(Js.string "loading");
*)
  http_get "scene.json" >>= fun s ->
(*
  Firebug.console##timeEnd(Js.string "loading");
  Firebug.console##time(Js.string "parsing");
*)
  let ((x1, y1, x2, y2), bboxes, scene) = json##parse (Js.string s) in
(*
  Firebug.console##timeEnd(Js.string "parsing");
  Firebug.console##time(Js.string "init");
*)

  started := true;
  Dom.removeChild doc##body p;

  let st =
    { bboxes = bboxes;
      scene = scene;
      zoom_factor = 1. /. 20.;
      st_x = x1; st_y = y1; st_width = x2 -. x1; st_height = y2 -. y1;
      st_pixmap = Common.make_pixmap () }
  in

  let canvas = create_canvas (page##clientWidth) (page##clientHeight) in
  Dom.appendChild doc##body canvas;
  let allocation () =
    {x = 0; y = 0; width = canvas##width; height = canvas##height} in

  let hadj = new adjustment () in
  let vadj = new adjustment () in
  let sadj =
    new adjustment ~upper:20. ~step_incr:1. ~page_incr:0. ~page_size:0. () in
  let zoom_steps = 8. in (* Number of steps to get a factor of 2 *)
  let set_zoom_factor f =
    let count = ceil (log f /. log 2. *. zoom_steps) in
    let f = 2. ** (count /. zoom_steps) in
    sadj#set_bounds ~upper:count ();
    st.zoom_factor <- f
  in
  let get_scale () = 2. ** (sadj#value /. zoom_steps) /. st.zoom_factor in

  let redraw_queued = ref false in
  let update_view force =
(*
Firebug.console##log_2(Js.string "update", Js.date##now());
*)
    let a = allocation () in
    let scale = get_scale () in
    let aw = ceil (float a.width /. scale) in
    let ah = ceil (float a.height /. scale) in
    hadj#set_bounds ~step_incr:(aw /. 20.) ~page_incr:(aw /. 2.)
      ~page_size:(min aw st.st_width) ~upper:st.st_width ();
    let mv = st.st_width -. hadj#page_size in
    if hadj#value < 0. then hadj#set_value 0.;
    if hadj#value > mv then hadj#set_value mv;
    vadj#set_bounds ~step_incr:(ah /. 20.) ~page_incr:(ah /. 2.)
      ~page_size:(min ah st.st_height) ~upper:st.st_height ();
    let mv = st.st_height -. vadj#page_size in
    if vadj#value < 0. then vadj#set_value 0.;
    if vadj#value > mv then vadj#set_value mv;

    if force then redraw st (get_scale ()) hadj#value vadj#value canvas else
    if not !redraw_queued then
      ignore (redraw_queued := true;
(*
Firebug.console##log(Js.string "sleep");
*)
              Lwt_js.yield() >>= fun () ->
              redraw_queued := false;
              redraw st (get_scale ()) hadj#value vadj#value canvas;
              Lwt.return ())
  in

  let a = allocation () in
  let zoom_factor =
    max (st.st_width /. float a.width)
        (st.st_height /. float a.height)
  in
  set_zoom_factor zoom_factor;

  let prev_scale = ref (get_scale ()) in
  let rescale x y =
    let scale = get_scale () in
    let r = (1. -. !prev_scale /. scale) in
    hadj#set_value (hadj#value +. hadj#page_size *. r *. x);
    vadj#set_value (vadj#value +. vadj#page_size *. r *. y);
    prev_scale := scale;
    invalidate_pixmap st.st_pixmap;
    update_view false
  in

  let size = 16 in
  let height = 300 - size in
  let points d = Js.string (Printf.sprintf "%dpx" d) in
  let size_px = points size in
  let pos = ref height in
  let thumb = Html.createDiv doc in
  let style = thumb##style in
  style##position <- Js.string "absolute";
  style##width <- size_px;
  style##height <- size_px;
  style##top <- points !pos;
  style##left <- Js.string "0px";
  style##margin <- Js.string "1px";
  style##backgroundColor <- Js.string "black";
  let slider = Html.createDiv doc in
  let style = slider##style in
  style##position <- Js.string "absolute";
  style##width <- size_px;
  style##height <- points (height + size);
  style##border <- Js.string "2px solid black";
  style##padding <- Js.string "1px";
  style##top <- Js.string "10px";
  style##left <- Js.string "10px";
  Dom.appendChild slider thumb;
  Dom.appendChild doc##body slider;
  let set_slider_position pos' =
    if pos' <> !pos then begin
      thumb##style##top <- points pos';
      pos := pos';
      sadj#set_value (float (height - pos') *. sadj#upper /. float height);
      rescale 0.5 0.5
    end
  in
  handle_drag thumb
    (fun dx dy ->
       set_slider_position (min height (max 0 (!pos + dy))));
  slider##onmousedown <- Html.handler
    (fun ev ->
       let ey = ev##clientY in
       let (_, sy) = Dom_html.elementClientPosition slider in
       set_slider_position (max 0 (min height (ey - sy - size / 2)));
       Js._false);
  let adjust_slider () =
    let pos' =
      height - truncate (sadj#value *. float height /. sadj#upper +. 0.5) in
    thumb##style##top <- points pos';
    pos := pos'
  in

  Html.window##onresize <- Html.handler
    (fun _ ->
       let page = doc##documentElement in
       canvas##width <- page##clientWidth;
       canvas##height <- page##clientHeight;
       update_view true;
       Js._true);

  (* Drag the graph using the mouse *)
  handle_drag canvas
    (fun dx dy ->
       let scale = get_scale () in
       let offset a d =
         a#set_value
           (min (a#value -. float d /. scale) (a#upper -. a#page_size)) in
       offset hadj dx;
       offset vadj dy;
       update_view true);

  let bump_scale x y v =
    let a = allocation () in
    let x = x /. float a.width in
    let y = y /. float a.height in
    let prev = sadj#value in
    let vl =
      min (sadj#upper) (max (sadj#lower) (prev +. v *. sadj#step_increment)) in
    if vl <> prev then begin
      sadj#set_value vl;
      adjust_slider ();
      if x >= 0. && x <= 1. && y >= 0. && y <= 1. then
        rescale x y
      else
        rescale 0.5 0.5
    end;
    Js._false
  in

  (* Zoom using the mouse wheel *)
  ignore (Html.addMousewheelEventListener canvas
    (fun ev ~dx ~dy ->
       let (ex, ey) = Dom_html.elementClientPosition canvas in
       let x = float (ev##clientX - ex) in
       let y = float (ev##clientY - ey) in
       if dy < 0 then
         bump_scale x y 1.
       else if dy > 0 then
         bump_scale x y (-1.)
       else
         Js._false)
     Js._true);

(*
  Html.addEventListener Html.document Html.Event.keydown
    (Html.handler
       (fun e -> Firebug.console##log(e##keyCode);
         Js._true))
    Js._true;
*)
(*
  Html.addEventListener Html.document Html.Event.keypress
    (Html.handler
       (fun e ->
             Firebug.console##log(Js.string "press");
         match e##keyCode with
         | 37 -> (* left *)
             Js._false
         | 38 -> (* up *)
             Js._false
         | 39 -> (* right *)
             Js._false
         | 40 -> (* down *)
             Js._false
         | _ ->
             Firebug.console##log(- 1- e##keyCode);
             Js._true))
    Js._true;
*)
  let handle_key_event ev =
    match ev##keyCode with
      37 -> (* left *)
        hadj#set_value (hadj#value -. hadj#step_increment);
        update_view false;
        Js._false
    | 38 -> (* up *)
        vadj#set_value (vadj#value -. vadj#step_increment);
        update_view false;
        Js._false
    | 39 -> (* right *)
        hadj#set_value (hadj#value +. hadj#step_increment);
        update_view false;
        Js._false
    | 40 -> (* down *)
        vadj#set_value (vadj#value +. vadj#step_increment);
        update_view false;
        Js._false
    | _ ->
(*
        Firebug.console##log_2(Js.string "keycode:", ev##keyCode);
*)
        Js._true
  in
  let ignored_keycode = ref (-1) in
  Html.document##onkeydown <-
      (Html.handler
         (fun e ->
            ignored_keycode := e##keyCode;
            handle_key_event e));
  Html.document##onkeypress <-
      (Html.handler
         (fun e ->
            let k = !ignored_keycode in
            ignored_keycode := -1;
             if e##keyCode = k then Js._true else handle_key_event e));


(*
Firebug.console##time(Js.string "initial drawing");
*)
  update_view true;
(*
Firebug.console##timeEnd(Js.string "initial drawing");
Firebug.console##timeEnd(Js.string "init");
*)

  Lwt.return ()

let _ =
Html.window##onload <- Html.handler (fun _ -> ignore (start ()); Js._false)
