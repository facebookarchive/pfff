open Shared
open Event_arrows

let draw ctx (color, size, (x1, y1), (x2, y2)) =
  ctx##strokeStyle <- (Js.string color);
  ctx##lineWidth <- float size;
  ctx##beginPath();
  ctx##moveTo(float x1, float y1);
  ctx##lineTo(float x2, float y2);
  ctx##stroke()

let launch_client_canvas bus imageservice canvas_box =
  let canvas = Dom_html.createCanvas Dom_html.document in
  let ctx = canvas##getContext (Dom_html._2d_) in
  canvas##width <- width; canvas##height <- height;
  ctx##lineCap <- Js.string "round";

  (* The initial image: *)
  let img = Dom_html.createImg Dom_html.document in
  img##alt <- Js.string "canvas";
  img##src <- Js.string (Eliom_output.Xhtml5.make_string_uri ~service:imageservice ());
  img##onload <- Dom_html.handler (fun ev -> ctx##drawImage(img, 0., 0.); Js._false);

  Dom.appendChild canvas_box canvas;

  (* Size of the brush *)
  let slider = jsnew Goog.Ui.slider(Js.null) in
  slider##setMinimum(1.);
  slider##setMaximum(80.);
  slider##setValue(10.);
  slider##setMoveToPointEnabled(Js._true);
  slider##render(Js.some canvas_box);
  
  (* The color palette: *)
  let pSmall = 
    jsnew Goog.Ui.hsvPalette(Js.null, Js.null,
                             Js.some (Js.string "goog-hsv-palette-sm"))
  in
  pSmall##render(Js.some canvas_box);

  let x = ref 0 and y = ref 0 in
  let set_coord ev =
    let x0, y0 = Dom_html.elementClientPosition canvas in
    x := ev##clientX - x0; y := ev##clientY - y0 in
  let compute_line ev = 
    let oldx = !x and oldy = !y in
    set_coord ev;
    let color = Js.to_string (pSmall##getColor()) in
    let size = int_of_float (Js.to_float (slider##getValue())) in
    (color, size, (oldx, oldy), (!x, !y))
  in
  let line ev =
    let v = compute_line ev in
    let _ = Eliom_client_bus.write bus v in
    draw ctx v
  in
  let _ = Lwt_stream.iter (draw ctx) (Eliom_client_bus.stream bus) in
  ignore (run (mousedowns canvas
		 (arr (fun ev -> set_coord ev; line ev)
			      >>> first [mousemoves Dom_html.document (arr line);
					 mouseup Dom_html.document >>> (arr line)])) ())
