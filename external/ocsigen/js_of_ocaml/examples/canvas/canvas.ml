module D = Dom_html

let onload _ =
  let c = D.createCanvas D.document in
  c##width <- 150; c##height <- 150;
  Dom.appendChild D.document##body c;

  let ctx = c##getContext (D._2d_) in

  ctx##fillStyle <- Js.string "rgb(200,0,0)";
  ctx##fillRect (10., 10., 55., 50.);

  ctx##fillStyle <- Js.string "rgba(0,0,200,0.5)";
  ctx##fillRect (30., 30., 55., 50.);

  ctx##beginPath ();
  ctx##moveTo (75., 25.);
  ctx##quadraticCurveTo (25., 25., 25., 62.5);
  ctx##quadraticCurveTo (25., 100., 50., 100.);
  ctx##quadraticCurveTo (50., 120., 30., 125.);
  ctx##quadraticCurveTo (60., 120., 65., 100.);
  ctx##quadraticCurveTo (125., 100., 125., 62.5);
  ctx##quadraticCurveTo (125., 25., 75., 25.);
  ctx##stroke ();
  Js._false

let _ = D.window##onload <- D.handler onload
