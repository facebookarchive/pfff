(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)

let big_text = 
  String.concat "" 
    [ "English is so boring because everyone uses it.\n" ;
      "Here is something exciting:  " ;
      "وقد بدأ ثلاث من أكثر المؤسسات تقدما في شبكة اكسيون برامجها كمنظمات لا تسعى للربح، ثم تحولت في السنوات الخمس الماضية إلى مؤسسات مالية منظمة، وباتت جزءا من النظام المالي في بلدانها، ولكنها تتخصص في خدمة قطاع المشروعات الصغيرة. وأحد أكثر هذه المؤسسات نجاحا هو »بانكوسول« في بوليفيا.\n" ;
      "And here is some more plain, boring English." ; ]


let setup_text root =
  let r = GnoCanvas.rect root
      ~props:[ `X1 (-90.) ; `Y1 (-50.) ;
	       `X2 110. ; `Y2 50. ;
	       `FILL_COLOR "green" ;
	       `OUTLINE_COLOR "green" ] in
  GnoCanvas.rich_text root
    ~x:(-90.) ~y:(-50.)
    ~width:200. ~height:100.
    ~text:big_text ~props:[ `GROW_HEIGHT true ] ;
  GnoCanvas.ellipse root
    ~props:[ `X1 (-5.) ; `Y1 (-5.) ;
	     `X2 5. ; `Y2 5. ;
	     `FILL_COLOR "white" ] ;
  GnoCanvas.rect root
    ~props:[ `X1 100. ; `Y1 (-30.) ;
	     `X2 200. ; `Y2 30. ;
	     `FILL_COLOR "yellow" ;
	     `OUTLINE_COLOR "yellow" ] ;
  GnoCanvas.rich_text root
    ~x:100. ~y:(-30.) ~width:100. ~height:60.
    ~text:"The quick brown fox jumped over the lazy dog.\n"
    ~props:[ `GROW_HEIGHT true ; 
	     `CURSOR_VISIBLE true ;
	     `CURSOR_BLINK true ] ;
  GnoCanvas.rect root
    ~props:[ `X1 50. ; `Y1 70. ;
	     `X2 150. ; `Y2 100. ;
	     `FILL_COLOR "pink" ;
	     `OUTLINE_COLOR "pink" ] ;
    GnoCanvas.rich_text root
    ~x:50. ~y:70. ~width:100. ~height:30.
    ~text:"This is a test.\nI enjoy tests a great deal\nThree lines!"
    ~props:[ `CURSOR_VISIBLE true ;
	     `CURSOR_BLINK true ] ;
  ()


let create_canvas_rich_text window =
  let vbox = GPack.vbox ~spacing:4 ~border_width:4 ~packing:window#add () in
  let align = GBin.alignment ~packing:vbox#pack () in
  let frame = GBin.frame ~shadow_type:`IN ~packing:align#add () in
  let canvas = GnoCanvas.canvas ~width:600 ~height:450 ~packing:frame#add () in
  setup_text canvas#root


let main_1 () =
  Random.self_init () ;
  let window = GWindow.window () in
  create_canvas_rich_text window ;
  window#connect#destroy ~callback:GMain.Main.quit ;
  window#show () ;
  GMain.Main.main ()

let _ = 
   main_1 ()


(* Local Variables: *)
(* coding: utf-8 *)
(* compile-command: "ocamlopt -w s -i -I ../../src lablgtk.cmxa gtkInit.cmx lablgnomecanvas.cmxa canvas-richtext.ml" *)
(* End: *)
