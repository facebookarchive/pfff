(**************************************************************************)
(*  cairo-ocaml -- Objective Caml bindings for Cairo                      *)
(*  Copyright © 2004-2005 Olivier Andrieu                                 *)
(*                                                                        *)
(*  This code is free software and is licensed under the terms of the     *)
(*  GNU Lesser General Public License version 2.1 (the "LGPL").           *)
(**************************************************************************)

let size = 20.

let cairo_path cr = function
  | [] -> invalid_arg "empty path"
  | (x, y) :: t ->
      Cairo.move_to cr x y ;
      List.iter 
	(fun (x, y) -> 
	  Cairo.rel_line_to cr x y) t ;
      Cairo.close_path cr

let triangle cr =
  cairo_path cr
    [ size, 0. ;
      size, (2. *. size) ;
      ((-2.) *. size), 0. ]
let square cr =
  cairo_path cr
    [ 0., 0. ;
      (2. *. size), 0. ;
      0., (2. *. size);
      ((-2.) *. size), 0. ]
let bowtie cr =
  cairo_path cr
    [ 0., 0. ;
      (2. *. size), (2. *. size) ;
      ((-2.) *. size), 0. ;
      (2. *. size), ((-2.) *. size) ]
let inf cr =
  Cairo.move_to cr 0. size ;
  Cairo.rel_curve_to cr 0. size size size (2. *. size) 0. ;
  Cairo.rel_curve_to cr size (~-. size) (2. *. size) (~-. size) (2. *. size) 0. ;
  Cairo.rel_curve_to cr 0. size (~-. size) size ((-2.) *. size) 0. ;
  Cairo.rel_curve_to cr (~-. size) (~-. size) ((-2.) *. size) (~-. size) ((-2.) *. size) 0. ;
  Cairo.close_path cr

let draw_shapes cr x y fill =
  let paint = if fill then Cairo.fill else Cairo.stroke in
  Cairo.save cr ; begin
    Cairo.new_path cr ;
    Cairo.translate cr (x +. size) (y +. size) ;
    List.iter
      (fun draw ->
	draw cr ; 
	paint cr ;
	Cairo.new_path cr ;
	Cairo.translate cr (4. *. size) 0.)
      [ bowtie ; square ; triangle; inf ] end ;
  Cairo.restore cr

let pi = 4. *. atan 1.

let redraw (px : GDraw.pixmap) =
  begin
    px#set_foreground `BLACK ;
    let width, height = px#size in
    px#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ()
  end ;
  let cr = Cairo_lablgtk.create px#pixmap in
  Cairo.set_source_rgb cr 1. 1. 1. ;

  Cairo.save cr ; begin 
    Cairo.set_font_size cr 20. ;
    Cairo.move_to cr 10. 10. ;
    Cairo.rotate cr (pi /. 2.) ;
    Cairo.show_text cr "Hello World !" end ;
  Cairo.restore cr ;
  
  Cairo.set_line_width cr (size /. 4.) ;
  Cairo.set_tolerance cr 1. ;
  
  Cairo.set_line_join cr Cairo.LINE_JOIN_ROUND ;
  Cairo.set_dash cr [| size /. 4. ; size /. 4. |] 0. ;
  draw_shapes cr 0. 0. false ;
  Cairo.translate cr 0. (4. *. size) ;

  Cairo.set_dash cr [||] 0. ;
  draw_shapes cr 0. 0. false ;
  Cairo.translate cr 0. (4. *. size) ;

  Cairo.set_line_join cr Cairo.LINE_JOIN_BEVEL ;
  draw_shapes cr 0. 0. false ;
  Cairo.translate cr 0. (4. *. size) ;

  Cairo.set_line_join cr Cairo.LINE_JOIN_MITER ;
  draw_shapes cr 0. 0. false ;
  Cairo.translate cr 0. (4. *. size) ;
    
  draw_shapes cr 0. 0. true ;
  Cairo.translate cr 0. (4. *. size) ;

  Cairo.set_line_join cr Cairo.LINE_JOIN_BEVEL ;
  draw_shapes cr 0. 0. true ;

  Cairo.set_source_rgb cr 1. 0. 0. ;
  draw_shapes cr 0. 0. false



let main () = 
  let w = GWindow.window ~title:"Cairo demo" () in
  w#connect#destroy GMain.quit ;

  let px = GDraw.pixmap ~width:400 ~height:500 ~window:w () in
  begin try redraw px 
  with Cairo.Error _ -> prerr_endline "Cairo is unhappy" end ;
  ignore (GMisc.pixmap px ~packing:w#add ()) ;
  
  w#show () ;
  GMain.main ()

let _ = main ()
