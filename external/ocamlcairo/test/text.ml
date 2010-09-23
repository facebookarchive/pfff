(**************************************************************************)
(*  cairo-ocaml -- Objective Caml bindings for Cairo                      *)
(*  Copyright © 2004-2005 Olivier Andrieu                                 *)
(*                                                                        *)
(*  This code is free software and is licensed under the terms of the     *)
(*  GNU Lesser General Public License version 2.1 (the "LGPL").           *)
(**************************************************************************)

let num_glyphs = 10
let text = "hello, world"

let box_text cr txt x y =
  Cairo.save cr ; begin
    let ext = Cairo.text_extents cr text in
    let line_width = Cairo.get_line_width cr in
    Cairo.rectangle cr
      (x +. ext.Cairo.x_bearing -. line_width)
      (y +. ext.Cairo.y_bearing -. line_width)
      (ext.Cairo.text_width  +. 2. *. line_width)
      (ext.Cairo.text_height +. 2. *. line_width) ;
    Cairo.stroke cr ;
    
    Cairo.move_to cr x y ;
    Cairo.show_text cr txt ;
    Cairo.text_path cr txt ;
    Cairo.set_source_rgb cr 1. 0. 0. ;
    Cairo.set_line_width cr 1.0 ;
    Cairo.stroke cr end ;

  Cairo.restore cr


let box_glyphs cr gly x y =
  Cairo.save cr ; begin
    let ext = Cairo.glyph_extents cr gly in
    let line_width = Cairo.get_line_width cr in
    Cairo.rectangle cr
      (x +. ext.Cairo.x_bearing -. line_width)
      (y +. ext.Cairo.y_bearing -. line_width)
      (ext.Cairo.text_width  +. 2. *. line_width)
      (ext.Cairo.text_height +. 2. *. line_width) ;
    Cairo.stroke cr ;
    let gly = 
      Array.map
	(fun g ->
	  { g with Cairo.glyph_x = g.Cairo.glyph_x +. x ; 
	           Cairo.glyph_y = g.Cairo.glyph_y +. y })
	gly in
    Cairo.show_glyphs cr gly ;
    Cairo.glyph_path cr gly ;
    Cairo.set_source_rgb cr 1. 0. 0. ;
    Cairo.set_line_width cr 1. ;
    Cairo.stroke cr end ;
  Cairo.restore cr

let draw cr w h = 
  Cairo.set_source_rgb cr 0. 0. 0. ;
  Cairo.set_line_width cr 2. ;

  Cairo.save cr ; begin
    Cairo.set_source_rgb cr 1. 1. 1. ;
    Cairo.rectangle cr 0. 0. w h ;
    Cairo.set_operator cr Cairo.OPERATOR_SOURCE ;
    Cairo.fill cr end ;
  Cairo.restore cr ;

  Cairo.select_font_face cr "serif" Cairo.FONT_SLANT_NORMAL Cairo.FONT_WEIGHT_NORMAL ;
  Cairo.set_font_size cr 40. ;
  let { Cairo.font_height = height } = 
    Cairo.font_extents cr in
  
  let glyphs =
    begin
      let dx = ref 0. in
      let dy = ref 0. in
      Array.init num_glyphs
	(fun i ->
	  let g = 
	    { Cairo.index = i + 4 ;
	      Cairo.glyph_x = !dx ;
	      Cairo.glyph_y = !dy } in
	  let ext = Cairo.glyph_extents cr [| g |] in
	  dx := !dx +. ext.Cairo.x_advance ;
	  dy := !dy +. ext.Cairo.y_advance ;
	  g)
    end in

  box_text cr text 10. height ;

  Cairo.translate cr 0. height ;
  Cairo.save cr ; begin
    Cairo.translate cr 10. height ;
    Cairo.rotate cr (10. *. atan 1. /. 45.) ;
    box_text cr text 0. 0. end ;
  Cairo.restore cr ;

  Cairo.translate cr 0. (2. *. height) ;
  Cairo.save cr ; begin
    let m = Cairo.Matrix.init_rotate (10. *. atan 1. /. 45.) in
    Cairo.set_font_matrix cr m ;
    box_text cr text 10. height end ;
  Cairo.restore cr ;

  Cairo.translate cr 0. (2. *. height) ;
  box_glyphs cr glyphs 10. height ;

  Cairo.translate cr 10. (2. *. height) ;
  Cairo.save cr ; begin
    Cairo.rotate cr (10. *. atan 1. /. 45.) ;
    box_glyphs cr glyphs 0. 0. end ;
  Cairo.restore cr ;

  Cairo.translate cr 0. height ;
  box_glyphs cr
    (Array.mapi
       (fun i g -> { g with Cairo.glyph_y = g.Cairo.glyph_y +. float (i * 5) })
       glyphs)
    10. height

let width = 450
let height = 600

let main () = 
  let w = GWindow.window ~title:"Cairo Text API" () in
  w#connect#destroy GMain.quit ;

  let p = GDraw.pixmap ~width ~height ~window:w () in
  let cr = Cairo_lablgtk.create p#pixmap in
  draw cr (float width) (float height) ;
  GMisc.pixmap p ~packing:w#add () ;

  w#show () ;
  GMain.main ()

let _ = main ()
