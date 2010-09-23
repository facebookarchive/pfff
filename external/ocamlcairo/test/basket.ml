(**************************************************************************)
(*  cairo-ocaml -- Objective Caml bindings for Cairo                      *)
(*  Copyright © 2004-2005 Olivier Andrieu                                 *)
(*                                                                        *)
(*  This code is free software and is licensed under the terms of the     *)
(*  GNU Lesser General Public License version 2.1 (the "LGPL").           *)
(**************************************************************************)

type point = Cairo.point = { x : float ; y : float }

let _ =
  Cairo.init

let print_path_elem = function
  | `MOVE_TO p ->
      Format.printf "@ move_to (%f, %f)" p.x p.y
  | `LINE_TO p ->
      Format.printf "@ line_to (%f, %f)" p.x p.y
  | `CURVE_TO (p1, p2, p3) ->
      Format.printf "@ curve_to (%f, %f, %f, %f, %f, %f)" p1.x p1.y p2.x p2.y p3.x p3.y
  | `CLOSE ->
      Format.printf "@ close\n"
let print_path c =
  Format.printf "@[<v 2>current_path:" ;
  let nb = Cairo.fold_path c
      (fun nb el -> print_path_elem el ; nb+1) 0 in
  Format.printf "@]%d elements@." nb

let draw ?(print=false) c =
  Cairo.move_to c 50. 50. ;
  Cairo.line_to c 550. 50. ;
  Cairo.curve_to c 450. 240. 150. 240. 50. 50. ;
  Cairo.close_path c ;
  
  if print then print_path c ;
  
  Cairo.save c ; begin
    Cairo.set_source_rgb c 0.8 0.1 0.1 ;
    Cairo.fill_preserve c end ;
  Cairo.restore c ;

  Cairo.set_line_width c 6. ;
  Cairo.set_source_rgb c 0. 0. 0. ;
  Cairo.stroke c

let do_file_out fname f =
  let oc = open_out fname in
  try
    let r = f oc in
    close_out oc ;
    r
  with exn ->
    close_out_noerr oc ;
    raise exn

let x_inches = 8.
let y_inches = 3.


let file_backend ?(verbose=false) ~backend_name ~filename surface_create =
  prerr_endline backend_name ;
  do_file_out filename
    (fun oc ->
      let width_in_points  = x_inches *. 72. in
      let height_in_points = y_inches *. 72. in
      let s = surface_create oc ~width_in_points ~height_in_points in
      let c = Cairo.create s in
      draw ~print:verbose c ;
      Cairo.show_page c ;
      Cairo.surface_finish s)

let main () =

  file_backend
    ~verbose:true
    ~backend_name:"PS"
    ~filename:"basket.ps"
    Cairo_ps.surface_create_for_channel ;

  file_backend
    ~backend_name:"PDF"
    ~filename:"basket.pdf"
    Cairo_pdf.surface_create_for_channel ;

  file_backend
    ~backend_name:"SVG"
    ~filename:"basket.svg"
    Cairo_svg.surface_create_for_channel ;

  begin
    prerr_endline "Bigarray, PPM and PNG (ARGB32) " ;
    let arr = 
      Bigarray.Array2.create Bigarray.int32 Bigarray.c_layout
	(int_of_float y_inches * 72) (int_of_float x_inches * 72) in
    Bigarray.Array2.fill arr 0xffffffl ;
    let s = Cairo_bigarray.of_bigarr_32 ~alpha:true arr in
    let c = Cairo.create s in
    draw c ;
    do_file_out "basket.ppm"
      (fun oc -> Cairo_bigarray.write_ppm_int32 oc arr) ;
    Cairo_png.surface_write_to_file s "basket.png"
  end

(*
   begin
   prerr_endline "GdkPixbuf and PNG" ;
   let pb = GdkPixbuf.create
   ~width:(int_of_float width)
   ~height:(int_of_float height) ~bits:8 ~has_alpha:true () in
   GdkPixbuf.fill pb (Int32.of_string "0xffffffff") ;
   let img = Cairo_lablgtk.image_of_pixbuf pb in
   Cairo.set_target_image c img ;
   draw c ;
   Cairo_lablgtk.shuffle_pixels pb ;
   GdkPixbuf.save ~filename:"basket.png" ~typ:"png" pb
   end
 *)

let () = 
  try main () 
  with Cairo.Error s ->
    Printf.eprintf "Fatal error: cairo exception: '%d'\n" (Obj.magic s)
