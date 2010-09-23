(**************************************************************************)
(*  cairo-ocaml -- Objective Caml bindings for Cairo                      *)
(*  Copyright © 2004-2005 Olivier Andrieu                                 *)
(*                                                                        *)
(*  This code is free software and is licensed under the terms of the     *)
(*  GNU Lesser General Public License version 2.1 (the "LGPL").           *)
(**************************************************************************)

let pi = 4. *. atan 1.

let main font_arg =
  let ft = Cairo_ft.init_freetype () in
  let font, clean_up =
    if Sys.file_exists font_arg
    then
      let face = Cairo_ft.new_face ft font_arg in
      let font = Cairo_ft.font_face_create_for_ft_face face 0 in
      (font, (fun () -> Cairo_ft.done_face face))
    else begin
      let pattern = Cairo_ft.fc_name_parse font_arg in
      let font = Cairo_ft.font_face_create_for_pattern pattern in
      (font, ignore)
    end
  in

  let s = Cairo.image_surface_create Cairo.FORMAT_ARGB32 ~width:200 ~height:200 in
  let cr = Cairo.create s in

  Cairo.set_font_face cr font ;

  Cairo.set_font_size cr 20. ;
  Cairo.move_to cr 10. 10. ;
  Cairo.rotate cr (pi /. 2.) ;
  Cairo.show_text cr "Hello World !" ;

  Cairo_png.surface_write_to_file s "test_font.png" ;

  clean_up () ;
  Cairo_ft.done_freetype ft

let _ =
  if Array.length Sys.argv < 2 then exit 1 ;
  try main Sys.argv.(1)
  with Cairo.Error s ->
    Printf.eprintf "Cairo error: '%s'\n%!" (Cairo.string_of_status s)
