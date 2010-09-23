(**************************************************************************)
(*  cairo-ocaml -- Objective Caml bindings for Cairo                      *)
(*  Copyright © 2004-2005 Olivier Andrieu                                 *)
(*                                                                        *)
(*  This code is free software and is licensed under the terms of the     *)
(*  GNU Lesser General Public License version 2.1 (the "LGPL").           *)
(**************************************************************************)

type args = {
    svg_file : string ;
    png_file : string ;
    scale    : float ;
    width    : int ;
    height   : int ;
  }

let parse_args () =
  let svg_file = ref "" in
  let png_file = ref "" in
  let scale = ref 1. in
  let width = ref (-1) in
  let height = ref (-1) in
  let spec = [
    "-s", Arg.Set_float scale, "scale";
    "-w", Arg.Set_int width, "width";
    "-h", Arg.Set_int height, "height" ] in
  let msg = 
    Printf.sprintf "usage: %s [options] <svg_file> [png_file]"
      (Filename.basename Sys.executable_name) in
  Arg.parse 
    spec
    (fun arg -> 
      if !svg_file = "" then svg_file := arg else
      if !png_file = "" then png_file := arg else
      ())
    msg ;
  if !svg_file = "" then begin
    Arg.usage spec msg ;
    exit 1
  end ;
  if !png_file = "" then
    png_file :=
      begin
	if Filename.check_suffix !svg_file ".svg"
	then Filename.chop_suffix !svg_file ".svg"
	else !svg_file 
      end ^ ".png" ;
  { svg_file = !svg_file ; png_file = !png_file ;
    scale = !scale ; width = !width ; height = !height }

let render_to_png args =
  let svgc = Svg_cairo.create () in
  Svg_cairo.parse svgc args.svg_file ;
  let svg_width, svg_height = Svg_cairo.get_size svgc in

  let scale = ref args.scale in
  let width = ref args.width in
  let height = ref args.height in
  let dx = ref 0. in
  let dy = ref 0. in

  begin
    if args.width < 0 && args.height < 0 then begin
	width  := int_of_float (float svg_width  *. args.scale +. 0.5) ;
	height := int_of_float (float svg_height *. args.scale +. 0.5)
    end
    else if args.width < 0 then begin
      scale := float args.height /. float svg_height ;
      width := int_of_float (float svg_width *. args.scale +. 0.5) ;
    end
    else if args.height < 0 then begin
      scale  := float args.width /. float svg_width ;
      height := int_of_float (float svg_height *. args.scale +. 0.5) ;
    end
    else begin
      scale := min (float args.height /. float svg_height) (float args.width /. float svg_width) ;
      dx := (float args.width  -. (float svg_width  *. args.scale +. 0.5)) /. 2. ;
      dy := (float args.height -. (float svg_height *. args.scale +. 0.5)) /. 2.
    end
  end ;

  let surf = Cairo.image_surface_create Cairo.FORMAT_ARGB32 !width !height in
  let cr = Cairo.create surf in
  Cairo.save cr ; begin
    Cairo.set_operator cr Cairo.OPERATOR_CLEAR ;
    Cairo.paint cr end ;
  Cairo.restore cr ;

  Cairo.translate cr !dx !dy ;
  Cairo.scale cr !scale !scale ;

  Cairo.set_source_rgb cr 1. 1. 1. ;
  Svg_cairo.render svgc cr ;
  Cairo_png.surface_write_to_file surf args.png_file

let _ = 
  render_to_png (parse_args ())
