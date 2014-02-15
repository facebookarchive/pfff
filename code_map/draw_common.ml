(*s: draw_common.ml *)
(*s: Facebook copyright *)
(* Yoann Padioleau
 * 
 * Copyright (C) 2010-2012 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
(*e: Facebook copyright *)
open Common2.ArithFloatInfix
module CairoH = Cairo_helpers

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Anamorphic entities *)
(*****************************************************************************)

(*s: final_font_size_when_multiplier *)
let final_font_size_when_multiplier 
    ~multiplier ~size_font_multiplier_multiplier 
    ~font_size ~font_size_real 
   = 
  ignore(font_size_real);
  let size_font_multiplier = multiplier in
  
  let font_size_adjusted = 
    if size_font_multiplier = 1.
    then font_size
    else 
      max 
       (font_size * size_font_multiplier * size_font_multiplier_multiplier)
       (font_size * 1.5)
  in
  
  let final_font_size = 
    Common2.borne ~min:font_size ~max:(font_size * 30.) font_size_adjusted
  in
  final_font_size
(*e: final_font_size_when_multiplier *)


(*e: draw_common.ml *)
