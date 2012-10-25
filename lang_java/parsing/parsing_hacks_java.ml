(* Yoann Padioleau
 *
 * Copyright (C) 2012 Facebook
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)
open Common

open Parser_java

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * This module transforms certain tokens like '<', normally a LT
 * into a LT2, which helps solving conflicts in the original
 * Java grammar.
 * 
 * This is similar to what we do for C/C++. 
 * See pfff/lang_cpp/parsing/parsing_hacks.ml for more information.
 *)

(*****************************************************************************)
(* Fix tokens *)
(*****************************************************************************)

let fix_tokens xs =

  let rec aux env xs = 
    let depth_angle = env in
    if depth_angle < 0 
    then begin 
      pr2 (spf "depth_angle < 0, %d" depth_angle);
      pr2_gen (List.hd xs);
      (* failwith "depth < 0" *)
      aux 0 xs
    end
    else 

    match xs with
    | [] -> []

    (* dont transform the < of type parameters in LT2. Transforms
     * only for type arguments (but increment depth_angle because
     * we may still need to transform some >> into > >).
     *)
    | CLASS ii::TCommentSpace ii2::IDENTIFIER(s3, ii3)::LT ii4::xs ->
        CLASS ii::TCommentSpace ii2::IDENTIFIER(s3, ii3)::LT ii4::
          aux (depth_angle + 1) xs
    | CLASS ii::TCommentSpace ii2::IDENTIFIER(s3, ii3)::TCommentSpace iisp::
        LT ii4::xs ->
        CLASS ii::TCommentSpace ii2::IDENTIFIER(s3, ii3)::TCommentSpace iisp
        ::LT ii4::
          aux (depth_angle + 1) xs

    | INTERFACE ii::TCommentSpace ii2::IDENTIFIER(s3, ii3)::LT ii4::xs ->
        INTERFACE ii::TCommentSpace ii2::IDENTIFIER(s3, ii3)::LT ii4::
          aux (depth_angle + 1) xs
    | INTERFACE ii::TCommentSpace ii2::IDENTIFIER(s3, ii3)::TCommentSpace iisp::
        LT ii4::xs ->
        INTERFACE ii::TCommentSpace ii2::IDENTIFIER(s3, ii3)::TCommentSpace iisp
        ::LT ii4::
          aux (depth_angle + 1) xs

    (* UGLY HARDCODE, proper way is to have a phase where filter all
     * TCommentSpace and Newline
     *)

    | CLASS ii::TCommentNewline iinewline::
        TCommentSpace ii2::IDENTIFIER(s3, ii3)::LT ii4::xs ->
        CLASS ii::TCommentNewline iinewline::
        TCommentSpace ii2::IDENTIFIER(s3, ii3)::LT ii4::
          aux (depth_angle + 1) xs

(* too many FPs
    | IDENTIFIER (s, ii1)::TCommentSpace iispace::LT ii2::xs 
       when s =~ "^[A-Z]" ->
        IDENTIFIER (s, ii1)::TCommentSpace iispace::LT2 ii2::
          aux (depth_angle + 1) xs
*)

    (* less: allow also a small space, but usually we should fix
     * this code. But pb, see previous comment.
     *)
    | IDENTIFIER (s, ii1)::LT ii2::xs when s =~ "^[A-Z]"->
        IDENTIFIER (s, ii1)::LT2 ii2::aux (depth_angle + 1) xs

    | IDENTIFIER (s, ii1)::TCommentSpace iispace::LT ii2::
      IDENTIFIER (s3, ii3)::xs
       when s =~ "^[A-Z]" && s3 =~ "^[A-Z]" ->
        IDENTIFIER (s, ii1)::TCommentSpace iispace::LT2 ii2::
        aux (depth_angle + 1) (IDENTIFIER (s3, ii3)::xs)

    | IDENTIFIER (s, ii1)::TCommentSpace iispace::LT ii2::
      COND ii3::xs
       when s =~ "^[A-Z]" ->
        IDENTIFIER (s, ii1)::TCommentSpace iispace::LT2 ii2::
        aux (depth_angle + 1) (COND ii3::xs)

    (* xxx.<type>of(...), actually don't have to transform in a LT2
     * but it's a type context so we need to augment depth_angle
     * so at least the >> get transformed into > >.
     *)
    | DOT ii1::LT ii2::xs ->
      DOT ii1::LT2 ii2::aux (depth_angle + 1) xs

    (* <T extends ...> bar().
     * could also check for public|static|... just before the <
     * which is also the sign of generic method.
     *)
    | LT ii1::IDENTIFIER (s, ii2)::TCommentSpace iispace::EXTENDS ii3::xs ->
      LT ii1::IDENTIFIER (s, ii2)::TCommentSpace iispace::EXTENDS ii3::
        aux (depth_angle + 1) xs

    | GT ii::xs when depth_angle > 0 ->
        GT ii::aux (depth_angle - 1) xs

    (* transform >> into two > > *)
    | SRS ii::xs when depth_angle > 0 ->
        (* todo: split ii *)
        GT ii::GT ii::aux (depth_angle - 2) xs

    (* transform >>> into three > > > *)
    | URS ii::xs when depth_angle > 0 ->
        (* todo: split ii *)
        GT ii::GT ii::GT ii::aux (depth_angle - 3) xs

      
  | x::xs -> x::aux env xs
  in
  aux 0 xs

