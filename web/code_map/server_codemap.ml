(* Yoann Padioleau
 * 
 * Copyright (C) 2013 Facebook
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
open Common

module FT = File_type
module T = Treemap

module Model = Model_codemap
module Flag = Flag_web

(*
module Parsing = Parsing2
*)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

let filter = ref (fun file -> true)

(*****************************************************************************)
(* Macro view server helpers *)
(*****************************************************************************)

let treemap_generator paths = 
  let treemap = Treemap_pl.code_treemap ~filter_file:!filter paths in
  let algo = Treemap.Ordered Treemap.PivotByMiddle in
  let rects = Treemap.render_treemap
    ~algo ~big_borders:!Flag.boost_label_size
    treemap 
  in
  pr2 (spf "%d rectangles to draw" (List.length rects));
  rects

(*
    Common.cache_computation ~verbose:true path "_treemap.marshall" 
      (fun () ->)
    in
*)

(* optimize and filter very small rectangles so that we send less data *)
let optimize_rects root rects =
  let rects = rects +> Common.map_filter (fun rect ->
    let readable = Common.readable ~root rect.T.tr_label in
    let rect = { rect with T.tr_label = readable } in
    let r = rect.Treemap.tr_rect in
    let w = Figures.rect_width r in
    let h = Figures.rect_height r in
    (match () with 
    | _ when w *. h <= 0.000009 ->
      None
    | _ when w *. h <= 0.0009 ->
      Some { rect with Treemap.tr_label = ""; }
    | _ ->
      Some rect
    )
  )
  in
  pr2 (spf "nb rects after filtering: %d" (List.length rects));
  rects

(*****************************************************************************)
(* Micro view server helpers *)
(*****************************************************************************)

(*
let is_big_file_with_few_lines ~nblines fullpath = 
  nblines < 20. && 
  Common2.filesize_eff fullpath > 4000
*)

  (* if the file is not textual, or contain weird characters, then
   * it confuses cairo which then can confuse computation done in gtk
   * idle callbacks
   *)
  (*if Common2.lfile_exists_eff file && File_type.is_textual_file file*)

  (* Common.nblines_with_wc was really slow. fork sucks.
   * alternative: we could store the nblines of a file in the db but
   * we would need a fast absolute_to_readable then.
   *)

let fileinfo_of_file file =

  (* coupling: with parsing2.ml *)
  let style =
    match FT.file_type_of_file file with
    | ( FT.PL (FT.Web (FT.Php _))
      | FT.PL (FT.Web (FT.Js))
      | FT.PL (FT.Web (FT.Html))
      | FT.PL (FT.ML _)
      | FT.PL (FT.Cplusplus _ | FT.C _)
      | FT.PL (FT.Thrift)
      | FT.Text ("nw" | "tex"  | "texi" | "web" | "org")
      | FT.PL (FT.Lisp _)
      | FT.PL (FT.Haskell _)
      | FT.PL (FT.Python)
      | FT.PL (FT.Csharp)
      | FT.PL (FT.Java)
            (*    | FT.PL (FT.Prolog _) *)
      | FT.PL (FT.Erlang)
      | FT.PL (FT.Opa)
      (* | (FT.Text "txt") when Common2.basename file =$= "info.txt" *)
      ) -> 
        let tokens_with_categ = 
          Server_codemap_parsing.tokens_with_categ_of_file 
            file (* entities *) in

        let res = tokens_with_categ +> List.map
          (fun (s, categ, filepos) ->
            Common2.lines_with_nl_either s, categ, filepos
          )
        in
        Model.Fancy res
    | FT.PL _ | FT.Text _ ->
        Model.Regular (Common.cat file)
    | _ -> 
        Model.Nothing
  in

  { Model.
      style;
      nblines = float_of_int (Common2.nblines_eff file);
  }
