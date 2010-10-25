(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*
 * The goal of this module is to provide a data-structure to represent
 * code "layers" (a.k.a. code "aspects"). The idea is to imitate google
 * earth layers (e.g. the wikipedia layer, panoramio layer, etc), but
 * for code. One could have a deadcode layer, a test coverage layer,
 * and then display those layers or not on an existing codebase in 
 * pfff_visual. sgrep can also be extended to transform a query result
 * into a layer.
 * 
 * The layer is basically some mapping from files to a set of lines with
 * a specific color code. A few design choices:
 * 
 *  - How to represent a layer at the macro and micro level in pfff_visual ?
 * 
 *    At the micro-level one has just to display the line with the
 *    requested color. At the macro-level have to either do a majority
 *    scheme or mixing scheme where for instance draw half of the 
 *    treemap rectangle in red and the other in green. 
 * 
 *    Because different layers could have different composition needs
 *    it is simpler to just have the layer say how it should be displayed
 *    at the macro_level. See the 'macro_level' field below.
 * 
 *  - how to have a layer data-structure that can cope with many
 *    needs ? 
 * 
 *   Here are some examples of layers and how they are "encoded" by the
 *   'layer' type below:
 * 
 *    * deadcode (dead function, dead class, dead statement, dead assignnements)
 * 
 *      How? dead lines in red color. At the macro_level one can give
 *      a grey_xxx color  with a percentage (e.g. grey53).
 * 
 *    * test coverage (static or dynamic)
 * 
 *      How? covered lines in green, not covered in red ? Also
 *      convey a GreyLevel visualization by setting the 'macro_level' field.
 * 
 *    * age of file
 * 
 *      How? 2010 in green, 2009 in yelow, 2008 in red and so on.
 *      At the macro_level can do a mix of colors.
 * 
 *    * bad smells
 * 
 *      How? each bad smell could have a different color and macro_level
 *      showing a percentage of the rectangle with the right color
 *      for each smells in the file.
 * 
 *    * security patterns (bad smells)
 * 
 *    * activity ? 
 * 
 *      How whow add and delete information ?
 *      At the micro_level can't show the delete, but at macro_level
 *      could divide the treemap_rectangle in 2 where percentage of
 *      add and delete, and also maybe white to show the amount of add
 *      and delete. Could also use my big circle scheme.
 *      How link to commit message ? TODO
 * 
 * 
 * later: 
 *  - could  associate more than just a color, e.g. a commit message when want
 *    to display a version-control layer, or some filling-patterns in
 *    addition to the color.
 *  - Could have  better precision than the line.
 * 
 * history:
 *  - I was writing some treemap generator specific for the deadcode
 *    analysis, the static coverage, the dynamic coverage, and the activity
 *    in a file (see treemap_php.ml). I was also offering different
 *    way to visualize the result (DegradeArchiColor | GreyLevel | YesNo).
 *    It was working fine but there was no easy way to combine 2
 *    visualisations, like the age "layer" and the "deadcode" layer
 *    to see correlations. Also adding simple layers like 
 *    visualizing all calls to HTML() or XHP was requiring to
 *    write another treemap generator. To be more generic and flexible require
 *    a real 'layer' type.
 *)

(*****************************************************************************)
(* Type *)
(*****************************************************************************)

(* note: the filenames must be in readable format 
 * 
 * alternatives:
 *  - could have line range ? useful for layer matching lots of
 *    consecutive lines in a file ?
 *  - todo? have more precision than just the line ? precise pos range ?
 * 
 *  - could for the lines instead of a 'kind' to have a 'count',
 *    and then some mappings from range of values to a color.
 *    For instance on a coverage layer one could say that from X to Y
 *    then choose this color, from Y to Z another color.
 *    But can emulate that by having a "coverage1", "coverage2"
 *    kind with the current scheme.
 * 
 *  - have a macro_level_composing_scheme: Majority | Mixed
 *    that is then interpreted in pfff_visual instead of forcing
 *    the layer creator to specific how to show the micro_level
 *    data at the macro_level.
 *)

type layer = {
  files: (filename * file_info) list;
  kinds: (kind * Simple_color.emacs_color) list;
 }
 and file_info = {

   micro_level: (int (* line *) * kind) list;

   (* The list can be empty in which case pfff_visual can use
    * the micro_level information and show a mix of colors.
    * 
    * The list can have just one element too and have a kind
    * different than the one used in the micro_level. For instance
    * for the coverage one can have red/green at micro_level
    * and grey_xxx at macro_level.
    *)
   macro_level: (kind * float (* percentage of rectangle *)) list;
 }
 and kind = string

 (* with tarzan *)

(*****************************************************************************)
(* Meta *)
(*****************************************************************************)

(* generated by ocamltarzan *)

let vof_emacs_color s = Ocaml.vof_string s
let vof_filename s = Ocaml.vof_string s

let rec vof_layer { files = v_files; kinds = v_kinds } =
  let bnds = [] in
  let arg =
    Ocaml.vof_list
      (fun (v1, v2) ->
         let v1 = vof_kind v1
         and v2 = vof_emacs_color v2
         in Ocaml.VTuple [ v1; v2 ])
      v_kinds in
  let bnd = ("kinds", arg) in
  let bnds = bnd :: bnds in
  let arg =
    Ocaml.vof_list
      (fun (v1, v2) ->
         let v1 = vof_filename v1
         and v2 = vof_file_info v2
         in Ocaml.VTuple [ v1; v2 ])
      v_files in
  let bnd = ("files", arg) in let bnds = bnd :: bnds in Ocaml.VDict bnds
and
  vof_file_info { micro_level = v_micro_level; macro_level = v_macro_level }
                =
  let bnds = [] in
  let arg =
    Ocaml.vof_list
      (fun (v1, v2) ->
         let v1 = vof_kind v1
         and v2 = Ocaml.vof_float v2
         in Ocaml.VTuple [ v1; v2 ])
      v_macro_level in
  let bnd = ("macro_level", arg) in
  let bnds = bnd :: bnds in
  let arg =
    Ocaml.vof_list
      (fun (v1, v2) ->
         let v1 = Ocaml.vof_int v1
         and v2 = vof_kind v2
         in Ocaml.VTuple [ v1; v2 ])
      v_micro_level in
  let bnd = ("micro_level", arg) in
  let bnds = bnd :: bnds in Ocaml.VDict bnds
and vof_kind v = Ocaml.vof_string v

(*****************************************************************************)
(* Ocaml.v -> layer *)
(*****************************************************************************)

let emacs_color_ofv v = Ocaml.string_ofv v
let filename_ofv v = Ocaml.string_ofv v


let rec layer_ofv__ =
  let _loc = "Xxx.layer"
  in
    function
    | (Ocaml.VDict field_sexps as sexp) ->
        let files_field = ref None and kinds_field = ref None
        and duplicates = ref [] and extra = ref [] in
        let rec iter =
          (function
           | (field_name, field_sexp) :: tail ->
               ((match field_name with
                 | "files" ->
                     (match !files_field with
                      | None ->
                          let fvalue =
                            Ocaml.list_ofv
                              (function
                               | Ocaml.VList ([ v1; v2 ]) ->
                                   let v1 = filename_ofv v1
                                   and v2 = file_info_ofv v2
                                   in (v1, v2)
                               | sexp ->
                                   Ocaml.tuple_of_size_n_expected _loc 2 sexp)
                              field_sexp
                          in files_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | "kinds" ->
                     (match !kinds_field with
                      | None ->
                          let fvalue =
                            Ocaml.list_ofv
                              (function
                               | Ocaml.VList ([ v1; v2 ]) ->
                                   let v1 = kind_ofv v1
                                   and v2 = emacs_color_ofv v2
                                   in (v1, v2)
                               | sexp ->
                                   Ocaml.tuple_of_size_n_expected _loc 2 sexp)
                              field_sexp
                          in kinds_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | _ ->
                     if !Conv.record_check_extra_fields
                     then extra := field_name :: !extra
                     else ());
                iter tail)
           | [] -> ())
        in
          (iter field_sexps;
           if !duplicates <> []
           then Ocaml.record_duplicate_fields _loc !duplicates sexp
           else
             if !extra <> []
             then Ocaml.record_extra_fields _loc !extra sexp
             else
               (match ((!files_field), (!kinds_field)) with
                | (Some files_value, Some kinds_value) ->
                    { files = files_value; kinds = kinds_value; }
                | _ ->
                    Ocaml.record_undefined_elements _loc sexp
                      [ ((!files_field = None), "files");
                        ((!kinds_field = None), "kinds") ]))
    | sexp -> Ocaml.record_list_instead_atom _loc sexp
and layer_ofv sexp = layer_ofv__ sexp
and file_info_ofv__ =
  let _loc = "Xxx.file_info"
  in
    function
    | (Ocaml.VDict field_sexps as sexp) ->
        let micro_level_field = ref None and macro_level_field = ref None
        and duplicates = ref [] and extra = ref [] in
        let rec iter =
          (function
           | (field_name, field_sexp) :: tail ->
               ((match field_name with
                 | "micro_level" ->
                     (match !micro_level_field with
                      | None ->
                          let fvalue =
                            Ocaml.list_ofv
                              (function
                               | Ocaml.VList ([ v1; v2 ]) ->
                                   let v1 = Ocaml.int_ofv v1
                                   and v2 = kind_ofv v2
                                   in (v1, v2)
                               | sexp ->
                                   Ocaml.tuple_of_size_n_expected _loc 2 sexp)
                              field_sexp
                          in micro_level_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | "macro_level" ->
                     (match !macro_level_field with
                      | None ->
                          let fvalue =
                            Ocaml.list_ofv
                              (function
                               | Ocaml.VList ([ v1; v2 ]) ->
                                   let v1 = kind_ofv v1
                                   and v2 = Ocaml.float_ofv v2
                                   in (v1, v2)
                               | sexp ->
                                   Ocaml.tuple_of_size_n_expected _loc 2 sexp)
                              field_sexp
                          in macro_level_field := Some fvalue
                      | Some _ -> duplicates := field_name :: !duplicates)
                 | _ ->
                     if !Conv.record_check_extra_fields
                     then extra := field_name :: !extra
                     else ());
                iter tail)
           | [] -> ())
        in
          (iter field_sexps;
           if !duplicates <> []
           then Ocaml.record_duplicate_fields _loc !duplicates sexp
           else
             if !extra <> []
             then Ocaml.record_extra_fields _loc !extra sexp
             else
               (match ((!micro_level_field), (!macro_level_field)) with
                | (Some micro_level_value, Some macro_level_value) ->
                    {
                      micro_level = micro_level_value;
                      macro_level = macro_level_value;
                    }
                | _ ->
                    Ocaml.record_undefined_elements _loc sexp
                      [ ((!micro_level_field = None), "micro_level");
                        ((!macro_level_field = None), "macro_level") ]))
    | sexp -> Ocaml.record_list_instead_atom _loc sexp
and file_info_ofv sexp = file_info_ofv__ sexp
and kind_ofv__ = let _loc = "Xxx.kind" in fun sexp -> Ocaml.string_ofv sexp
and kind_ofv sexp = kind_ofv__ sexp

(*****************************************************************************)
(* Json *)
(*****************************************************************************)

let json_of_layer layer =
  layer +> vof_layer +> Ocaml.json_of_v

let layer_of_json json =
  json +> Ocaml.v_of_json +> layer_ofv

(*****************************************************************************)
(* Load/Save *)
(*****************************************************************************)

let is_json_file file = 
  match File_type.file_type_of_file file with
  | File_type.PL (File_type.Web (File_type.Json)) -> true
  | _ -> false

(* we allow to save in JSON format because it may be useful to let
 * the user edit the layer file, for instance to adjust the colors.
 *)
let load_layer file =
  if is_json_file file
  then Ocaml.load_json file +> layer_of_json
  else Common.get_value file

let save_layer layer file =
  if is_json_file file
  (* layer +> vof_layer +> Ocaml.string_of_v +> Common.write_file ~file *)
  then layer +> json_of_layer +> Ocaml.save_json file
  else  Common.write_value layer file
