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
 * for code. One could have a deadcode layer, a coverage layer,
 * and then display those layers or not on an existing codebase in 
 * pfff_visual. sgrep can also be extended to transform a query result
 * into a layer.
 * 
 * The layer is basically some mapping from files to a set of lines with
 * a specific color code. A few questions and answers:
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
 *   'layer type below:
 * 
 *    * deadcode (dead function, class, statement)
 *      how? dead lines in red color. At the macro_level one can give
 *      a grey_xxx color  with a percentage (e.g. grey53).
 * 
 *    * test coverage (static or dynamic)
 *      how? covered lines in green, not covered in red ? Also
 *      convey a GreyLevel visualization by setting the 'macro_level' field.
 * 
 *    * age of file
 *      how? 2010 in green, 2009 in yelow, 2008 in red and so on.
 *      At the macro_level can do a mix of colors.
 * 
 *    * bad smells
 *      how? each bad smell could have a different color and macro_level
 *      showing a percentage of the rectangle with the right color
 *      for each smells in the file.
 * 
 *    * security patterns (bad smells)
 * 
 *    * activity ? How whow add and delete information ?
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

(*****************************************************************************)
(* Json *)
(*****************************************************************************)
