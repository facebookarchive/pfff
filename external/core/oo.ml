(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*         Jerome Vouillon, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id: oo.ml 11156 2011-07-27 14:17:02Z doligez $ *)

let copy = CamlinternalOO.copy
external id : < .. > -> int = "%field1"
let new_method = CamlinternalOO.public_method_label
let public_method_label = CamlinternalOO.public_method_label
