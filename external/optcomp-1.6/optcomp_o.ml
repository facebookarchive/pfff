(*
 * optcomp_o.ml
 * ------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of optcomp.
 *)

(* Standalone version, with directives in original syntax *)

let module M = Camlp4OCamlParser.Make(Camlp4OCamlRevisedParser.Make(Camlp4.PreCast.Syntax)) in ()

let _ = Optcomp.main Optcomp.O
