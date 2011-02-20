(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Ocamlbuild plugin
 * Copyright (C) 2008 Stéphane Glondu
 * Laboratoire PPS - CNRS Université Paris Diderot
 *               2009 Jérémie Dimino
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 *)

open Printf
open Ocamlbuild_plugin

(* +-----------------------------------------------------------------+
   | Configuration                                                   |
   +-----------------------------------------------------------------+ *)

let try_exec command =
  try
    let _ = run_and_read command in
    true
  with _ ->
    false

let () =
  if not (try_exec "ocamlfind printconf") then begin
    prerr_endline "ocamlfind is not available, please install it";
    exit 1
  end

let have_native = try_exec "ocamlfind ocamlopt -version"
let have_lwt_glib = try_exec "ocamlfind query lwt.glib"
let have_lwt_text = try_exec "ocamlfind query lwt.text"
let have_lwt_preemptive = try_exec "ocamlfind query lwt.preemptive"

(* +-----------------------------------------------------------------+
   | Ocamlfind                                                       |
   +-----------------------------------------------------------------+ *)

(* Packages we want to use: *)
let packages = [
  "threads";
  "lwt";
  "lwt.preemptive";
  "lwt.extra";
  "lwt.ssl";
  "lwt.glib";
  "lwt.text";
  "lwt.unix";
  "lwt.mmap";
  "lablgtk2";

  (* Syntax extensions: *)
  "camlp4";
  "lwt.syntax";
  "lwt.syntax.log";
]

(* List of syntaxes: *)
let syntaxes = [ "camlp4o"; "camlp4r" ]

(* +-----------------------------------------------------------------+
   | Utils                                                           |
   +-----------------------------------------------------------------+ *)

(* Given the tag [tag] add the command line options [f] to all stages
   of compilatiopn but linking *)
let flag_all_stages_except_link tag f =
  flag ["ocaml"; "compile"; tag] f;
  flag ["ocaml"; "ocamldep"; tag] f;
  flag ["ocaml"; "doc"; tag] f

(* Same as [flag_all_stages_except_link] but also flag the linking
   stage *)
let flag_all_stages tag f =
  flag_all_stages_except_link tag f;
  flag ["ocaml"; "link"; tag] f

let _ =
  dispatch begin function
    | Before_options ->

        (* override default commands by ocamlfind ones *)
        let ocamlfind x = S[A"ocamlfind"; A x] in
        Options.ocamlc   := ocamlfind "ocamlc";
        Options.ocamlopt := ocamlfind "ocamlopt";
        Options.ocamldep := ocamlfind "ocamldep";
        Options.ocamldoc := ocamlfind "ocamldoc"

    | After_rules ->

        (* +---------------------------------------------------------+
           | Virtual targets                                         |
           +---------------------------------------------------------+ *)

        let examples = "logging" :: "parallelize" :: "relay" :: "start_editor" :: "load_file" ::
          (List.concat
             (List.map snd
                (List.filter fst
                   [(have_lwt_glib, ["ex_gtk"]);
                    (have_lwt_text, ["show_keys"; "kikoolol_prompt"]);
                    (have_lwt_preemptive, ["minishell"])])))
        in

        let byte = List.map (sprintf "%s.byte") examples
        and native = List.map (sprintf "%s.native") examples in

        let virtual_rule name deps =
          rule name ~stamp:name ~deps (fun _ _ -> Nop)
        in

        virtual_rule "best" & if have_native then native else byte;
        virtual_rule "byte" & byte;
        virtual_rule "native" & native;

        (* +---------------------------------------------------------+
           | Ocamlfind stuff                                         |
           +---------------------------------------------------------+ *)

        (* When one link an OCaml binary, one should use -linkpkg *)
        flag ["ocaml"; "link"; "program"] & A"-linkpkg";

        (* For each ocamlfind package one inject the -package option
           when compiling, computing dependencies, generating
           documentation and linking. *)
        List.iter
          (fun package -> flag_all_stages ("pkg_" ^ package) (S[A"-package"; A package]))
          packages;

        (* Like -package but for extensions syntax. Morover -syntax is
           useless when linking. *)
        List.iter
          (fun syntax -> flag_all_stages_except_link ("syntax_" ^ syntax) (S[A"-syntax"; A syntax]))
          syntaxes;
    | _ -> ()
  end
