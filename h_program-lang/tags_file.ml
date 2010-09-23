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
 * Supposed syntax for emacs TAGS (.tags) files, as analysed from output
 * of etags, read in etags.c and discussed with Francesco Potorti.
 * 
 * src: otags readme:
 * Supposed syntax for emacs TAGS (.tags) files, as analysed from output
 * of etags, read in etags.c.
 * 
 * <file> ::= <page>+
 * 
 * <page> ::= <header><body>
 * 
 * <header> ::= <NP><CR><file-name>,<body-length><CR>
 * 
 * <body> ::= <tag-line>*
 * 
 * <tag-line> ::= <prefix><DEL><tag><SOH><line-number>,<begin-char-index><CR>
 * pad: when tag is already at the beginning of the line:
 * <tag-line> ::=<tag><DEL><line-number>,<begin-char-index><CR>
 * 
 * <NP> ::= ascii NP, (emacs ^L) 
 * <DEL> ::= ascii DEL, (emacs ^?)
 * <SOH> ::= ascii SOH, (emacs ^A)
 * <CR> :: ascii CR
 * 
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* see http://en.wikipedia.org/wiki/Ctags#Tags_file_formats *)
let header = "\x0c\n"

let footer = ""

type tag = {
  tag_definition_text: string;
  tagname: string;
  line_number: int;
  (* offset of beginning of tag_definition_text, when have 0-indexed filepos *)
  byte_offset: int; 
}

let mk_tag s1 s2 i1 i2 = {
  tag_definition_text = s1;
  tagname = s2;
  line_number = i1;
  byte_offset = i2;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let string_of_tag t = 
  spf "%s\x7f%s\x01%d,%d\n" 
    t.tag_definition_text
    t.tagname
    t.line_number
    t.byte_offset

(* of tests/misc/functions.php *)
let fake_defs = [
  mk_tag "function a() {" "a" 3 7;
  mk_tag "function b() {" "b" 7 32;
  mk_tag "function c() {" "c" 14 65;
  mk_tag "function d() {" "d" 20 107;
]

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let generate_TAGS_file files_and_defs =

  let tag_file = Common.relative_to_absolute "TAGS" in
  let res = Common.y_or_no (spf "writing data in %s" tag_file) in
  if not res 
  then failwith "ok I stop";
  
  Common.with_open_outfile tag_file (fun (pr_no_nl, _chan) ->

    pr_no_nl header;

    files_and_defs +> List.iter (fun (file, defs) ->

      let all_defs = defs +> List.map string_of_tag +> Common.join "" in
      let size_defs = String.length all_defs in
      pr_no_nl (spf "%s,%d\n" file size_defs);
      pr_no_nl all_defs;
      pr_no_nl "\x0c\n";
    );
  );
  ()
