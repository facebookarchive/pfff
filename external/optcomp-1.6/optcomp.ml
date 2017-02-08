(*
 * optcomp.ml
 * ----------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of optcomp.
 *)

(* Standalone version *)

open Camlp4.PreCast
open Camlp4.Sig

type mode = O | R

(* +-----------------------------------------------------------------+
   | The lexer                                                       |
   +-----------------------------------------------------------------+ *)

external filter : 'a Gram.not_filtered -> 'a = "%identity"

(* A lexer that does not filter tokens. *)
let lexer fname ic =
  let stream = filter (Gram.lex (Loc.mk fname) (Stream.of_channel ic)) in
  let pos = ref None in
  (* Fix the Camlp4 lexer. Start locations are often wrong but end
     locations are always correct. *)
  let next i =
    let tok, loc = Stream.next stream in
    match !pos with
      | None ->
          pos := Some loc;
          Some (tok, loc)
      | Some loc' ->
          pos := Some loc;
          if Loc.file_name loc' = Loc.file_name loc then
            let _, _, _, _, a, b, c, _ = Loc.to_tuple loc'
            and n, _, _, _, d, e, f, g = Loc.to_tuple loc in
            Some (tok, Loc.of_tuple (n, a, b, c, d, e, f, g))
          else
            Some (tok, loc)
  in
  Stream.from next

(* +-----------------------------------------------------------------+
   | Printer                                                         |
   +-----------------------------------------------------------------+ *)

module File_map = Map.Make(String)

(* Iterate over the filtered stream. *)
let rec print mode current_fname current_line current_col files token_stream =
  match try Some (Stream.next token_stream) with Stream.Failure -> None with
    | None ->
        ()
    | Some (EOI, _) ->
        flush stdout
    | Some (tok, loc) ->
        let fname = Loc.file_name loc
        and off = Loc.start_off loc
        and line = Loc.start_line loc
        and col = Loc.start_off loc - Loc.start_bol loc
        and len = Loc.stop_off loc - Loc.start_off loc in
        (* Get the input. *)
        let ic, files =
          try
            (File_map.find fname files, files)
          with Not_found ->
            let ic = open_in fname in
            (ic, File_map.add fname ic files)
        in
        let str, stop_line, stop_col =
          match tok with
            | QUOTATION { q_name = "optcomp"; q_contents = str } ->
                let str =
                  (match mode with
                     | O -> Pa_optcomp.string_of_value_o
                     | R -> Pa_optcomp.string_of_value_r)
                    (Pa_optcomp.get_quotation_value str)
                in
                (str, line, col + String.length str)
            | tok ->
                (* Go to the right position in the input. *)
                if pos_in ic <> off then seek_in ic off;
                (* Read the part to copy. *)
                let str = String.create len in
                really_input ic str 0 len;
                (str, Loc.stop_line loc, Loc.stop_off loc - Loc.stop_bol loc)
        in
        if current_fname = fname && current_line = line && current_col = col then
          (* If we at the right position, just print the string. *)
          print_string str
        else begin
          (* Otherwise print a location directive. *)
          if current_col > 0 then print_char '\n';
          Printf.printf "# %d %S\n" line fname;
          (* Ensure that the string start at the right column. *)
          for i = 1 to col do
            print_char ' '
          done;
          print_string str
        end;
        print mode fname stop_line stop_col files token_stream

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

let main mode =
  if Array.length Sys.argv <> 2 then begin
    Printf.eprintf "usage: %s <file>\n%!" (Filename.basename Sys.argv.(0));
    exit 2
  end;
  try
    let fname = Sys.argv.(1) in
    let ic = open_in fname in
    (* Create the filtered token stream. *)
    let token_stream = Pa_optcomp.filter ~lexer (lexer fname ic) in
    print mode "" (-1) (-1) File_map.empty token_stream
  with exn ->
    flush stdout;
    Format.eprintf "@[<v0>%a@]@." Camlp4.ErrorHandler.print exn;
    exit 1
