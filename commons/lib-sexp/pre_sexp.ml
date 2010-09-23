# 1 "pre_sexp.ml.in"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "pre_sexp.ml.in"
(*pp cpp *)

(* File: sexp.ml

    Copyright (C) 2005-

      Jane Street Holding, LLC
      Author: Markus Mottl
      email: mmottl\@janestcapital.com
      WWW: http:

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
*)

(* Sexp: Module for handling S-expressions (I/O, etc.) *)

open Format

include Type

(* Default indentation level for human-readable conversions *)

let default_indent = ref 1

(* Escaping of strings used as atoms in S-expressions *)

let is_special_char c =
  c <= ' ' || c = '"' || c = '(' || c = ')' || c = ';' || c = '\\'

let must_escape str =
  let len = String.length str in
  len = 0 ||
    let rec loop ix = is_special_char str.[ix] || ix > 0 && loop (ix - 1) in
    loop (len - 1)

let maybe_esc_str str =
  if must_escape str then
    let estr = String.escaped str in
    let elen = String.length estr in
    let res = String.create (elen + 2) in
    String.blit estr 0 res 1 elen;
    res.[0] <- '"';
    res.[elen + 1] <- '"';
    res
  else str

let pp_maybe_esc_str ppf str = pp_print_string ppf (maybe_esc_str str)

(* Output of S-expressions to formatters *)

let rec pp_hum_indent indent ppf = function
  | Atom str -> pp_maybe_esc_str ppf str
  | List (h :: t) ->
      pp_open_box ppf indent;
      pp_print_string ppf "(";
      pp_hum_indent indent ppf h;
      pp_hum_rest indent ppf t
  | List [] -> pp_print_string ppf "()"

and pp_hum_rest indent ppf = function
  | h :: t ->
      pp_print_space ppf ();
      pp_hum_indent indent ppf h;
      pp_hum_rest indent ppf t
  | [] ->
      pp_print_string ppf ")";
      pp_close_box ppf ()

let rec pp_mach_internal may_need_space ppf = function
  | Atom str ->
      let str' = maybe_esc_str str in
      let new_may_need_space = str' == str in
      if may_need_space && new_may_need_space then pp_print_string ppf " ";
      pp_print_string ppf str';
      new_may_need_space
  | List (h :: t) ->
      pp_print_string ppf "(";
      let may_need_space = pp_mach_internal false ppf h in
      pp_mach_rest may_need_space ppf t;
      false
  | List [] -> pp_print_string ppf "()"; false

and pp_mach_rest may_need_space ppf = function
  | h :: t ->
      let may_need_space = pp_mach_internal may_need_space ppf h in
      pp_mach_rest may_need_space ppf t
  | [] -> pp_print_string ppf ")"

let pp_hum ppf sexp = pp_hum_indent !default_indent ppf sexp

let pp_mach ppf sexp = ignore (pp_mach_internal false ppf sexp)
let pp = pp_mach

(* Sexp size *)

let rec size_loop (v, c as acc) = function
  | Atom str -> v + 1, c + String.length str
  | List lst -> List.fold_left size_loop acc lst

let size sexp = size_loop (0, 0) sexp


(* Buffer conversions *)

let to_buffer_hum ~buf ?(indent = !default_indent) sexp =
  Format.bprintf buf "%a@?" (pp_hum_indent indent) sexp

let to_buffer_mach ~buf sexp =
  let rec loop may_need_space = function
    | Atom str ->
        let str' = maybe_esc_str str in
        let new_may_need_space = str' == str in
        if may_need_space && new_may_need_space then Buffer.add_char buf ' ';
        Buffer.add_string buf str';
        new_may_need_space
    | List (h :: t) ->
        Buffer.add_char buf '(';
        let may_need_space = loop false h in
        loop_rest may_need_space t;
        false
    | List [] -> Buffer.add_string buf "()"; false
  and loop_rest may_need_space = function
    | h :: t ->
        let may_need_space = loop may_need_space h in
        loop_rest may_need_space t
    | [] -> Buffer.add_char buf ')' in
  ignore (loop false sexp)

let to_buffer = to_buffer_mach


(* Output of S-expressions to I/O-channels *)

let buffer () = Buffer.create 4096

let with_new_buffer oc f =
  let buf = buffer () in
  f buf;
  Buffer.output_buffer oc buf

let output_hum oc sexp =
  with_new_buffer oc (fun buf -> to_buffer_hum sexp ~buf)

let output_hum_indent indent oc sexp =
  with_new_buffer oc (fun buf -> to_buffer_hum ~indent sexp ~buf)

let output_mach oc sexp =
  with_new_buffer oc (fun buf -> to_buffer_mach sexp ~buf)

let output = output_mach


(* String conversions *)

let to_string_hum ?indent sexp =
  let buf = buffer () in
  to_buffer_hum ?indent sexp ~buf;
  Buffer.contents buf

let to_string_mach sexp =
  let buf = buffer () in
  to_buffer_mach sexp ~buf;
  Buffer.contents buf

let to_string = to_string_mach


(* Scan functions *)

let scan_sexp ?buf lexbuf = Parser.sexp (Lexer.main ?buf) lexbuf
let scan_sexps ?buf lexbuf = Parser.sexps (Lexer.main ?buf) lexbuf

let get_main_buf buf =
  let buf =
    match buf with
    | None -> Buffer.create 64
    | Some buf -> buf in
  Lexer.main ~buf

let scan_fold_sexps ?buf ~f ~init lexbuf =
  let main = get_main_buf buf in
  let rec loop acc =
    match Parser.sexp_opt main lexbuf with
    | None -> acc
    | Some sexp -> loop (f sexp acc) in
  loop init

let scan_iter_sexps ?buf ~f lexbuf =
  let main = get_main_buf buf in
  let rec loop () =
    match Parser.sexp_opt main lexbuf with
    | None -> ()
    | Some sexp -> f sexp; loop () in
  loop ()

let scan_cnv_sexps ?buf ~f lexbuf =
  let coll sexp acc = f sexp :: acc in
  List.rev (scan_fold_sexps ?buf ~f:coll ~init:[] lexbuf)


(* Partial parsing *)

type parse_pos =
  {
    mutable text_line : int; (** Line position in parsed text *)
    mutable text_char : int; (** Character position in parsed text *)
    mutable buf_pos : int; (** Reading position in buffer *)
  }

type 'a parse_result = Done of t * parse_pos | Cont of bool * 'a parse_fun
and 'a parse_fun = pos : int -> len : int -> 'a -> 'a parse_result

type parse_state =
  {
    parse_pos : parse_pos;
    mutable pstack : t list list;
    pbuf : Buffer.t;
  }

type parse_error =
  {
    location : string;
    err_msg : string;
    parse_state : parse_state;
  }

exception ParseError of parse_error

let bump_text_line { parse_pos = parse_pos } =
  parse_pos.text_line <- parse_pos.text_line + 1;
  parse_pos.text_char <- 1

let bump_text_pos { parse_pos = parse_pos } =
  parse_pos.text_char <- parse_pos.text_char + 1

let bump_pos_cont state str ~max_pos ~pos cont =
  bump_text_pos state;
  cont state str ~max_pos ~pos:(pos + 1)

let bump_line_cont state str ~max_pos ~pos cont =
  bump_text_line state;
  cont state str ~max_pos ~pos:(pos + 1)

let add_bump bump state str ~max_pos ~pos c cont =
  Buffer.add_char state.pbuf c;
  bump state;
  cont state str ~max_pos ~pos:(pos + 1)

let add_bump_pos state str ~max_pos ~pos c cont =
  add_bump bump_text_pos state str ~max_pos ~pos c cont

let add_bump_line state str ~max_pos ~pos c cont =
  add_bump bump_text_line state str ~max_pos ~pos c cont

let mk_parse_pos { parse_pos = parse_pos } buf_pos =
  parse_pos.buf_pos <- buf_pos;
  parse_pos

let bump_found_atom bump state str ~max_pos ~pos cont =
  let pbuf = state.pbuf in
  let atom = Atom (Buffer.contents pbuf) in
  match state.pstack with
  | [] -> Done (atom, mk_parse_pos state pos)
  | rev_sexp_lst :: sexp_stack ->
      Buffer.clear pbuf;
      state.pstack <- (atom :: rev_sexp_lst) :: sexp_stack;
      bump state;
      cont state str ~max_pos ~pos:(pos + 1)

let raise_parse_error state location err_msg =
  let parse_error =
    {
      location = location;
      err_msg = err_msg;
      parse_state = state;
    }
  in
  raise (ParseError parse_error)

let raise_unexpected_char state ~loc pos c =
  let err_msg = sprintf "unexpected character: '%c'" c in
  let parse_pos = state.parse_pos in
  parse_pos.buf_pos <- pos;
  parse_pos.text_char <- parse_pos.text_char + 1;
  raise_parse_error state loc err_msg

(* Macro for generating parsers *)
# 521 "pre_sexp.ml.in"
let check_str_bounds loc ~pos ~len (str : string) = if pos < 0 then invalid_arg (loc ^ ": pos < 0"); if len < 0 then invalid_arg (loc ^ ": len < 0"); let str_len = String.length str in let pos_len = pos + len in if pos_len > str_len then invalid_arg (loc ^ ": pos + len > str_len"); pos_len - 1 let mk_cont name cont state = let ws_only = state.pstack = [] && Buffer.length state.pbuf = 0 in let parse_fun ~pos ~len str = let max_pos = check_str_bounds name ~pos ~len str in cont state str ~max_pos ~pos in Cont (ws_only, parse_fun) let rec parse_str state str ~max_pos ~pos = if pos > max_pos then mk_cont "parse" parse_str state else match str.[pos] with | '(' -> state.pstack <- [] :: state.pstack; bump_pos_cont state str ~max_pos ~pos parse_str | ')' as c -> (match state.pstack with | [] -> raise_unexpected_char state ~loc:"parse" pos c | rev_sexp_lst :: sexp_stack -> let sexp = List (List.rev rev_sexp_lst) in match sexp_stack with | [] -> Done (sexp, mk_parse_pos state (pos + 1)) | higher_rev_sexp_lst :: higher_sexp_stack -> state.pstack <- (sexp :: higher_rev_sexp_lst) :: higher_sexp_stack; bump_pos_cont state str ~max_pos ~pos parse_str) | ' ' | '\009' | '\012' -> bump_pos_cont state str ~max_pos ~pos parse_str | '\010' -> bump_line_cont state str ~max_pos ~pos parse_str | '\013' -> bump_line_cont state str ~max_pos ~pos parse_nl | ';' -> bump_pos_cont state str ~max_pos ~pos parse_comment | '"' -> bump_pos_cont state str ~max_pos ~pos parse_quoted | c -> add_bump_pos state str ~max_pos ~pos c parse_atom and parse_nl state str ~max_pos ~pos = if pos > max_pos then mk_cont "parse_nl" parse_nl state else let pos = if str.[pos] = '\010' then pos + 1 else pos in parse_str state str ~max_pos ~pos and parse_comment state str ~max_pos ~pos = if pos > max_pos then mk_cont "parse_comment" parse_comment state else match str.[pos] with | '\010' -> bump_line_cont state str ~max_pos ~pos parse_str | '\013' -> bump_line_cont state str ~max_pos ~pos parse_nl | _ -> bump_pos_cont state str ~max_pos ~pos parse_comment and parse_atom state str ~max_pos ~pos = if pos > max_pos then mk_cont "parse_atom" parse_atom state else match str.[pos] with | ' ' | '\009' | '\012' -> bump_found_atom bump_text_pos state str ~max_pos ~pos parse_str | '(' -> let pbuf = state.pbuf in let atom = Atom (Buffer.contents pbuf) in (match state.pstack with | [] -> Done (atom, mk_parse_pos state pos) | rev_sexp_lst :: sexp_stack -> Buffer.clear pbuf; state.pstack <- [] :: (atom :: rev_sexp_lst) :: sexp_stack; bump_pos_cont state str ~max_pos ~pos parse_str) | ')' -> let pbuf = state.pbuf in let atom = Atom (Buffer.contents pbuf) in (match state.pstack with | [] -> Done (atom, mk_parse_pos state pos) | rev_sexp_lst :: sexp_stack -> let sexp = List (List.rev_append rev_sexp_lst [atom]) in match sexp_stack with | [] -> Done (sexp, mk_parse_pos state (pos + 1)) | higher_rev_sexp_lst :: higher_sexp_stack -> Buffer.clear pbuf; state.pstack <- (sexp :: higher_rev_sexp_lst) :: higher_sexp_stack; bump_pos_cont state str ~max_pos ~pos parse_str) | '\010' -> bump_found_atom bump_text_line state str ~max_pos ~pos parse_str | '\013' -> bump_found_atom bump_text_line state str ~max_pos ~pos parse_nl | ';' -> bump_found_atom bump_text_pos state str ~max_pos ~pos parse_comment | '"' -> bump_found_atom bump_text_pos state str ~max_pos ~pos parse_quoted | c -> add_bump_pos state str ~max_pos ~pos c parse_atom and parse_quoted state str ~max_pos ~pos = if pos > max_pos then mk_cont "parse_quoted" parse_quoted state else match str.[pos] with | '"' -> let pbuf = state.pbuf in let atom = Atom (Buffer.contents pbuf) in (match state.pstack with | [] -> Done (atom, mk_parse_pos state (pos + 1)) | rev_sexp_lst :: sexp_stack -> Buffer.clear pbuf; state.pstack <- (atom :: rev_sexp_lst) :: sexp_stack; bump_pos_cont state str ~max_pos ~pos parse_str) | '\\' -> bump_pos_cont state str ~max_pos ~pos parse_escaped | '\010' as c -> add_bump_line state str ~max_pos ~pos c parse_quoted | '\013' as c -> add_bump_line state str ~max_pos ~pos c parse_quoted_nl | c -> add_bump_pos state str ~max_pos ~pos c parse_quoted and parse_quoted_nl state str ~max_pos ~pos = if pos > max_pos then mk_cont "parse_quoted_nl" parse_quoted_nl state else let pos = let c = '\010' in if str.[pos] = c then ( Buffer.add_char state.pbuf c; pos + 1 ) else pos in parse_quoted state str ~max_pos ~pos and parse_escaped state str ~max_pos ~pos = if pos > max_pos then mk_cont "parse_escaped" parse_escaped state else match str.[pos] with | '\010' -> bump_line_cont state str ~max_pos ~pos parse_skip_ws | '\013' -> bump_line_cont state str ~max_pos ~pos parse_skip_ws_nl | '0' .. '9' as c -> bump_text_pos state; let d = Char.code c - 48 in parse_dec state str ~max_pos ~pos:(pos + 1) ~count:2 ~d | 'x' -> bump_text_pos state; parse_hex state str ~max_pos ~pos:(pos + 1) ~count:2 ~d:0 | ('\\' | '"' | '\'' ) as c -> add_bump_pos state str ~max_pos ~pos c parse_quoted | 'n' -> add_bump_pos state str ~max_pos ~pos '\n' parse_quoted | 't' -> add_bump_pos state str ~max_pos ~pos '\t' parse_quoted | 'b' -> add_bump_pos state str ~max_pos ~pos '\b' parse_quoted | 'r' -> add_bump_pos state str ~max_pos ~pos '\r' parse_quoted | c -> Buffer.add_char state.pbuf '\\'; add_bump_pos state str ~max_pos ~pos c parse_quoted and parse_skip_ws state str ~max_pos ~pos = if pos > max_pos then mk_cont "parse_skip_ws" parse_skip_ws state else match str.[pos] with | ' ' | '\009' -> bump_pos_cont state str ~max_pos ~pos parse_skip_ws | _ -> parse_quoted state str ~max_pos ~pos and parse_skip_ws_nl state str ~max_pos ~pos = if pos > max_pos then mk_cont "parse_skip_ws_nl" parse_skip_ws_nl state else let pos = if str.[pos] = '\010' then pos + 1 else pos in parse_skip_ws state str ~max_pos ~pos and parse_dec state str ~max_pos ~pos ~count ~d = if pos > max_pos then mk_cont "parse_dec" (parse_dec ~count ~d) state else match str.[pos] with | '0' .. '9' as c -> let d = 10 * d + Char.code c - 48 in if count = 1 then if d > 255 then let err_msg = sprintf "illegal decimal escape: \\%d" d in raise_parse_error state "parse_dec" err_msg else add_bump_pos state str ~max_pos ~pos (Char.chr d) parse_quoted else ( bump_text_pos state; parse_dec state str ~max_pos ~pos:(pos + 1) ~count:(count - 1) ~d) | c -> raise_unexpected_char state ~loc:"parse_dec" pos c and parse_hex state str ~max_pos ~pos ~count ~d = if pos > max_pos then mk_cont "parse_hex" (parse_hex ~count ~d) state else match str.[pos] with | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' as c -> let corr = if c >= 'a' then 87 else if c >= 'A' then 55 else 48 in let d = 16 * d + Char.code c - corr in if count = 1 then if d > 255 then let err_msg = sprintf "illegal hexadecimal escape: \\%x" d in raise_parse_error state "parse_hex" err_msg else add_bump_pos state str ~max_pos ~pos (Char.chr d) parse_quoted else ( bump_text_pos state; parse_hex state str ~max_pos ~pos:(pos + 1) ~count:(count - 1) ~d) | c -> raise_unexpected_char state ~loc:"parse_hex" pos c let parse_str ?(text_line = 1) ?(text_char = 1) ?(pos = 0) ?len str = let len = match len with | Some len -> len | None -> String.length str - pos in let max_pos = check_str_bounds "parse" ~pos ~len str in let state = { parse_pos = { text_line = text_line; text_char = text_char; buf_pos = pos; }; pstack = []; pbuf = Buffer.create 128; } in parse_str state str ~max_pos ~pos

let parse = parse_str

let plain_parse ~pos ~len str = parse ~pos ~len str


(* Partial parsing from bigstrings *)

(* NOTE: this is really an awful duplication of the code for parsing
   strings, but since OCaml does not inline higher-order functions known
   at compile, other solutions would sacrifice a lot of efficiency. *)

open Bigarray

type bstr = (char, int8_unsigned_elt, c_layout) Array1.t

let check_str_bounds loc ~pos ~len (str : bstr) = if pos < 0 then invalid_arg (loc ^ ": pos < 0"); if len < 0 then invalid_arg (loc ^ ": len < 0"); let str_len = Array1.dim str in let pos_len = pos + len in if pos_len > str_len then invalid_arg (loc ^ ": pos + len > str_len"); pos_len - 1 let mk_cont name cont state = let ws_only = state.pstack = [] && Buffer.length state.pbuf = 0 in let parse_fun ~pos ~len str = let max_pos = check_str_bounds name ~pos ~len str in cont state str ~max_pos ~pos in Cont (ws_only, parse_fun) let rec parse_bstr state str ~max_pos ~pos = if pos > max_pos then mk_cont "parse" parse_bstr state else match str.{pos} with | '(' -> state.pstack <- [] :: state.pstack; bump_pos_cont state str ~max_pos ~pos parse_bstr | ')' as c -> (match state.pstack with | [] -> raise_unexpected_char state ~loc:"parse" pos c | rev_sexp_lst :: sexp_stack -> let sexp = List (List.rev rev_sexp_lst) in match sexp_stack with | [] -> Done (sexp, mk_parse_pos state (pos + 1)) | higher_rev_sexp_lst :: higher_sexp_stack -> state.pstack <- (sexp :: higher_rev_sexp_lst) :: higher_sexp_stack; bump_pos_cont state str ~max_pos ~pos parse_bstr) | ' ' | '\009' | '\012' -> bump_pos_cont state str ~max_pos ~pos parse_bstr | '\010' -> bump_line_cont state str ~max_pos ~pos parse_bstr | '\013' -> bump_line_cont state str ~max_pos ~pos parse_nl | ';' -> bump_pos_cont state str ~max_pos ~pos parse_comment | '"' -> bump_pos_cont state str ~max_pos ~pos parse_quoted | c -> add_bump_pos state str ~max_pos ~pos c parse_atom and parse_nl state str ~max_pos ~pos = if pos > max_pos then mk_cont "parse_nl" parse_nl state else let pos = if str.{pos} = '\010' then pos + 1 else pos in parse_bstr state str ~max_pos ~pos and parse_comment state str ~max_pos ~pos = if pos > max_pos then mk_cont "parse_comment" parse_comment state else match str.{pos} with | '\010' -> bump_line_cont state str ~max_pos ~pos parse_bstr | '\013' -> bump_line_cont state str ~max_pos ~pos parse_nl | _ -> bump_pos_cont state str ~max_pos ~pos parse_comment and parse_atom state str ~max_pos ~pos = if pos > max_pos then mk_cont "parse_atom" parse_atom state else match str.{pos} with | ' ' | '\009' | '\012' -> bump_found_atom bump_text_pos state str ~max_pos ~pos parse_bstr | '(' -> let pbuf = state.pbuf in let atom = Atom (Buffer.contents pbuf) in (match state.pstack with | [] -> Done (atom, mk_parse_pos state pos) | rev_sexp_lst :: sexp_stack -> Buffer.clear pbuf; state.pstack <- [] :: (atom :: rev_sexp_lst) :: sexp_stack; bump_pos_cont state str ~max_pos ~pos parse_bstr) | ')' -> let pbuf = state.pbuf in let atom = Atom (Buffer.contents pbuf) in (match state.pstack with | [] -> Done (atom, mk_parse_pos state pos) | rev_sexp_lst :: sexp_stack -> let sexp = List (List.rev_append rev_sexp_lst [atom]) in match sexp_stack with | [] -> Done (sexp, mk_parse_pos state (pos + 1)) | higher_rev_sexp_lst :: higher_sexp_stack -> Buffer.clear pbuf; state.pstack <- (sexp :: higher_rev_sexp_lst) :: higher_sexp_stack; bump_pos_cont state str ~max_pos ~pos parse_bstr) | '\010' -> bump_found_atom bump_text_line state str ~max_pos ~pos parse_bstr | '\013' -> bump_found_atom bump_text_line state str ~max_pos ~pos parse_nl | ';' -> bump_found_atom bump_text_pos state str ~max_pos ~pos parse_comment | '"' -> bump_found_atom bump_text_pos state str ~max_pos ~pos parse_quoted | c -> add_bump_pos state str ~max_pos ~pos c parse_atom and parse_quoted state str ~max_pos ~pos = if pos > max_pos then mk_cont "parse_quoted" parse_quoted state else match str.{pos} with | '"' -> let pbuf = state.pbuf in let atom = Atom (Buffer.contents pbuf) in (match state.pstack with | [] -> Done (atom, mk_parse_pos state (pos + 1)) | rev_sexp_lst :: sexp_stack -> Buffer.clear pbuf; state.pstack <- (atom :: rev_sexp_lst) :: sexp_stack; bump_pos_cont state str ~max_pos ~pos parse_bstr) | '\\' -> bump_pos_cont state str ~max_pos ~pos parse_escaped | '\010' as c -> add_bump_line state str ~max_pos ~pos c parse_quoted | '\013' as c -> add_bump_line state str ~max_pos ~pos c parse_quoted_nl | c -> add_bump_pos state str ~max_pos ~pos c parse_quoted and parse_quoted_nl state str ~max_pos ~pos = if pos > max_pos then mk_cont "parse_quoted_nl" parse_quoted_nl state else let pos = let c = '\010' in if str.{pos} = c then ( Buffer.add_char state.pbuf c; pos + 1 ) else pos in parse_quoted state str ~max_pos ~pos and parse_escaped state str ~max_pos ~pos = if pos > max_pos then mk_cont "parse_escaped" parse_escaped state else match str.{pos} with | '\010' -> bump_line_cont state str ~max_pos ~pos parse_skip_ws | '\013' -> bump_line_cont state str ~max_pos ~pos parse_skip_ws_nl | '0' .. '9' as c -> bump_text_pos state; let d = Char.code c - 48 in parse_dec state str ~max_pos ~pos:(pos + 1) ~count:2 ~d | 'x' -> bump_text_pos state; parse_hex state str ~max_pos ~pos:(pos + 1) ~count:2 ~d:0 | ('\\' | '"' | '\'' ) as c -> add_bump_pos state str ~max_pos ~pos c parse_quoted | 'n' -> add_bump_pos state str ~max_pos ~pos '\n' parse_quoted | 't' -> add_bump_pos state str ~max_pos ~pos '\t' parse_quoted | 'b' -> add_bump_pos state str ~max_pos ~pos '\b' parse_quoted | 'r' -> add_bump_pos state str ~max_pos ~pos '\r' parse_quoted | c -> Buffer.add_char state.pbuf '\\'; add_bump_pos state str ~max_pos ~pos c parse_quoted and parse_skip_ws state str ~max_pos ~pos = if pos > max_pos then mk_cont "parse_skip_ws" parse_skip_ws state else match str.{pos} with | ' ' | '\009' -> bump_pos_cont state str ~max_pos ~pos parse_skip_ws | _ -> parse_quoted state str ~max_pos ~pos and parse_skip_ws_nl state str ~max_pos ~pos = if pos > max_pos then mk_cont "parse_skip_ws_nl" parse_skip_ws_nl state else let pos = if str.{pos} = '\010' then pos + 1 else pos in parse_skip_ws state str ~max_pos ~pos and parse_dec state str ~max_pos ~pos ~count ~d = if pos > max_pos then mk_cont "parse_dec" (parse_dec ~count ~d) state else match str.{pos} with | '0' .. '9' as c -> let d = 10 * d + Char.code c - 48 in if count = 1 then if d > 255 then let err_msg = sprintf "illegal decimal escape: \\%d" d in raise_parse_error state "parse_dec" err_msg else add_bump_pos state str ~max_pos ~pos (Char.chr d) parse_quoted else ( bump_text_pos state; parse_dec state str ~max_pos ~pos:(pos + 1) ~count:(count - 1) ~d) | c -> raise_unexpected_char state ~loc:"parse_dec" pos c and parse_hex state str ~max_pos ~pos ~count ~d = if pos > max_pos then mk_cont "parse_hex" (parse_hex ~count ~d) state else match str.{pos} with | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' as c -> let corr = if c >= 'a' then 87 else if c >= 'A' then 55 else 48 in let d = 16 * d + Char.code c - corr in if count = 1 then if d > 255 then let err_msg = sprintf "illegal hexadecimal escape: \\%x" d in raise_parse_error state "parse_hex" err_msg else add_bump_pos state str ~max_pos ~pos (Char.chr d) parse_quoted else ( bump_text_pos state; parse_hex state str ~max_pos ~pos:(pos + 1) ~count:(count - 1) ~d) | c -> raise_unexpected_char state ~loc:"parse_hex" pos c let parse_bstr ?(text_line = 1) ?(text_char = 1) ?(pos = 0) ?len str = let len = match len with | Some len -> len | None -> Array1.dim str - pos in let max_pos = check_str_bounds "parse" ~pos ~len str in let state = { parse_pos = { text_line = text_line; text_char = text_char; buf_pos = pos; }; pstack = []; pbuf = Buffer.create 128; } in parse_bstr state str ~max_pos ~pos


(* Input functions *)

let reraise_parse_error pe global_pos =
  let ps = pe.parse_state in
  let ppos = ps.parse_pos in
  let new_ppos = { ppos with buf_pos = global_pos + ppos.buf_pos } in
  let new_ps = { ps with parse_pos = new_ppos } in
  let new_pe = { pe with parse_state = new_ps } in
  raise (ParseError new_pe)

let input_sexp ?text_line ?text_char ?(buf_pos = 0) ic =
  let buf = String.create 1 in
  let rec loop this_parse =
    let c = input_char ic in
    buf.[0] <- c;
    let parse_res =
      try this_parse ~pos:0 ~len:1 buf
      with ParseError pe -> reraise_parse_error pe buf_pos
    in
    match parse_res with
    | Done (sexp, _) -> sexp
    | Cont (_, this_parse) -> loop this_parse
  in
  let this_parse ~pos ~len str = parse ?text_line ?text_char ~pos ~len str in
  loop this_parse

let input_rev_sexps
      ?text_line ?text_char
      ?(buf_pos = 0) ?(buf = String.create 8192) ic =
  let rev_sexps_ref = ref [] in
  let buf_len = String.length buf in
  let is_incomplete_ref = ref false in
  let buf_pos_ref = ref buf_pos in
  let rec loop this_parse pos len =
    if len > 0 then
      let parse_res =
        try this_parse ~pos ~len buf
        with ParseError pe -> reraise_parse_error pe !buf_pos_ref
      in
      match parse_res with
      | Done (sexp, new_pos) ->
          rev_sexps_ref := sexp :: !rev_sexps_ref;
          let n_parsed = new_pos.buf_pos - pos in
          is_incomplete_ref := false;
          let text_line = new_pos.text_line in
          let text_char = new_pos.text_char in
          let this_parse ~pos ~len str =
            parse ~text_line ~text_char ~pos ~len str
          in
          if n_parsed = len then
            let new_len = input ic buf 0 buf_len in
            buf_pos_ref := !buf_pos_ref + new_pos.buf_pos;
            loop this_parse 0 new_len
          else loop this_parse new_pos.buf_pos (len - n_parsed)
      | Cont (ws_only, this_parse) ->
          is_incomplete_ref := not ws_only;
          buf_pos_ref := !buf_pos_ref + len + pos;
          loop this_parse 0 (input ic buf 0 buf_len)
    else if !is_incomplete_ref then raise End_of_file
    else !rev_sexps_ref
  in
  let this_parse ~pos ~len str = parse ?text_line ?text_char ~pos ~len str in
  loop this_parse 0 (input ic buf 0 buf_len)

let input_sexps ?text_line ?text_char ?buf_pos ?buf ic =
  let rev_sexps = input_rev_sexps ?text_line ?text_char ?buf_pos ?buf ic in
  List.rev rev_sexps

(* of_string and of_bstr *)

let of_string_bstr loc this_parse ws_buf get_len get_sub str =
  match this_parse str with
  | Done (_, { buf_pos = buf_pos }) when buf_pos <> get_len str ->
      let prefix_len = min (get_len str - buf_pos) 20 in
      let prefix = get_sub str buf_pos prefix_len in
      let msg =
        sprintf "%s: S-expression followed by data at position %d: %S..."
          loc buf_pos prefix
      in
      failwith msg
  | Done (sexp, _) -> sexp
  | Cont (ws_only, this_parse) ->
      if ws_only then failwith (loc ^ ": whitespace only");
      match this_parse ~pos:0 ~len:1 ws_buf with
      | Done (sexp, _) -> sexp
      | Cont _ -> failwith (loc ^ ": incomplete S-expression")

let of_string str =
  of_string_bstr "Sexp.of_string" parse " " String.length String.sub str

let get_bstr_sub_str bstr pos len =
  let str = String.create len in
  for i = 0 to len - 1 do str.[i] <- bstr.{pos + i} done;
  str

let bstr_ws_buf = Array1.create char c_layout 1
let () = bstr_ws_buf.{0} <- ' '

let of_bstr bstr =
  of_string_bstr "Sexp.of_bstr"
    parse_bstr bstr_ws_buf Array1.dim get_bstr_sub_str bstr

(* Loading *)

let load_sexp ?(buf = String.create 8192) file =
  let buf_len = String.length buf in
  let ic = open_in file in
  let rec loop this_parse =
    let len = input ic buf 0 buf_len in
    if len = 0 then raise End_of_file
    else
      match this_parse ~pos:0 ~len buf with
      | Done (sexp, _) -> sexp
      | Cont (_, this_parse) -> loop this_parse
  in
  try
    let sexp = loop plain_parse in
    close_in ic;
    sexp
  with exc -> close_in_noerr ic; raise exc

let load_rev_sexps ?buf file =
  let ic = open_in file in
  try
    let sexps = input_rev_sexps ?buf ic in
    close_in ic;
    sexps
  with exc -> close_in_noerr ic; raise exc

let load_sexps ?buf file =
  let rev_sexps = load_rev_sexps ?buf file in
  List.rev rev_sexps


(* Utilities for automated type conversions *)

let unit = List []

external sexp_of_t : t -> t = "%identity"
external t_of_sexp : t -> t = "%identity"
