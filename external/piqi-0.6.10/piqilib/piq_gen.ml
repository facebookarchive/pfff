(*
   Copyright 2009, 2010, 2011, 2012, 2013, 2014 Anton Lavrik

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)


open Piqi_common
open Iolist 


(* split (utf8) string into individual lines treating '\n' as a separator *)
let split_text s =
  let rec aux len i accu =
    if i < 0
    then
      let res = String.sub s 0 len in
      res::accu
    else
      if s.[i] = '\n'
      then
        let res = String.sub s (i + 1) len in
        aux 0 (i - 1) (res::accu)
      else
        aux (len + 1) (i - 1) accu
  in
  aux 0 (String.length s - 1) []


let make_text_line s =
  if s = ""
  then ios "#"
  else ios "# " ^^ ios s


(* NOTE: list is not empty *)
let print_text l =
  let l = List.fold_left
    (fun accu x -> eol :: (make_text_line x) :: accu) [] l
  in
  iol (List.rev l)


let rec is_multiline = function
  | Ios s -> String.contains s '\n'
  | Iol l -> List.fold_left (fun accu x -> accu || is_multiline x) false l
  | Iob '\n' -> true
  | Indent | Unindent | Eol -> true
  | _ -> false


let uint64_to_string x =
  (* XXX: printing large unsigned uint values in hex *)
  if Int64.compare x 0L >= 0
  then Printf.sprintf "%Lu" x
  else Printf.sprintf "0x%Lx" x


(* This method for printing floats is borrowed from Martin Jambon's Yojson
 * library, see piqi_json_gen.ml for details. *)
(*
  Ensure that the float is not printed as an int.
  This is not required by JSON, but useful in order to guarantee
  reversibility.
*)
let float_needs_period s =
  try
    for i = 0 to String.length s - 1 do
      match s.[i] with
	  '0'..'9' | '-' -> ()
	| _ -> raise Exit
    done;
    true
  with Exit ->
    false

(*
  Both write_float_fast and write_float guarantee
  that a sufficient number of digits are printed in order to 
  allow reversibility.

  The _fast version is faster but often produces unnecessarily long numbers.
*)

let write_float ob x =
  let s1 = Printf.sprintf "%.16g" x in
  let s =
    if float_of_string s1 = x then s1
    else Printf.sprintf "%.17g" x
  in
  Buffer.add_string ob s;
  if float_needs_period s then
    Buffer.add_string ob ".0"

(*
let write_float_fast ob x =
  let s = Printf.sprintf "%.17g" x in
  Buffer.add_string ob s;
  if float_needs_period s then
    Buffer.add_string ob ".0"

let write_float = write_float_fast
*)

let string_of_float x =
  let ob = Buffer.create 20 in
  write_float ob x;
  Buffer.contents ob


(* XXX: providing custom version since Pervasives.string_of_float will add
 * trailing "." to the literal *)
let format_float x =
  match Pervasives.classify_float x with
    | FP_nan -> "0.nan"
    | FP_infinite  ->   (** Number is positive or negative infinity *)
        if x = Pervasives.infinity
        then "0.inf"
        else "-0.inf"
    | FP_normal         (** Normal number, none of the below *)
    | FP_zero           (** Number is 0.0 or -0.0 *)
    | FP_subnormal ->   (** Number very close to 0.0, has reduced precision *)
        string_of_float x


(*
 * Pretty-printing
 *)

module Fmt = Easy_format


let common_list =
  Fmt.({
    list with
    indent_body = 4;
  })

let atom_list =
  Fmt.({
    common_list with
    wrap_body = `Always_wrap;
  })

let single_elem_list =
  Fmt.({
    common_list with
    wrap_body = `Always_wrap;
  })

let multiple_elem_list =
  Fmt.({
    common_list with
    wrap_body = `Force_breaks;
  })

let form_list =
  Fmt.({
    common_list with
    space_after_opening = false;
    space_before_closing = false;
  })

let multi_form_list =
  Fmt.({
    form_list with
    wrap_body = `Force_breaks;
  })

let single_form_list =
  Fmt.({
    form_list with
    wrap_body = `Always_wrap;
  })

let atom_form_list =
  Fmt.({
    form_list with
    wrap_body = `Always_wrap;
  })

let make_atom x =
  Fmt.Atom (x, Fmt.atom)


let is_atom = function
  | Fmt.Atom _ -> true
  | _ -> false


let rec has_list = function
  | Fmt.List _ -> true
  | Fmt.Custom _ -> true
  | Fmt.Label ((label, _), node) ->
      if has_list label
      then true
      else has_list node
  | _ -> false


let make_list l =
  let fmt =
    match l with
      | [] ->
          single_elem_list
      | [x] ->
          if has_list x
          then multiple_elem_list
          else single_elem_list
      | _ ->
          if List.for_all is_atom l
          then atom_list
          else multiple_elem_list
  in
  Fmt.List (("[", "", "]", fmt), l)


let make_form_fmt args =
  (* TODO: unify this with similar code in make_list *)
  match args with
    | [] ->
        single_form_list
    | [x] ->
        if has_list x
        then multi_form_list
        else single_form_list
    | l ->
        if List.for_all is_atom l
        then atom_form_list
        else multi_form_list


let make_form name args =
  let fmt = make_form_fmt args in
  let extra_space = (* add space after name it is followed by args *)
    if args <> []
    then " "
    else ""
  in
  Fmt.List (("(" ^ name ^ extra_space, "", ")", fmt), args)


(* parenthesis around an ast element *)
let make_parens ast =
  let fmt = make_form_fmt [ast] in
  Fmt.List (("(", "", ")", fmt), [ast])


let make_label label node =
  let fmt = Fmt.({
    label with
    indent_after_label = 4;
  })
  in
  Fmt.Label ((label, fmt), node)


let quote s = "\"" ^ s ^ "\""


let format_text_line ?(indent=false) s =
  let space =
    if indent
    then "    " (* standard 4 space indentation after label *)
    else "" (* no indentation *)
  in
  if s = ""
  then space ^ "#"
  else space ^ "# " ^ s


(* ~is_labeled = true if text appears after a label;
 * ~is_first = true if text is the first element in the list *)
let format_text l ~is_labeled ~is_first =
  match l with
    | [] ->
        assert false
    | [x] when is_labeled -> (* single text line after label *)
        (* try to put a single text line on the same line with its label *)
        let fmt =
          Fmt.({
            common_list with
            wrap_body = `Force_breaks;
            align_closing = false;
            space_after_opening = false;
            space_before_closing = false;
        })
        in
        (* no opening, closing; break after each item; standard 4-space
         * indentation *)
        let line = format_text_line x in
        Fmt.List (("", "", "", fmt), [make_atom line])
    | h::t -> (* more than one lines of text *)
        (* print several lines them as one block; indented if it appears after
         * a label *)
        Fmt.Custom (fun fmt ->
          (* force new line before text block if it appears after a label or if
            * it is not the first element of the list *)
          if is_labeled || not is_first
          then Format.pp_force_newline fmt ();

          let print_line s =
            let line = format_text_line s ~indent:is_labeled in
            Format.pp_print_string fmt line
          in
          print_line h;
          List.iter (fun x ->
            Format.pp_force_newline fmt ();
            print_line x;
          ) t;
        )


(* we need to take `name in parenthesis unless followed by `named or another
 * `name *)
let preprocess_names l =
  let rec aux l =
    match l with
      | [] | [_] -> l
      | (`name _) as name :: t ->
          let t = aux t in  (* we need to process the list from right to left *)
          (match List.hd t with
            | `name _ | `named _ | `typename _ | `typed _ ->
                (* leave unchanged *)
                name :: t
            | _ ->
                (* if followed by anything else, we need to take the name in
                 * parenthesis *)
                `form (name, []) :: t
          )
      | h :: t ->
          h :: (aux t)
  in
  aux l


let format_ast (x :piq_ast) =
  let rec aux ?(is_labeled=false) ?(is_first=false) = function
    | `int (x, "") -> make_atom (Int64.to_string x)
    | `uint (x, "") -> make_atom (uint64_to_string x)
    | `float (x, "") -> make_atom (format_float x)

    | `ascii_string (s, "") | `utf8_string (s, "") ->
        make_atom (quote (Piq_lexer.escape_string s))
    | `binary (s, "") ->
        make_atom (quote (Piq_lexer.escape_binary s))

    (* use original literals when they are available *)
    | `int (_, s)
    | `uint (_, s)
    | `float (_, s) -> make_atom s
    | `ascii_string (_, s)
    | `utf8_string (_, s)
    | `binary (_, s) -> make_atom (quote s)

    | `raw_binary s ->
        (* This literal can't be read back reliably after printing, and it
         * doesn't come from Piq, but we still need to print it somehow -- in
         * case if it is present. *)
        (* XXX: printing it is as binary for now, but may try to print it as
         * utf8 string if it does represet a valid string. *)
        make_atom (quote (Piq_lexer.escape_binary s))

    | `bool true -> make_atom "true"
    | `bool false -> make_atom "false"
    | `word s -> make_atom s
    | `text s -> format_text (split_text s) ~is_labeled ~is_first
    | `name s -> make_atom ("." ^ s)
    | `typename s ->
        let name = ":" ^ s in
        (* parentheses are not needed if `typename is followed by `typed,
         * `named, `name or another `typename, but using them anyway for now *)
        make_form name []
    | `named {Piq_ast.Named.name = n; Piq_ast.Named.value = v} ->
        let joined_labels, node = format_labeled_ast v in
        let name = "." ^ n ^ joined_labels in
        (match node with
          | None ->
              make_atom name
          | Some node ->
              make_label (make_atom name) node
        )
    | `typed {Piq_ast.Typed.typename = n; Piq_ast.Typed.value = v} ->
        let joined_labels, node = format_labeled_ast v in
        let name = ":" ^ n ^ joined_labels in
        if not is_labeled
        then
          match node with
            | None ->
                make_atom name
            | Some node ->
                make_label (make_atom name) node
        else
          let nodes =
            match node with
              | None -> []
              | Some node -> [node]
          in
          (* wrap typed in parenthesis by creating a (:<typename> ...) form *)
          (make_form name nodes)
    | `list [] ->
        make_atom "[]"
    | `list l ->
        make_list (map_aux l)
    | `form (name, args) ->
        (match name with
          | (#Piq_ast.form_name as form_name) -> (* this is a form *)
              let name =
                match form_name with
                  | `word s -> s
                  | `name s -> "." ^ s
                  | `typename s -> ":" ^ s
              in
              make_form name (map_aux args)
          | ast -> (* this is an ast element in parenthesis *)
              make_parens (aux ast)
        )
    | `any _ -> (* shouldn't happen except when C.debug_level > 0 *)
        make_atom "!PIQI-ANY!"

  and map_aux l =
    match l with
      | [] -> []
      | l ->
          (* we need to take `name in parenthesis unless followed by `named or
           * another `name *)
          let l = preprocess_names l in
          (aux (List.hd l) ~is_first:true)::(List.map aux (List.tl l))

  and format_labeled_ast = function
    | `name n ->
        "." ^ n, None
    | `named {Piq_ast.Named.name = n; Piq_ast.Named.value = v} ->
        let joined_labels, node = format_labeled_ast v in
        "." ^ n ^ joined_labels, node
    | x ->
        "", Some (aux x ~is_labeled:true)
  in
  aux x ~is_first:true


(* TODO: remove trailing line whitespace left by the pretty-printing library *)
let to_buffer ?(nl = true) buf x =
  Fmt.Pretty.to_buffer buf (format_ast x);
  if nl then Buffer.add_char buf '\n'
    
    
let to_string ?(nl = true) x =
  let buf = Buffer.create 256 in
  to_buffer ~nl buf x;
  Buffer.contents buf


let to_channel ch x =
  Fmt.Pretty.to_channel ch (format_ast x);
  output_char ch '\n' (* make sure that text file ends with a newline *)

