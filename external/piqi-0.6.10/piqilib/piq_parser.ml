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


module L = Piq_lexer


(* tokenize '[.:]'-separated string; return separator character as string
 * between separated tokens;
 *
 * Also, treat '.' character inside domain name part of typenames as a normal
 * name character
*) 

let tokenize_name ?start s =
  let l = U.string_split s '.' ?start in
  match l with
    | h::t -> h :: (U.flatmap (fun x -> ["."; x]) t)
    | _ -> assert false


let tokenize_typename s =
  let start =
    (* tokenize only the rightmost part of the pathname *)
    try String.rindex s '/' + 1
    with Not_found -> 0
  in
  let names = tokenize_name s ~start in
  ":" :: names


let tokenize_name first_c s =
  let parts = U.string_split s ':' in
  match first_c with
    | ":" -> U.flatmap tokenize_typename parts
    | "." ->
        (match parts with
           | h::t ->
               "." :: (tokenize_name h) @ (U.flatmap tokenize_typename t)
           | _ -> assert false)
    | _ -> assert false


let check_name n =
  (* XXX: this should refer to piq rather than piqi name *)
  if Piqi_name.is_valid_name n ~allow:"."
  then ()
  else error n ("invalid name: " ^ U.quote n)


let check_typename n =
  if Piqi_name.is_valid_typename n ~allow:"."
  then ()
  else error n ("invalid type name: " ^ U.quote n)


let piq_addrefret dst (src :piq_ast) =
  let f iner_src =
    Piqloc.addref dst iner_src;
    Piqloc.addrefret dst src;
  in
  match src with
    | `int x -> f x
    | `uint x -> f x
    | `float x -> f x
    | `bool x -> f x
    | `ascii_string x -> f x
    | `utf8_string x -> f x
    | `binary x -> f x
    | `word x -> f x
    | `text x -> f x
    | `name x -> f x
    | `typename x -> f x
    | `named x -> f x
    | `typed x -> f x
    | `list x -> f x
    | `form x -> f x
    | `raw_binary x -> f x
    | `any _ ->
        assert false
  

let piq_reference f x =
  let res = f x in
  if Obj.repr res == Obj.repr x (* the object is unchanged -- nothing to do *)
  then res
  else piq_addrefret x res


let make_named n v :piq_ast =
  check_name n;
  match v with
    | None -> `name n
    | Some v ->
        let res = Piq_ast.Named.({name = n; value = v}) in
        Piqloc.addref n res;
        `named res


let make_typed n v :piq_ast =
  check_typename n;
  match v with
    | None -> `typename n
    | Some v ->
        let res = Piq_ast.Typed.({typename = n; value = v}) in
        Piqloc.addref n res;
        `typed res


let make_named_or_typed c name n v =
  Piqloc.addref name n;
  let res =
    match c with
      | "." -> make_named n v
      | ":" -> make_typed n v
      | _ -> assert false
  in
  Piqloc.addrefret name res


let expand_name obj c name value =
  if not (String.contains name '.') && not (String.contains name ':')
  then obj
  else
    let rec aux = function
      | [c; n] ->
          make_named_or_typed c name n value
      | c::n::t ->
          let v = aux t in
          make_named_or_typed c name n (Some v)
      | _ -> assert false
    in
    aux (tokenize_name c name)


let expand_obj_names (obj :piq_ast) :piq_ast =
  let exp = expand_name obj in
  match obj with
    | `name x -> exp "." x None
    | `typename x -> exp ":" x None
    | `named x -> exp "." x.Piq_ast.Named.name (Some x.Piq_ast.Named.value)
    | `typed x -> 
        exp ":" x.Piq_ast.Typed.typename (Some x.Piq_ast.Typed.value)
    | x -> x


let expand_obj_names = piq_reference expand_obj_names


(* XXX: don't create new objects if included objects are not modified *)
let expand_names (x: piq_ast) :piq_ast =
  let rec aux0 obj =
    match obj with
      | `named ({Piq_ast.Named.name = n; Piq_ast.Named.value = v} as named) ->
          let v' = aux v in
          if v' != v
          then named.Piq_ast.Named.value <- v';
          expand_obj_names obj
      | `typed ({Piq_ast.Typed.typename = n; Piq_ast.Typed.value = ast} as typed) ->
          let ast' = aux ast in
          if ast' != ast (* changed? *)
          then typed.Piq_ast.Typed.value <- ast';
          expand_obj_names obj
      | `list l ->
          `list (List.map aux l)
      | `form (name, args) ->
          (* at this stage, after we've run expand_forms, this can not be a
           * named or typed form, so leaving name without a transformation *)
          `form (name, List.map aux args)
      | _ ->
          expand_obj_names obj
  and aux obj =
    piq_reference aux0 obj
  in aux x


(* rewrite the ast to expand some of forms (...) such as
     forms affecting parsing associativity, e.g.:
        .foo (.bar)
        .foo (:bar baz)
     abbreviations for repeated fields, e.g.
        (.foo a b c) -> .foo a .foo b .foo c

   NOTE: we perform expansion here rather than during original parsing since we
   want to preserve the original formatting of forms to be able to pretty-print
   without altering symbolic ast representation.
*)

let cons_named n v =
  let res = `named {Piq_ast.Named.name = n; Piq_ast.Named.value = v} in
  piq_addrefret v res


let cons_typed n v =
  let res = `typed {Piq_ast.Typed.typename = n; Piq_ast.Typed.value = v} in
  piq_addrefret v res


let cons_named_or_typed name v =
  match name with
    | `name n ->
        cons_named n v
    | `typename n ->
        cons_typed n v


(* expand named and typed forms *)
let expand_forms (x: piq_ast) :piq_ast =
  let rec aux0 obj =
    match obj with
      | `form (name, args) when args = [] ->
          (* a single name, typename or other ast element enclosed in
           * parenthesis to control associativity -- removing parenthesis *)
          (match name with
            | `word _ ->
                (* we can't remove parenthesis around words, because it can be a
                 * 0-aritity function or macro application *)
                obj
            | _ ->
                aux0 name
          )
      | `form ((`name _) as name, args) | `form ((`typename _) as name, args) when args <> [] ->
          (match args with
            | [v] ->
                (* this is, in fact, typed or named -- it is time to convert
                 * whatever we have to them *)
                cons_named_or_typed name (aux v)
            | _ ->
                error obj "named and typed forms are allowed only inside lists"
          )
      | `named ({Piq_ast.Named.name = n; Piq_ast.Named.value = v} as named) ->
          let v' = aux v in
          if v' != v
          then named.Piq_ast.Named.value <- v';
          (* return the original object taking advantage of object being mutable
           *)
          obj
      | `typed ({Piq_ast.Typed.typename = n; Piq_ast.Typed.value = ast} as typed) ->
          let ast' = aux ast in
          if ast' != ast (* changed? *)
          then typed.Piq_ast.Typed.value <- ast';
          (* return the original object taking advantage of object being mutable
           *)
          obj
      | `list l ->
          if List.exists (function `form (`name _, _) | `form (`typename _, _) -> true | _ -> false) l
          then
            (* expand and splice the results of named and typed form expansion *)
            `list (U.flatmap expand_list_elem l)
          else
            (* process inner elements *)
            `list (List.map aux l)
      | _ ->
          obj
  and expand_list_elem = function
    | `form ((`name _) as name, args) | `form ((`typename _) as name, args) when args <> [] ->
        let expanded_form = List.map (cons_named_or_typed name) args in
        List.map aux expanded_form
    | x ->
        [aux x]
  and aux obj =
    piq_reference aux0 obj
  in aux x


(* expand built-in syntax abbreviations *)
let expand x =
  (* expand (...) when possible *)
  let x = expand_forms x in
  (* expand multi-component names *)
  let x = expand_names x in
  (*
    (* check if expansion produces correct location bindings *)
    let x = expand_forms x in
    let x = expand_names x in
  *)
  x


let make_string loc str_type s raw_s =
  let value = (s, raw_s) in
  let res =
    match str_type with
      | L.String_a -> `ascii_string value
      | L.String_b -> `binary value
      | L.String_u -> `utf8_string value
  in
  Piqloc.addloc loc value;
  Piqloc.addloc loc s;
  Piqloc.addret res


let retry_parse_uint s =
  Piqi_c.piqi_strtoull s


let parse_uint s =
  (* NOTE:
   * OCaml doesn't support large unsingned decimal integer literals. For
   * intance, this call failes with exception (Failure "int_of_string"):
   *
   *      Int64.of_string (Printf.sprintf "%Lu" 0xffff_ffff_ffff_ffffL)
   *
   * However it works with hex representations:
   *
   *      Int64.of_string (Printf.sprintf "%Lu" 0xffff_ffff_ffff_ffffL)
   *
   * We provide custom implementation based on C strtoull() function
   * -- we're using if OCaml's conversion function fails on decimal integer.
   *)
  try Int64.of_string s
  with Failure _ ->
    retry_parse_uint s


let parse_int s =
  try
    match s.[0] with
      | '-' -> (* negative integer *)
          let i = Int64.of_string s in
          let res = (i, s) in
          Piqloc.addref s i;
          Piqloc.addref s res;
          `int res
      | _ ->
          let i = parse_uint s in
          let res = (i, s) in
          Piqloc.addref s i;
          Piqloc.addref s res;
          `uint res
  with Failure _ ->
      failwith ("invalid integer literal: " ^ U.quote s)


let parse_float s =
  (* TODO: be more specific in defining floating point syntax, e.g. disallow
   * omission of trailing '0' after '.' *)
  try
    let f =
      match s with
        | "0.nan" -> Pervasives.nan
        | "0.inf" -> Pervasives.infinity
        | "-0.inf" -> Pervasives.neg_infinity
        | _ -> Pervasives.float_of_string s
    in
    let res = (f, s) in
    Piqloc.addref s res;
    `float res
  with Failure _ ->
    failwith ("invalid floating point literal: " ^ U.quote s)


let parse_number s =
  if String.contains s '.' || String.contains s 'e'
  then parse_float s
  else parse_int s


(*
 * a simple piq parser
 *)

let read_next ?(expand_abbr=true) (fname, lexstream) =
  let location = ref (0,0) in
  let loc () =
    let line, col = !location in
    (fname, line, col)
  in
  let next_token () =
    let tok, loc = Stream.next lexstream in
    location := loc; tok
  in
  let peek_token () =
    match Stream.peek lexstream with
      | None -> assert false
      | Some (tok, loc) ->
          location := loc; tok
  in
  let junk_token () = Stream.junk lexstream in
  let error s = error_at (loc ()) s in

  let rec parse_common ?(chain=true) = function
    | L.Lbr -> parse_list ()
    | L.Rbr -> error "unexpected `]'"
    | L.Lpar -> parse_form ()
    | L.Rpar -> error "unexpected `)'"
    | L.String (t, s, raw_s) ->
        let loc = loc () in
        make_string loc t s raw_s
    | L.Word s when s.[0] = '.' -> (* name part of the named pair *)
        parse_named_or_typed s make_named ~chain
    | L.Word s when s.[0] = ':' -> (* typename part of the typed pair *)
        parse_named_or_typed s make_typed ~chain
    | L.Word s ->
        let word_loc = loc () in
        Piqloc.addloc word_loc s;
        let res = parse_word s in
        Piqloc.addlocret word_loc res
    | L.Raw_binary s ->
        (* Used in pretty-printing mode and in some other cases, similar to
         * String, but we don't parse it -- just pass it through. *)
        Piqloc.addloc (loc ()) s;
        Piqloc.addret (`raw_binary s)
    | L.Text text -> 
        let text_loc = loc () in
        let (fname, line, col) = text_loc in
        let text = parse_text line text in
        (* XXX: make on off by one correction in the line number *)
        let text_loc = (fname, line - 1, col) in
        Piqloc.addloc text_loc text;
        Piqloc.addret (`text text)
    | L.EOF -> error "unexpected end of input"

  (* TODO, XXX: move this functionality to the lexer *)
  (* join adjacent text lines *)
  and parse_text prev_line accu =
    let tok = peek_token () in
    let _,line,_ = loc () in
    match tok with
      | L.Text text when prev_line + 1 = line ->
          (* add next line to the text unless there's a line between them *)
          junk_token (); parse_text line (accu ^ "\n" ^ text)
      | _ -> (* something else -- end of text block *)
          accu

  and parse_form () =
    let startloc = loc () in
    (* parse form name *)
    let t = next_token () in
    let name =
      match t with
        | L.EOF ->
            error "unexpected end of input while reading a form"
        | _ ->
            (* specify that we don't want to chain `typename and `name and turn
             * them into `typed and `named *)
            parse_common t ~chain:false
    in
    (* parse form args *)
    let rec aux accu =
      let t = next_token () in
      match t with
        | L.Rpar -> List.rev accu
        | _ -> aux ((parse_common t)::accu)
    in
    let args = aux [] in
    (* check that this is one of `typename, `name or `word and for example not a
     * number *)
    (match name with
      | #Piq_ast.form_name -> () (* this is valid form *)
      | obj when args <> [] ->
          Piqi_common.error obj
            "invalid form name: only words, names and typenames are allowed"
      | _ ->
          (* this is an ast element in parenthesis -- passing it through; we
           * allow use of parenthesis for explicit control of associativity
           * and to be consisten with "identity" forms like (.foo) *)
          ()
    );
    (* contruct a resulting form from name and args *)
    let pair = (name, args) in
    let res = `form pair in
    Piqloc.addloc startloc pair;
    Piqloc.addret res

  and parse_word s =
    let len = String.length s in
    let parse_number s =
      try parse_number s
      with Failure e ->
        (* allowing a string which prefix looks like a number to be parsed as
         * word in relaxed parsing mode
         *
         * TODO: need a more robust implementation of integer parsing; for
         * instance, the current implementation doesn't distingwish between
         * integer overlows and invalid integer literals *)
        if !Config.piq_relaxed_parsing
        then `word s
        else error e
    in
    match s with
      | "true" -> `bool true
      | "false" -> `bool false
      | _ when s.[0] >= '0' && s.[0] <= '9' -> parse_number s
      | _ when len > 1 && s.[0] = '-' && s.[1] >= '0' && s.[1] <= '9' -> parse_number s
      | _ -> `word s (* just a word *)

  and parse_named_or_typed s make_f ~chain =
    let loc = loc () in
    (* cut the first character which is '.' or ':' *)
    let n = String.sub s 1 (String.length s - 1) in
    Piqloc.addloc loc n;

    let value =
      if chain
      then parse_named_part ()
      else None
    in
    let res = make_f n value in
    (*
    let res = expand_obj_names res in
    *)
    Piqloc.addlocret loc res

  and parse_named_part () =
    let t = peek_token () in
    match t with
      (* name delimiters *)
      | L.Word s when s.[0] = '.' || s.[0] = ':' -> (* other name or type *)
          None
      | L.Rbr | L.Rpar (* closing parenthesis or bracket *)
      | L.EOF -> (* end of input *)
          None
      (* something else *)
      | _ ->
          junk_token (); Some (parse_common t) (* parse named object *)

  and parse_list () =
    let startloc = loc () in
    let rec aux accu =
      let t = next_token () in
      match t with
        | L.Rbr -> 
            let l = List.rev accu in
            let res = `list l in
            Piqloc.addloc startloc l;
            Piqloc.addret res
        | _ -> aux ((parse_common t)::accu)
    in aux []
  in
  let parse_top () =
    let t = next_token () in
    match t with
      | L.EOF -> None
      | _ ->
          let ast = parse_common t in
          let res =
            if expand_abbr
            then expand ast (* expand built-in syntax abbreviations *)
            else ast (* return as it is *)
          in Some res
  in
  try parse_top ()
  with L.Error (s, (line, col)) ->
    (* convert lexer's errors *)
    error_at (fname, line, col) s


let read_all ?(expand_abbr=true) piq_parser =
  let rec aux accu =
    match read_next piq_parser ~expand_abbr with
      | None -> List.rev accu
      | Some x -> aux (x::accu)
  in aux []


let make_lexstream lexbuf =
  let f _counter =
    let tok = L.token lexbuf in
    let loc = L.location lexbuf in
    Some (tok, loc)
  in
  Stream.from f


let init_from_channel fname ch =
  let lexbuf = L.init_from_channel ch in
  (fname, make_lexstream lexbuf)


let init_from_string fname s =
  let lexbuf = L.init_from_string s in
  (fname, make_lexstream lexbuf)


let init_from_token_list fname l =
  (fname, Stream.of_list l)

