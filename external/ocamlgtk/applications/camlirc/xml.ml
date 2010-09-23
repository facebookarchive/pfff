(**************************************************************************)
(*     Lablgtk - Camlirc                                                  *)
(*                                                                        *)
(*    * You are free to do anything you want with this code as long       *)
(*      as it is for personal use.                                        *)
(*                                                                        *)
(*    * Redistribution can only be "as is".  Binary distribution          *)
(*      and bug fixes are allowed, but you cannot extensively             *)
(*      modify the code without asking the authors.                       *)
(*                                                                        *)
(*    The authors may choose to remove any of the above                   *)
(*    restrictions on a per request basis.                                *)
(*                                                                        *)
(*    Authors:                                                            *)
(*      Nobuaki Yoshida  <nyoshi@dd.iij4u.or.jp>                          *)
(*      Jacques Garrigue <garrigue@kurims.kyoto-u.ac.jp>                  *)
(*                                                                        *)
(**************************************************************************)

(* $Id: xml.ml 1354 2007-07-20 04:18:38Z garrigue $ *)

open Xml_lexer

type element = { elt_desc: element_desc; elt_start: int; elt_end: int}
and element_desc =
  | Node of string * (string * string) list * element list
  | Text of string

type presence = [`Required | `Optional]
type dtd =
    { tags: (string * ((string * presence) list * dtd)) list;
      allow: [`Mixed | `Tags | `Any] }

let any = {tags = []; allow = `Any}
let text = {tags = []; allow = `Mixed}

let check_tag ~name ~attrs ~dtd =
  try
    let attr_dtd, child_dtd = List.assoc name dtd.tags in
    List.iter (fun (key,_) -> ignore (List.assoc key attr_dtd)) attrs;
    List.iter 
      (function (key,`Required) -> ignore (List.assoc key attrs) | _ -> ())
      attr_dtd ;
    child_dtd
  with Not_found ->
    if dtd.allow = `Any then any else
    raise (Error(Other"input does not conform to DTD", token_start ()))

let check_text ~dtd =
  if dtd.allow = `Tags then
    raise (Error(Other"input does not conform to DTD", token_start ()))

let parse ?doctype ?(dtd=any) lexbuf =
  begin match doctype with None -> ()
  | Some doctype -> match token lexbuf with
    | Tag ("!doctype", attrs, _) ->
        if not (List.mem_assoc (String.lowercase doctype) attrs) then
          raise (Error(Other"Document type differs", token_start ()))
    | _ ->
        raise (Error(Other"Document type missing", token_start ()))
  end;
  let mkelt d =
    { elt_desc = d;
      elt_start = token_start ();
      elt_end = Lexing.lexeme_end lexbuf } in
  let rec parse ~prev ~dtd =
    match token lexbuf with
    | Tag (name, attrs, closed) ->
        let closed = closed || name.[0] = '!' in
        let child_dtd = check_tag ~name ~attrs:attrs ~dtd in
        if closed then
          parse ~prev:(mkelt(Node (name, attrs, [])) :: prev) ~dtd
        else begin
          let nodes, closing = parse ~prev:[] ~dtd:child_dtd in
          let prev = mkelt(Node (name, attrs, List.rev nodes)) :: prev in
          if closing = Some name then
            parse ~prev ~dtd
          else
            prev, closing
        end
    | Chars s ->
        check_text ~dtd;
        parse ~prev:(mkelt(Text s) :: prev) ~dtd
    | Endtag name ->
        prev, Some name
    | EOF ->
        prev, None
  in parse ~prev:[] ~dtd

let parse_lexbuf ?doctype ?dtd lexbuf =
  List.rev (fst (parse lexbuf ?doctype ?dtd))

let parse_string ?doctype ?dtd s =
  parse_lexbuf (Lexing.from_string s) ?doctype ?dtd

type 'a result = Ok of 'a | Exn of exn
let protect f x = try Ok (f x) with exn -> Exn exn
let return = function Ok x -> x | Exn exn -> raise exn

let parse_file ?doctype ?dtd name =
  let ic = open_in name in
  let res = protect (parse_lexbuf ?doctype ?dtd) (Lexing.from_channel ic) in
  close_in ic;
  return res

(*
class reader lexbuf ~name ~attrs ~closed =
  object (self)
    val mutable closed = closed
    val mutable closing = None
    val mutable current = None
    val start = token_start ()
    method name = name
    method attrs = attrs
    method get_attr key : string = List.assoc (String.lowercase key) attrs
    method has_attr key = List.mem_assoc (String.lowercase key) attrs

    method finish =
      while not closed do ignore (self#next_child) done;
      closing
      
    method next_child : [`NODE of string * reader | `TEXT of string | `NONE] =
      begin match current with None -> ()
      | Some node ->
          current <- None;
          match node#finish with None -> ()
          | Some name' ->
              if name <> name' then closing <- Some name';
              closed <- true
      end;
      if closed then `NONE else begin
        match token lexbuf with
        | Tag (name, attrs, closed) ->
            let attrs =
              List.map attrs ~f:(fun (k,v) -> String.lowercase k,v) in
            let closed = closed || name.[0] = '!' in
            let node = new reader lexbuf ~name ~attrs ~closed in
            current <- Some node;
            `NODE (name, node)
        | Chars s ->
            `TEXT s
        | Endtag name' ->
            if name' <> name then closing <- Some name';
            `NONE
        | EOF ->
            closing <- Some "";
            `NONE
      end

    method iter (f : [`NODE of string * reader | `TEXT of string] -> unit) =
      while match self#next_child with
        `NODE _ | `TEXT _ as x -> f x; true
      | `NONE -> false
      do () done
  end

let reader ic =
  new reader (Lexing.from_channel ic) ~name:"" ~attrs:[] ~closed:false

let string_reader s =
  new reader (Lexing.from_string s) ~name:"" ~attrs:[] ~closed:false
*)
