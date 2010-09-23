(* $Id: check_externals.ml4 940 2003-06-11 10:02:30Z garrigue $ *)

(* Check that all external statements differ in a .ml or .mli file *)

open StdLabels

(*** Objective Caml simplified lexer ***)

type token =
    Ident of string
  | Num of int
  | Sym of char
  | String of string
  | Char of string
  | EOF

let rec implode l =
  let s = String.create (List.length l) in
  let i = ref 0 in
  List.iter l ~f:(fun c -> s.[!i] <- c; incr i);
  s

let rec skip tok = parser [< ' tok' ; s >] -> if tok <> tok' then skip tok s

let rec star ~acc p = parser
    [< x = p ; s >] -> star ~acc:(x::acc) p s
  | [< >] -> List.rev acc

let alphanum = parser [< ' ('A'..'Z'|'a'..'z'|'0'..'9'|'\''|'_' as c) >] -> c

let num = parser [< ' ('0'..'9'|'_' as c) >] -> c

let escaped = parser
    [< ' ('0'..'9' as c1); ' ('0'..'9' as c2); ' ('0'..'9' as c3) >] ->
      [c1;c2;c3]
  | [< ' c >] ->
      [c]

let char = parser
    [< ''\\'; l = escaped; ''\'' >] -> implode ('\\'::l)
  | [< ' c ; ''\'' >] -> String.make 1 c

let rec string ~acc = parser
    [< ''"' >] -> implode (List.rev acc)
  | [< ''\''; l = escaped; s >] ->
      string ~acc:(List.rev_append l ('\''::acc)) s
  | [< ' c ; s >] ->
      string ~acc:(c::acc) s

let rec token = parser
    [< ' ('A'..'Z'|'a'..'z'|'_' as c); l = star alphanum ~acc:[c] >] ->
      Ident (implode l)
  | [< ' ('0'..'9' as c); l = star ~acc:[c] num >] ->
      Num (int_of_string (implode l))
  | [< ''('; r = may_comment >] ->
      r
  | [< ''\''; s >] ->
      (try Char (char s) with _ -> token s) (* skip type variables... *)
  | [< ''"'; s = string ~acc:[] >] ->
      String s
  | [< ' (' '|'\n'|'\r'|'\t'); s >] ->
      token s
  | [< ' c >] ->
      Sym c
  | [< >] ->
      raise End_of_file

and may_comment = parser
    [< ''*'; s >] ->
      let s' = lexer s in skip (Sym '*') s'; may_close_comment s'
  | [< >] -> Sym '('

and may_close_comment = parser
    [< ' Sym ')'; ' tok >] -> tok
  | [< s >] -> skip (Sym '*') s; may_close_comment s

and lexer s = [< ' token s ; lexer s >]



(**** The actual checker ***)

let defs = Hashtbl.create 13

let add impl name =
  try
    let name' = Hashtbl.find defs impl in
    Printf.eprintf "externals [%s] and [%s] have same implementation \"%s\"\n"
      name' name impl
  with Not_found ->
    Hashtbl.add defs impl name

let may_string = parser
    [< ' String s >] -> s
  | [< >] -> ""

let rec skip_type = parser
    [< ' Sym '=' >] -> ()
  | [< ' Sym '('; _ = skip (Sym ')'); s >] -> skip_type s
  | [< ' Sym '['; _ = skip (Sym ']'); s >] -> skip_type s
  | [< ' _; s >] -> skip_type s

let check_external = parser
    [< ' Ident name; ' Sym ':'; _ = skip_type;
       ' String impl; native1 = may_string; native2 = may_string >] ->
       if impl <> "" && impl.[0] <> '%' then add impl name;
       let native =
         match native1, native2 with
           ("noalloc"|"float"), ("noalloc"|"float") -> ""
         | ("noalloc"|"float"), n -> n
         | n, _ -> n
       in
       if native <> "" then add native name

let check f =
  prerr_endline ("processing " ^ f);
  let ic = open_in f in
  let chars = Stream.of_channel ic in
  let s = lexer chars in
  try while true do skip (Ident"external") s; check_external s done with
    End_of_file -> ()
  | Stream.Error _ | Stream.Failure ->
      Printf.eprintf "Parse error in file `%s' before char %d\n"
        f (Stream.count chars);
      exit 2
  | exn ->
      Printf.eprintf "Exception %s in file `%s' before char %d\n"
        (Printexc.to_string exn) f (Stream.count chars);
      exit 2

let main () =
  Arg.parse [] check "usage: check_externals file.ml ..."

let () = Printexc.print main ()

