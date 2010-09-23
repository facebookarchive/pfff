open Pcre

let parse_args () =
  let quick       = ref false
  and first       = ref false
  and ignore_case = ref false
  and offset      = ref 0

  and pat    = ref None
  and substr = ref None in

  let q   = "-q", Arg.Set quick,
            "Quick replacement. Interpretes substitution as plain text."
  and f   = "-f", Arg.Set first, "Replace first occurrence in line only."
  and i   = "-i", Arg.Set ignore_case, "Ignore case."
  and ofs = "-ofs", Arg.Int (fun n -> offset := n),
            "Start matching at column n."

  and usage =
    "Usage: subst [-q] [-f] [-i] [-ofs offset] pattern substitution\n\n\
     Reads lines from standard input and replaces occurrences of\n\
     the PERL-style regular expression \"pattern\" with \"substitution\",\n\
     printing the result to standard output.\n\
     In default mode the contents of \"substitution\" will be interpreted\n\
     similarly to its equivalent in PERL.\n"

  and anon_arg arg =
    match !pat, !substr with
    | None, _ -> pat := Some arg
    | _, None -> substr := Some arg
    | _       -> raise (Arg.Bad "too many arguments!") in

  let args = [q; f; i; ofs] in
  Arg.parse args anon_arg usage;

  let flags = if !ignore_case then [`CASELESS] else [] in

  let rex, sstr =
    match !pat, !substr with
      | Some rex, Some sstr -> regexp ~flags rex, sstr
      | _ -> prerr_endline (Sys.argv.(0) ^ ": not enough arguments!");
             Arg.usage args usage; exit 1 in

  match !quick, !first with
  | false, false -> fun s -> replace ~rex ~pos:!offset ~templ:sstr s
  | true, false  -> fun s -> qreplace ~rex ~pos:!offset ~templ:sstr s
  | false, true  -> fun s -> replace_first ~rex ~pos:!offset ~templ:sstr s
  | true, true   -> fun s -> qreplace_first ~rex ~pos:!offset ~templ:sstr s

let _ =
  let substitute = parse_args () in
  foreach_line (fun line ->
    try print_endline (substitute line)
    with Invalid_argument _ -> print_endline line)
