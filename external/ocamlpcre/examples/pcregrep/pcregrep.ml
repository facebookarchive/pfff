open Pcre
open Printf

let filenames      = ref true
and filenames_only = ref false
and count_only     = ref false
and invert         = ref false
and number         = ref false
and silent         = ref false
and whole_lines    = ref false

let parse_args () =
  let ignore_case = ref false

  and pat    = ref None
  and files  = ref [] in

  let c = "-c", Arg.Set count_only, "Count lines only."
  and h = "-h", Arg.Clear filenames,
          "Suppress printing of filenames when searching multiple files."
  and i = "-i", Arg.Set ignore_case, "Ignore case."
  and l = "-l", Arg.Set filenames_only,
          "Only print names of files containing matching lines (once)."
  and n = "-n", Arg.Set number,
          "Precede each line by its line number in the file."
  and s = "-s", Arg.Set silent,
          "Display nothing but error messages. Exit status indicates match."
  and v = "-v", Arg.Set invert,
          "Invert sense of the match: finds nonmatching lines."
  and x = "-x", Arg.Set whole_lines,
          "Force the pattern to be anchored and to match the entire line."
  and usage =
    "Usage: pcregrep [options] pattern [file] ...\n\n\
     Searches files for character patterns.\n"
  and anon_arg arg =
    if !pat = None then pat := Some arg
    else files := arg :: !files in

  let args = [c; h; i; l; n; s; v; x] in
  Arg.parse args anon_arg usage;

  let flags =
    let flag_list = if !ignore_case then [`CASELESS] else [] in
    if !whole_lines then `ANCHORED :: flag_list else flag_list in

  let rex = 
    match !pat with
    | Some pat -> regexp ~flags pat
    | None -> eprintf "%s: not enough arguments!\n" Sys.argv.(0);
              Arg.usage args usage; exit 2 in
  rex, List.rev !files

let _ =
  let rex, files = parse_args ()
  and rfl = rflags [] in

  let subgroups2, ovector = make_ovector rex in

  let pcregrep file name =
    let ret_code = ref 1
    and linenumber = ref 0
    and count = ref 0

    and stdin_print_name () =
      match name with
      | Some filename -> print_endline filename
      | None -> print_endline "<stdin>"

    and print_name () =
      match name with Some name -> printf "%s:" name | None -> () in

    let try_match line =
      let matched =
        try
          unsafe_pcre_exec rfl rex 0 line subgroups2 ovector None;
          if !whole_lines && ovector.(1) <> String.length line then false
          else true
        with Not_found -> false in

      incr linenumber;

      if matched <> !invert then begin
        if !count_only then incr count
        else if !filenames_only then begin stdin_print_name (); raise Exit end
        else if !silent then raise Exit
        else begin
          print_name ();
          if !number then printf "%d:" !linenumber;
          print_endline line
        end;
        ret_code := 0 end in

    try
      foreach_line ~ic:file try_match;
      if !count_only then begin
        print_name ();
        printf "%d\n" !count end;
      !ret_code
    with Exit -> 0 in

  if files = [] then exit (pcregrep stdin None);

  if List.length files = 1 then filenames := false;
  if !filenames_only then filenames := true;

  let collect ret_code filename =
    try
      let file = open_in filename in
      let frc = pcregrep file (if !filenames then Some filename else None) in
      close_in file;
      if frc = 0 && ret_code = 1 then 0 else ret_code
    with Sys_error msg -> prerr_endline msg; 2 in
  exit (List.fold_left collect 1 files)
