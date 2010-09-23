(* ********************************************************************** *)
(* Program: urlify *)
(* ********************************************************************** *)
let pleac_Program__urlify () = 
  #!/usr/bin/ocaml
  (* urlify - wrap HTML links around URL-like constructs *)
  #directory "+pcre";;
  #load "pcre.cma";;
  
  let urls = "(http|telnet|gopher|file|wais|ftp)"
  let ltrs = "\\w"
  let gunk = "/#~:.?+=&%@!\\-"
  let punc = ".:?\\-"
  let any  = ltrs ^ gunk ^ punc
  
  let rex = Pcre.regexp ~flags:[`CASELESS; `EXTENDED]
    (Printf.sprintf "
        \\b                   # start at word boundary
        (                     # begin $1  {
         %s        :          # need resource and a colon
         [%s] +?              # followed by one or more
                              #  of any valid character, but
                              #  be conservative and take only
                              #  what you need to....
        )                     # end   $1  }
        (?=                   # look-ahead non-consumptive assertion
         [%s]*                # either 0 or more punctuation
         [^%s]                #   followed by a non-url char
         |                    # or else
         $                    #   then end of the string
        )
  " urls any punc any)
  
  let templ = "<A HREF=\"$1\">$1</A>"
  
  let () =
    try
      while true do
        let line = input_line stdin in
        print_endline (Pcre.replace ~rex ~templ line)
      done
    with End_of_file ->
      ()
  

