(* ********************************************************************** *)
(*  Commenting Regular Expressions *)
(* ********************************************************************** *)
let pleac__Commenting_Regular_Expressions () = 
  #!/usr/bin/ocaml
  (* resname - change all "foo.bar.com" style names in the input stream
     into "foo.bar.com [204.148.40.9]" (or whatever) instead *)
  
  #directory "+pcre";;
  #load "pcre.cma";;
  #load "unix.cma";;
  
  let regexp =
    Pcre.regexp ~flags:[`EXTENDED] "
      (                     # capture the hostname in substring 1
        (?:                 # these parens for grouping only
          (?! [-_]  )       # lookahead for neither underscore nor dash
          [\\w-] +          # hostname component
          \\.               # and the domain dot
        ) +                 # now repeat that whole thing a bunch of times
        [A-Za-z]            # next must be a letter
        [\\w-] +            # now trailing domain part
      )                     # end of substring 1 capture
    "
  
  let process line =
    print_endline
      (Pcre.substitute_substrings
         ~rex:regexp
         ~subst:(fun subs ->
                   let name = Pcre.get_substring subs 1 in
                   let addr =
                     try
                       Unix.string_of_inet_addr
                         (Unix.gethostbyname name).Unix.h_addr_list.(0)
                     with Not_found -> "???" in
                   name ^ " [" ^ addr ^ "]")
         line)
  
  let () =
    try
      while true do
        let line = read_line () in
        process line
      done
    with End_of_file -> ()
  
  (*-----------------------------*)
  
  let vars = Hashtbl.create 0
  let () =
    Hashtbl.replace vars "name" "Bob";
    Hashtbl.replace vars "flavor" "rhubarb"
  
  let () =
    print_endline
      (Pcre.substitute_substrings
         ~rex:(Pcre.regexp ~flags:[`EXTENDED] "
                 \\#                #   a pound sign
                 (\\w+)             #   the variable name
                 \\#                #   another pound sign
               ")
         ~subst:(fun subs -> Hashtbl.find vars (Pcre.get_substring subs 1))
         "Hello, #name#, would you like some #flavor# pie?")
  

