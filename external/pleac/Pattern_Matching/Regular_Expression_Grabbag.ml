(* ********************************************************************** *)
(* Regular Expression Grabbag *)
(* ********************************************************************** *)
let pleac_Regular_Expression_Grabbag () = 
  #directory "+pcre";;
  #load "pcre.cma";;
  
  (*-----------------------------*)
  
  Pcre.pmatch
    ~rex:(Pcre.regexp
            ~flags:[`CASELESS]
            "^m*(d?c{0,3}|c[dm])(l?x{0,3}|x[lc])(v?i{0,3}|i[vx])$")
    input
  
  (*-----------------------------*)
  
  Pcre.replace
    ~pat:"(\\S+)(\\s+)(\\S+)"
    ~templ:"$3$2$1"
    input
  
  (*-----------------------------*)
  
  Pcre.extract
    ~full_match:false
    ~pat:"(\\w+)\\s*=\\s*(.*)\\s*$"
    input
  
  (*-----------------------------*)
  
  Pcre.pmatch
    ~pat:".{80,}"
    input
  
  (*-----------------------------*)
  
  Pcre.extract
    ~full_match:false
    ~pat:"(\\d+)/(\\d+)/(\\d+) (\\d+):(\\d+):(\\d+)"
    input
  
  (*-----------------------------*)
  
  Pcre.replace
    ~pat:"/usr/bin"
    ~templ:"/usr/local/bin"
    input
  
  (*-----------------------------*)
  
  Pcre.substitute_substrings
    ~pat:"%([0-9A-Fa-f][0-9A-Fa-f])"
    ~subst:(fun subs ->
              let c = Pcre.get_substring subs 1 in
              String.make 1 (Char.chr (int_of_string ("0x" ^ c))))
    input
  
  (*-----------------------------*)
  
  Pcre.replace
    ~rex:(Pcre.regexp
            ~flags:[`DOTALL; `EXTENDED] "
              /\\*  # Match the opening delimiter
              .*?   # Match a minimal number of characters
              \\*/  # Match the closing delimiter
            ")
    input
  
  (*-----------------------------*)
  
  Pcre.replace ~pat:"^\\s+" input
  Pcre.replace ~pat:"\\s+$" input
  
  (*-----------------------------*)
  
  Pcre.replace ~pat:"\\\\n" ~templ:"\n" input
  
  (*-----------------------------*)
  
  Pcre.replace ~pat:"^.*::" input
  
  (*-----------------------------*)
  
  Pcre.extract
    ~full_match:false
    ~pat:"^([01]?\\d\\d|2[0-4]\\d|25[0-5])\\.([01]?\\d\\d|2[0-4]\\d|25[0-5])\\.\
           ([01]?\\d\\d|2[0-4]\\d|25[0-5])\\.([01]?\\d\\d|2[0-4]\\d|25[0-5])$"
    input
  
  (*-----------------------------*)
  
  Pcre.replace ~pat:"^.*/" input
  
  (*-----------------------------*)
  
  let termcap = ":co#80:li#24:"
  let cols =
    try int_of_string (Pcre.extract ~pat:":co#(\\d+):" termcap).(1)
    with Not_found | Failure "int_of_string" -> 80
  
  (*-----------------------------*)
  
  let name =
    Pcre.replace
      ~pat:" /\\S+/"
      ~templ:" "
      (" " ^ String.concat " " (Array.to_list Sys.argv))
  
  (*-----------------------------*)
  
  #load "unix.cma";;
  let () =
    let ch = Unix.open_process_in "uname -a" in
    let os = input_line ch in
    ignore (Unix.close_process_in ch);
    if not (Pcre.pmatch ~rex:(Pcre.regexp ~flags:[`CASELESS] "linux") os)
    then print_endline "This isn't Linux"
  
  (*-----------------------------*)
  
  Pcre.replace ~pat:"\n\\s+" ~templ:" " input
  
  (*-----------------------------*)
  
  let nums =
    Array.map
      (fun group -> float_of_string group.(1))
      (Pcre.extract_all
         ~pat:"(\\d+\\.?\\d*|\\.\\d+)"
         input)
  
  (*-----------------------------*)
  
  let capwords =
    Array.map
      (fun group -> group.(1))
      (Pcre.extract_all
         ~pat:"(\\b[^\\Wa-z0-9_]+\\b)"
         input)
  
  (*-----------------------------*)
  
  let lowords =
    Array.map
      (fun group -> group.(1))
      (Pcre.extract_all
         ~pat:"(\\b[^\\WA-Z0-9_]+\\b)"
         input)
  
  (*-----------------------------*)
  
  let icwords =
    Array.map
      (fun group -> group.(1))
      (Pcre.extract_all
         ~pat:"(\\b[^\\Wa-z0-9_][^\\WA-Z0-9_]*\\b)"
         input)
  
  (*-----------------------------*)
  
  let links =
    Array.map
      (fun group -> group.(1))
      (Pcre.extract_all
         ~rex:(Pcre.regexp
                 ~flags:[`DOTALL; `CASELESS]
                 "<A[^>]+?HREF\\s*=\\s*[\"']?([^'\" >]+?)[ '\"]?>")
         input)
  
  (*-----------------------------*)
  
  let initial =
    try (Pcre.extract ~pat:"^\\S+\\s+(\\S)\\S*\\s+\\S" input).(1)
    with Not_found -> ""
  
  (*-----------------------------*)
  
  Pcre.replace ~pat:"\"([^\"]*)\"" ~templ:"``$1''" input
  
  (*-----------------------------*)
  
  let sentences =
    Array.map
      (fun group -> group.(1))
      (Pcre.extract_all
         ~pat:"(\\S.*?\\pP)(?=  |\\Z)"
         (Pcre.replace ~pat:" {3,}" ~templ:"  "
            (Pcre.replace ~pat:"\n" ~templ:" "
               (Pcre.replace ~pat:"(\\pP\n)" ~templ:"$1  "
                  input))))
  
  (*-----------------------------*)
  
  Pcre.extract ~full_match:false ~pat:"(\\d{4})-(\\d\\d)-(\\d\\d)" input
  
  (*-----------------------------*)
  
  Pcre.pmatch
    ~pat:"^[01]?[- .]?(\\([2-9]\\d{2}\\)|[2-9]\\d{2})[- .]?\\d{3}[- .]?\\d{4}$"
    input
  
  (*-----------------------------*)
  
  Pcre.pmatch
    ~rex:(Pcre.regexp
            ~flags:[`CASELESS]
            "\\boh\\s+my\\s+gh?o(d(dess(es)?|s?)|odness|sh)\\b")
    input
  
  (*-----------------------------*)
  
  let lines =
    Array.map
      (fun group -> group.(1))
      (Pcre.extract_all
         ~pat:"([^\010\013]*)(\010\013?|\013\010?)"
         input)
  
  

