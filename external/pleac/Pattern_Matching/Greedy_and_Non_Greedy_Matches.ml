(* ********************************************************************** *)
(* Greedy and Non-Greedy Matches *)
(* ********************************************************************** *)
let pleac_Greedy_and_Non_Greedy_Matches () = 
  let s = "Even <TT>vi</TT> can edit <TT>troff</TT> effectively."
  
  (* The Str library does not support non-greedy matches. In many cases,
     you can turn a non-greedy match into a greedy one, however: *)
  
  #load "str.cma";;
  
  let () = print_endline (Str.global_replace (Str.regexp "<.*>") "" s)
  (* Even  effectively. *)
  let () = print_endline (Str.global_replace (Str.regexp "<[^>]*>") "" s)
  (* Even vi can edit troff effectively. *)
  
  (* If you need non-greedy matches, you'll want to use PCRE instead: *)
  
  #directory "+pcre";;
  #load "pcre.cma";;
  
  let () = print_endline (Pcre.replace ~pat:"<.*?>" ~templ:"" s)
  (* Even vi can edit troff effectively. *)
  
  (* Non-greedy matches don't always work the way you expect: *)
  
  let s = "<b><i>this</i> and <i>that</i> are important</b> Oh, <b><i>me too!</i></b>"
  
  let rex = Pcre.regexp "<b><i>(.*?)</i></b>"
  let () = print_endline (Pcre.extract ~rex s).(1)
  (* this</i> and <i>that</i> are important</b> Oh, <b><i>me too! *)
  
  (* One solution is to use a non-grouping negative lookahead assertion: *)
  
  let rex = Pcre.regexp "<b><i>((?:(?!</b>|</i>).)*)</i></b>"
  let () = print_endline (Pcre.extract ~rex s).(1)
  (* me too! *)
  
  (* If performance is important, here is a faster technique: *)
  
  let rex = Pcre.regexp ~flags:[`DOTALL; `EXTENDED] "
      <b><i>
      [^<]*  # stuff not possibly bad, and not possibly the end.
      (?:
   # at this point, we can have '<' if not part of something bad
       (?!  </?[ib]>  )   # what we can't have
       <                  # okay, so match the '<'
       [^<]*              # and continue with more safe stuff
      ) *
      </i></b>
  "
  
  let () = print_endline (Pcre.extract ~rex s).(0)
  (* <b><i>me too!</i></b> *)
  

