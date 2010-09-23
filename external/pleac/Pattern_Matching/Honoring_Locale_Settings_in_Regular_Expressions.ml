(* ********************************************************************** *)
(* Honoring Locale Settings in Regular Expressions *)
(* ********************************************************************** *)
let pleac_Honoring_Locale_Settings_in_Regular_Expressions () = 
  (* OCaml does not provide a way to change the locale, and PCRE does
     not appear to be sensitive to the default locale. Regardless, Str
     does not support locales, and PCRE only matches ASCII characters
     for \w and friends. This example instead demonstrates the use of
     PCRE's UTF-8 support to match words, and it does not use locales. *)
  
  #directory "+pcre";;
  #load "pcre.cma";;
  
  (* encoded as UTF-8 *)
  let name = "andreas k\xc3\xb6nig"
  
  (* the original regexp which is not Unicode-aware *)
  let ascii_regexp = Pcre.regexp "\\b(\\w+)\\b"
  
  (* a revised regexp which tests for Unicode letters and numbers *)
  let utf8_regexp = Pcre.regexp ~flags:[`UTF8] "([\\pL\\pN]+)"
  
  let () =
    List.iter
      (fun (enc, regexp) ->
         Printf.printf "%s names: %s\n" enc
           (String.concat " "
              (List.map
                 String.capitalize
                 (List.flatten
                    (Array.to_list
                       (Array.map
                          Array.to_list
                          (Pcre.extract_all
                             ~full_match:false
                             ~rex:regexp
                             name)))))))
      ["ASCII", ascii_regexp; "UTF-8", utf8_regexp]
  
  (*
    ASCII names: Andreas K Nig
    UTF-8 names: Andreas König
  *)
  

