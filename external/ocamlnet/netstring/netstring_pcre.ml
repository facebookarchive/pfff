(* $Id: netstring_pcre.ml 1003 2006-09-24 15:17:15Z gerd $
 * ----------------------------------------------------------------------
 *
 *)


type regexp = Pcre.regexp;;
type split_result = Pcre.split_result =
  | Text of string
  | Delim of string
  | Group of int * string
  | NoGroup
;;

type result = Pcre.substrings;;

let regexp s =
  Pcre.regexp ~flags:[`MULTILINE] s
;;

let regexp_case_fold s =
  Pcre.regexp ~flags:[`MULTILINE; `CASELESS] s
;;

let quote s =
  Pcre.quote s
;;

let regexp_string s =
  regexp (quote s)
;;

let regexp_string_case_fold s =
  regexp_case_fold (quote s)
;;

let string_match ?groups pat s pos =
  try
    let result = Pcre.exec ~rex:pat ~flags:[`ANCHORED] ~pos s in
    Some result
  with Not_found -> None
;;

let search_forward ?groups pat s pos =
  let result = Pcre.exec ~rex:pat ~pos s in
  fst (Pcre.get_substring_ofs result 0), result
;;

let search_backward ?groups pat s pos =
  let rec search p =
    try
      (* `ANCHORED: virtually prepends "^" to the regexp *)
      let result = Pcre.exec ~flags:[`ANCHORED] ~rex:pat ~pos:p s in
      fst (Pcre.get_substring_ofs result 0), result
    with
      Not_found ->
        if p > 0 then search (p-1) else raise Not_found
  in
  search pos
;;

let matched_string result _ =
  (* Unfortunately, Pcre.get_substring will not raise Not_found if there is
   * no matched string. Instead, it returns "", but this value cannot be
   * distinguished from an empty match.
   * The workaround is to call Pcre.get_substring_ofs first. This function
   * will raise Not_found if there is not any matched string.
   *
   * NOTE: Current versions of Pcre do return Not_found!
   *)
  ignore(Pcre.get_substring_ofs result 0);
  Pcre.get_substring result 0
;;

let match_beginning result =
  fst (Pcre.get_substring_ofs result 0)
;;

let match_end result =
  snd (Pcre.get_substring_ofs result 0)
;;

let matched_group result n _ =
  (* See also the comment for [matched_string] *)
  if n < 0 || n >= Pcre.num_of_subs result then raise Not_found;
  ignore(Pcre.get_substring_ofs result n);
  Pcre.get_substring result n
;;

let group_beginning result n =
  fst (Pcre.get_substring_ofs result n)
;;

let group_end result n =
  snd (Pcre.get_substring_ofs result n)
;;

let global_replace pat templ s =
  Pcre.replace ~rex:pat ~itempl:(Pcre.subst templ) s
;;

let replace_first pat templ s =
  Pcre.replace_first ~rex:pat ~itempl:(Pcre.subst templ) s
;;

let global_substitute ?groups pat subst s =
  Pcre.substitute_substrings ~rex:pat ~subst:(fun r -> subst r s) s
;;

let string_before s n =
  String.sub s 0 n
;;

let string_after s n =
  String.sub s n (String.length s - n)
;;

let first_chars s len =
  String.sub s 0 len
;;

let last_chars s len =
  String.sub s (String.length s - len) len
;;

(*
 * Uncomment for next version of Pcre
let substitute_first ?groups ~pat ~subst s =
  Pcre.substitute_substrings_first ~rex:pat ~subst:(fun r -> subst r s) s
;;
*)

let substitute_first ?groups pat subst s =
  (* Do it yourself in the meantime *)
  try
    let substrs = Pcre.exec ~rex:pat s in (* or Not_found *)
    let (match_beg,match_end) = Pcre.get_substring_ofs substrs 0 in
    let replacement_text = subst substrs s in
    String.concat "" [string_before s match_beg;
		      replacement_text;
		      string_after s match_end]
  with
      Not_found -> s
;;
  


(* Copied from Str for exact compatibility: *)
let bounded_split expr text num =
  let start =
    try
      let start_substrs = Pcre.exec ~rex:expr ~flags:[`ANCHORED] text in
                          (* or Not_found *)
      let (_,match_end) = Pcre.get_substring_ofs start_substrs 0 in
      match_end
    with
	Not_found -> 0
  in
  let rec split start n =
    if start >= String.length text then [] else
    if n = 1 then [string_after text start] else
      try
	let next_substrs = Pcre.exec ~rex:expr ~pos:start text 
	in (* or Not_found *)
	let (pos,match_end) = Pcre.get_substring_ofs next_substrs 0 in
        String.sub text start (pos-start) :: split match_end (n-1)
      with Not_found ->
        [string_after text start] in
  split start num
;;

let split sep s = bounded_split sep s 0
;;

(* Copied from Str for exact compatibility: *)
let bounded_split_delim expr text num =
  let rec split start n =
    if start > String.length text then [] else
    if n = 1 then [string_after text start] else
      try
	let next_substrs = Pcre.exec ~rex:expr ~pos:start text 
	in (* or Not_found *)
	let (pos,match_end) = Pcre.get_substring_ofs next_substrs 0 in
        String.sub text start (pos-start) :: split match_end (n-1)
      with Not_found ->
        [string_after text start] in
  if text = "" then [] else split 0 num
;;

let split_delim sep text = bounded_split_delim sep text 0 ;;

let full_split sep s =
  Pcre.full_split ~rex:sep ~max:(-1) s
;;

let bounded_full_split sep s max =
  let max' = if max <= 0 then -1 else max in
  Pcre.full_split ~rex:sep ~max:max' s
;;
