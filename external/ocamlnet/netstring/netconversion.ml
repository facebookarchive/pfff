(* $Id: netconversion.ml 1509 2010-12-17 02:01:12Z gerd $
 * ----------------------------------------------------------------------
 *)

open Netaux.ArrayAux

exception Malformed_code

exception Cannot_represent of int

let multibyte_limit = (* 6 *) 50;;
  (* The longest multibyte character of all supported encodings,
   * and the longest substitution string.
   *)

let big_slice = (* 3 *) 250;;
  (* The typical length of slices *)


(* Seems to be a good source: ftp://dkuug.dk/i18n/charmaps
 *)

type encoding =
  [  `Enc_utf8       (* UTF-8 *)
  |  `Enc_java
  |  `Enc_utf16      (* UTF-16 with unspecified endianess (restricted usage) *)
  |  `Enc_utf16_le   (* UTF-16 little endian *)
  |  `Enc_utf16_be   (* UTF-16 big endian *)
  |  `Enc_usascii    (* US-ASCII (only 7 bit) *)
  |  `Enc_iso88591   (* ISO-8859-1 *)
  |  `Enc_iso88592   (* ISO-8859-2 *)
  |  `Enc_iso88593   (* ISO-8859-3 *)
  |  `Enc_iso88594   (* ISO-8859-4 *)
  |  `Enc_iso88595   (* ISO-8859-5 *)
  |  `Enc_iso88596   (* ISO-8859-6 *)
  |  `Enc_iso88597   (* ISO-8859-7 *)
  |  `Enc_iso88598   (* ISO-8859-8 *)
  |  `Enc_iso88599   (* ISO-8859-9 *)
  |  `Enc_iso885910  (* ISO-8859-10 *)
  |  `Enc_iso885911  (* ISO-8859-11 *)
  |  `Enc_iso885913  (* ISO-8859-13 *)
  |  `Enc_iso885914  (* ISO-8859-14 *)
  |  `Enc_iso885915  (* ISO-8859-15 *)
  |  `Enc_iso885916  (* ISO-8859-16 *)
  |  `Enc_koi8r      (* KOI8-R *)   (* http://koi8.pp.ru *)
(*|  `Enc_koi8u      (* KOI8-U *)   (* http://www.net.ua/KOI8-U/index.html *)*)
  |  `Enc_jis0201    (* JIS-X-0201 *)
(*
  |  `Enc_jis0201_roman  (* JIS-X-0201 only roman half *)
  |  `Enc_jis0201_kana   (* JIS-X-0201 katakana half remapped to 0x21..XXX *)
  |  `Enc_jis0208_94x94  (* JIS-X-0208 in ISO-2022-style two byte encoding *)
  |  `Enc_jis0212_94x94  (* JIS-X-0212 in ISO-2022-style two byte encoding *)
*)
  |  `Enc_eucjp      (* EUC-JP *)
  |  `Enc_euckr      (* EUC-KR *)
(* 
  |  `Enc_iso2022 of iso2022_state
  |  `Enc_iso2022jp of iso2022jp_state
*)
    (* Microsoft: *)
  |  `Enc_windows1250  (* WINDOWS-1250 *)
  |  `Enc_windows1251  (* WINDOWS-1251 *)
  |  `Enc_windows1252  (* WINDOWS-1252 *)
  |  `Enc_windows1253  (* WINDOWS-1253 *)
  |  `Enc_windows1254  (* WINDOWS-1254 *)
  |  `Enc_windows1255  (* WINDOWS-1255 *)
  |  `Enc_windows1256  (* WINDOWS-1256 *)
  |  `Enc_windows1257  (* WINDOWS-1257 *)
  |  `Enc_windows1258  (* WINDOWS-1258 *)
    (* IBM, ASCII-based: *)
  |  `Enc_cp437
  |  `Enc_cp737
  |  `Enc_cp775
  |  `Enc_cp850
  |  `Enc_cp852
  |  `Enc_cp855
  |  `Enc_cp856
  |  `Enc_cp857
  |  `Enc_cp860
  |  `Enc_cp861
  |  `Enc_cp862
  |  `Enc_cp863
  |  `Enc_cp864
  |  `Enc_cp865
  |  `Enc_cp866  (* Russian *)
  |  `Enc_cp869
  |  `Enc_cp874
  |  `Enc_cp1006
   (* IBM, EBCDIC-based: *)
  |  `Enc_cp037   (* EBCDIC USA Canada *)
      (* 273: EBCDIC Germany, Austria,
       * 277: Denmark, Norway,
       * 278: Finland, Sweden,
       * 280: Italy,
       * 284: Spain, Latin America,
       * 285: United Kingdom,
       * 297: France,
       * 871: Iceland,
       *)
  |  `Enc_cp424
  |  `Enc_cp500  (* EBCDIC International *)
  |  `Enc_cp875  (* EBCDIC Modern Greek *)
  |  `Enc_cp1026 (* EBCDIC Turkish *)
  |  `Enc_cp1047 (* EBCDIC Latin1, OS 390 System Services *)
   (* Adobe: *)
  |  `Enc_adobe_standard_encoding
  |  `Enc_adobe_symbol_encoding
  |  `Enc_adobe_zapf_dingbats_encoding
   (* Apple: *)
  |  `Enc_macroman
   (* Encoding subset: *)
  |  `Enc_subset of (encoding * (int -> bool))
  |  `Enc_empty
  ]
;;


type charset =
  [  `Set_unicode    (* The full Unicode repertoire *)
  |  `Set_usascii    (* US-ASCII (only 7 bit) *)
  |  `Set_iso88591   (* ISO-8859-1 *)
  |  `Set_iso88592   (* ISO-8859-2 *)
  |  `Set_iso88593   (* ISO-8859-3 *)
  |  `Set_iso88594   (* ISO-8859-4 *)
  |  `Set_iso88595   (* ISO-8859-5 *)
  |  `Set_iso88596   (* ISO-8859-6 *)
  |  `Set_iso88597   (* ISO-8859-7 *)
  |  `Set_iso88598   (* ISO-8859-8 *)
  |  `Set_iso88599   (* ISO-8859-9 *)
  |  `Set_iso885910  (* ISO-8859-10 *)
  |  `Set_iso885911  (* ISO-8859-11 *)
  |  `Set_iso885913  (* ISO-8859-13 *)
  |  `Set_iso885914  (* ISO-8859-14 *)
  |  `Set_iso885915  (* ISO-8859-15 *)
  |  `Set_iso885916  (* ISO-8859-16 *)
  |  `Set_koi8r      (* KOI8-R *)
  |  `Set_jis0201    (* JIS-X-0201 *)
  |  `Set_jis0208    (* JIS-X-0208 *)
  |  `Set_jis0212    (* JIS-X-0212 *)
  |  `Set_ks1001     (* KS-X-1001 *)
    (* Microsoft: *)
  |  `Set_windows1250  (* WINDOWS-1250 *)
  |  `Set_windows1251  (* WINDOWS-1251 *)
  |  `Set_windows1252  (* WINDOWS-1252 *)
  |  `Set_windows1253  (* WINDOWS-1253 *)
  |  `Set_windows1254  (* WINDOWS-1254 *)
  |  `Set_windows1255  (* WINDOWS-1255 *)
  |  `Set_windows1256  (* WINDOWS-1256 *)
  |  `Set_windows1257  (* WINDOWS-1257 *)
  |  `Set_windows1258  (* WINDOWS-1258 *)
    (* IBM, ASCII-based: *)
  |  `Set_cp437
  |  `Set_cp737
  |  `Set_cp775
  |  `Set_cp850
  |  `Set_cp852
  |  `Set_cp855
  |  `Set_cp856
  |  `Set_cp857
  |  `Set_cp860
  |  `Set_cp861
  |  `Set_cp862
  |  `Set_cp863
  |  `Set_cp864
  |  `Set_cp865
  |  `Set_cp866
  |  `Set_cp869
  |  `Set_cp874
  |  `Set_cp1006
   (* IBM, EBCDIC-based: *)
  |  `Set_cp037
  |  `Set_cp424
  |  `Set_cp500
  |  `Set_cp875
  |  `Set_cp1026
  |  `Set_cp1047
   (* Adobe: *)
  |  `Set_adobe_standard_encoding
  |  `Set_adobe_symbol_encoding
  |  `Set_adobe_zapf_dingbats_encoding
   (* Apple: *)
  |  `Set_macroman
  ]
;;


let ascii_compat_encodings =
  [ `Enc_utf8; `Enc_java; `Enc_usascii;
    `Enc_iso88591; `Enc_iso88592; `Enc_iso88593; `Enc_iso88594; `Enc_iso88595;
    `Enc_iso88596; `Enc_iso88597; `Enc_iso88598; `Enc_iso88599; `Enc_iso885910;
    `Enc_iso885911; `Enc_iso885913; `Enc_iso885914; `Enc_iso885915;
    `Enc_iso885916;
    `Enc_koi8r;
    `Enc_windows1250; `Enc_windows1251; `Enc_windows1252; `Enc_windows1253;
    `Enc_windows1254; `Enc_windows1255; `Enc_windows1256; `Enc_windows1257;
    `Enc_windows1258;
    `Enc_cp437; `Enc_cp737; `Enc_cp775; `Enc_cp850; `Enc_cp852; `Enc_cp855;
    `Enc_cp856; `Enc_cp857; `Enc_cp860; `Enc_cp861; `Enc_cp862; `Enc_cp863;
    `Enc_cp864; `Enc_cp865; `Enc_cp866; `Enc_cp869; `Enc_cp874; `Enc_cp1006;
    `Enc_eucjp; `Enc_euckr;
    `Enc_macroman;
  ]
;;


let rec is_ascii_compatible = 
  function
    | `Enc_subset(e,_) -> is_ascii_compatible e
    | e -> List.mem e ascii_compat_encodings
;;


let rec is_single_byte =
  function
      `Enc_utf8
    | `Enc_java
    | `Enc_utf16
    | `Enc_utf16_le
    | `Enc_utf16_be -> false
    | `Enc_eucjp -> false
    | `Enc_euckr -> false
    | `Enc_subset(e,_) -> is_single_byte e
    | _ -> true
;;


let punct_re = Netstring_pcre.regexp "[\\-\\_\\.]";;
let ibm_re = Netstring_pcre.regexp "^IBM(\\d+)$";;
let year_re = Netstring_pcre.regexp ":\\d\\d\\d\\d$";;


let norm_enc_name e =
  (* Removes some punctuation characters from e; uppercase;
   * converts "IBM#" to "CP#"; drops ":YEAR" suffixes 
   *)
  let e1 = String.uppercase e in
  let e2 = Netstring_pcre.global_replace punct_re "" e1 in
  let e3 = Netstring_pcre.global_replace year_re "" e2 in
  match Netstring_pcre.string_match ibm_re e3 0 with
      Some r ->
	"CP" ^ Netstring_pcre.matched_group r 1 e3
    | None ->
	e3
;;


let names =
  (* The first name is the official name, the other are aliases.
   * The aliases must not contain any of the punctuation characters
   * - _ .
   * `Enc_subset is missing in this list, of course.
   *
   * http://www.iana.org/assignments/character-sets
   *
   * A good reference is also:
   * http://www.firstobject.com/character-set-name-alias-code-page.htm
   *)
  [ `Enc_utf16,        [ "UTF-16"; "UTF16"; "UCS2"; "ISO10646UCS2" ];
    `Enc_utf16_be,     [ "UTF-16BE"; "UTF16BE" ];
    `Enc_utf16_le,     [ "UTF-16LE"; "UTF16LE" ];
    `Enc_utf8,         [ "UTF-8"; "UTF8" ];
    `Enc_java,         [ "UTF-8-JAVA"; "UTF8JAVA"; "JAVA" ];
    `Enc_usascii,      [ "US-ASCII"; "USASCII"; "ASCII"; "ISO646US"; "CP367"; 
			 "ISOIR6"; "ANSIX341968" ];
    `Enc_iso88591,     [ "ISO-8859-1"; "ISO88591"; "LATIN1"; "CP819"; 
			 "ISOIR100" ];
    `Enc_iso88592,     [ "ISO-8859-2"; "ISO88592"; "LATIN2"; "ISOIR101";
			 "CP912"
		       ];
    `Enc_iso88593,     [ "ISO-8859-3"; "ISO88593"; "LATIN3"; "ISOIR109" ];
    `Enc_iso88594,     [ "ISO-8859-4"; "ISO88594"; "LATIN4"; "ISOIR110" ];
    `Enc_iso88595,     [ "ISO-8859-5"; "ISO88595"; "CYRILLIC"; "ISOIR144";
			 "CP915"
		       ];
    `Enc_iso88596,     [ "ISO-8859-6"; "ISO88596"; "ARABIC"; "ECMA114"; 
			 "ASMO708"; "ISOIR127"; "CP1089" ];
    `Enc_iso88597,     [ "ISO-8859-7"; "ISO88597"; "GREEK"; "GREEK8"; 
			 "ELOT928"; "ECMA118"; "ISOIR126"; "CP813" ];
    `Enc_iso88598,     [ "ISO-8859-8"; "ISO88598"; "HEBREW"; "ISOIR138";
			 "CP916"
		       ];
    `Enc_iso88599,     [ "ISO-8859-9"; "ISO88599"; "LATIN5"; "ISOIR148";
			 "CP920"
		       ];
    `Enc_iso885910,    [ "ISO-8859-10"; "ISO885910"; "LATIN6"; "ISOIR157" ];
    `Enc_iso885911,    [ "ISO-8859-11"; "ISO885911"; ];
    `Enc_iso885913,    [ "ISO-8859-13"; "ISO885913"; "LATIN7" ];
    `Enc_iso885914,    [ "ISO-8859-14"; "ISO885914"; "LATIN8"; "ISOIR199"; 
			 "ISOCELTIC" ];
    `Enc_iso885915,    [ "ISO-8859-15"; "ISO885915"; "LATIN9"; "ISOIR203" ];
    `Enc_iso885916,    [ "ISO-8859-16"; "ISO885916"; "LATIN10"; "SR14111"; 
			 "ROMANIAN"; "ISOIR226" ];
    `Enc_koi8r,        [ "KOI8-R"; "KOI8R"; "CP878" ];
    `Enc_jis0201,      [ "JIS_X0201"; "JIS0201"; "JISX0201"; "X0201" ];
    `Enc_eucjp,        [ "EUC-JP"; "EUCJP"; ];
    `Enc_euckr,        [ "EUC-KR"; "EUCKR"; ];
    `Enc_windows1250,  [ "WINDOWS-1250"; "WINDOWS1250" ];
    `Enc_windows1251,  [ "WINDOWS-1251"; "WINDOWS1251" ];
    `Enc_windows1252,  [ "WINDOWS-1252"; "WINDOWS1252" ];
    `Enc_windows1253,  [ "WINDOWS-1253"; "WINDOWS1253" ];
    `Enc_windows1254,  [ "WINDOWS-1254"; "WINDOWS1254" ];
    `Enc_windows1255,  [ "WINDOWS-1255"; "WINDOWS1255" ];
    `Enc_windows1256,  [ "WINDOWS-1256"; "WINDOWS1256" ];
    `Enc_windows1257,  [ "WINDOWS-1257"; "WINDOWS1257" ];
    `Enc_windows1258,  [ "WINDOWS-1258"; "WINDOWS1258" ];
    `Enc_cp437,        [ "IBM437"; "CP437" ];
    `Enc_cp737,        [ "IBM737"; "CP737" ];
    `Enc_cp775,        [ "IBM775"; "CP775" ];
    `Enc_cp850,        [ "IBM850"; "CP850" ];
    `Enc_cp852,        [ "IBM852"; "CP852" ];
    `Enc_cp855,        [ "IBM855"; "CP855" ];
    `Enc_cp856,        [ "IBM856"; "CP856" ];
    `Enc_cp857,        [ "IBM857"; "CP857" ];
    `Enc_cp860,        [ "IBM860"; "CP860" ];
    `Enc_cp861,        [ "IBM861"; "CP861"; "CPIS" ];
    `Enc_cp862,        [ "IBM862"; "CP862" ];
    `Enc_cp863,        [ "IBM863"; "CP863" ];
    `Enc_cp864,        [ "IBM864"; "CP864" ];
    `Enc_cp865,        [ "IBM865"; "CP865" ];
    `Enc_cp866,        [ "IBM866"; "CP866" ];
    `Enc_cp869,        [ "IBM869"; "CP869"; "CPGR" ];
    `Enc_cp874,        [ "IBM874"; "CP874" ];
    `Enc_cp1006,       [ "IBM1006"; "CP1006" ];
    `Enc_cp037,        [ "IBM037"; "CP037"; "EBCDICCPUS"; "EBCDICCPCA"; 
			 "EBCDICCPWT"; "EBCDICCPNL" ];
    `Enc_cp424,        [ "IBM424"; "CP424"; "EBCDICCPHE" ];
    `Enc_cp500,        [ "IBM500"; "CP500"; "EBCDICCPBE"; "EBCDICCPCH" ];
    `Enc_cp875,        [ "IBM875"; "CP875" ];
    `Enc_cp1026,       [ "IBM1026"; "CP1026" ];
    `Enc_cp1047,       [ "IBM1047"; "CP1047"; ];
    `Enc_adobe_standard_encoding,       [ "ADOBE-STANDARD-ENCODING"; 
					  "ADOBESTANDARDENCODING" ];
    `Enc_adobe_symbol_encoding,         [ "ADOBE-SYMBOL-ENCODING"; 
					  "ADOBESYMBOLENCODING" ];
    `Enc_adobe_zapf_dingbats_encoding,  [ "ADOBE-ZAPF-DINGBATS-ENCODING"; 
					  "ADOBEZAPFDINGBATSENCODING" ];
    `Enc_macroman,                      [ "MACINTOSH"; "MACINTOSH"; 
					  "MACROMAN"; "MAC" ];
  ]
;;


let encoding_of_string e =
  let ne = norm_enc_name e in
  try
    fst
      (List.find
	 (fun (enc, nlist) ->
	    List.mem ne (List.tl nlist))
	 names
      )
  with
      Not_found ->
	failwith "Netconversion.encoding_of_string: unknown encoding"
;;


let rec string_of_encoding (e : encoding) =
  (* If there is a "preferred MIME name", this name is returned (see IANA). *)
  match e with
    | `Enc_subset(e,_) -> string_of_encoding e
    | _ ->
	try
	  let l = List.assoc e names in
	  List.hd l
	with
	    Not_found -> assert false
	      (* Because [names] must be complete *)
;;


let internal_name (cs : charset) =
  (* The name used for netdb lookups *)
  match cs with
    | `Set_unicode -> "unicode"
    | `Set_usascii -> "usascii"
    | `Set_iso88591 -> "iso88591"
    | `Set_iso88592 -> "iso88592"
    | `Set_iso88593 -> "iso88593"
    | `Set_iso88594 -> "iso88594"
    | `Set_iso88595 -> "iso88595"
    | `Set_iso88596 -> "iso88596"
    | `Set_iso88597 -> "iso88597"
    | `Set_iso88598 -> "iso88598"
    | `Set_iso88599 -> "iso88599"
    | `Set_iso885910 -> "iso885910"
    | `Set_iso885911 -> "iso885911"
    | `Set_iso885913 -> "iso885913"
    | `Set_iso885914 -> "iso885914"
    | `Set_iso885915 -> "iso885915"
    | `Set_iso885916 -> "iso885916"
    | `Set_koi8r -> "koi8r"
    | `Set_jis0201 -> "jis0201"
    | `Set_jis0208 -> "jis0208"
    | `Set_jis0212 -> "jis0212"
    | `Set_ks1001 -> "ks1001"
    | `Set_windows1250 -> "windows1250"
    | `Set_windows1251 -> "windows1251"
    | `Set_windows1252 -> "windows1252"
    | `Set_windows1253 -> "windows1253"
    | `Set_windows1254 -> "windows1254"
    | `Set_windows1255 -> "windows1255"
    | `Set_windows1256 -> "windows1256"    
    | `Set_windows1257 -> "windows1257"
    | `Set_windows1258 -> "windows1258"
    | `Set_cp437 -> "cp437"
    | `Set_cp737 -> "cp737"
    | `Set_cp775 -> "cp775"
    | `Set_cp850 -> "cp850"
    | `Set_cp852 -> "cp852"
    | `Set_cp855 -> "cp855"
    | `Set_cp856 -> "cp856"
    | `Set_cp857 -> "cp857"
    | `Set_cp860 -> "cp860"
    | `Set_cp861 -> "cp861"
    | `Set_cp862 -> "cp862"
    | `Set_cp863 -> "cp863"
    | `Set_cp864 -> "cp864"
    | `Set_cp865 -> "cp865"
    | `Set_cp866 -> "cp866"
    | `Set_cp869 -> "cp869"
    | `Set_cp874 -> "cp874"
    | `Set_cp1006 -> "cp1006"
    | `Set_cp037 -> "cp037"
    | `Set_cp424 -> "cp424"
    | `Set_cp500 -> "cp500"
    | `Set_cp875 -> "cp875"
    | `Set_cp1026 -> "cp1026"
    | `Set_cp1047 -> "cp1047"
    | `Set_adobe_standard_encoding -> "adobe_standard_encoding"
    | `Set_adobe_symbol_encoding -> "adobe_symbol_encoding"
    | `Set_adobe_zapf_dingbats_encoding -> "adobe_zapf_dingbats_encoding"
    | `Set_macroman -> "macroman"
;;


let rec required_charsets (e : encoding) =
  (* The name is a bit misleading. The function returns the charsets that
   * correspond to the conversion tables that are required to support the
   * encoding.
   *)
  match e with
    | `Enc_utf8 | `Enc_java | `Enc_utf16 | `Enc_utf16_le | `Enc_utf16_be ->
	[]
    | `Enc_usascii -> []
    | `Enc_iso88591 -> []
    | `Enc_iso88592 -> [ `Set_iso88592 ]
    | `Enc_iso88593 -> [ `Set_iso88593 ]
    | `Enc_iso88594 -> [ `Set_iso88594 ]
    | `Enc_iso88595 -> [ `Set_iso88595 ]
    | `Enc_iso88596 -> [ `Set_iso88596 ]
    | `Enc_iso88597 -> [ `Set_iso88597 ]
    | `Enc_iso88598 -> [ `Set_iso88598 ]
    | `Enc_iso88599 -> [ `Set_iso88599 ]
    | `Enc_iso885910 -> [ `Set_iso885910 ]
    | `Enc_iso885911 -> [ `Set_iso885911 ]
    | `Enc_iso885913 -> [ `Set_iso885913 ]
    | `Enc_iso885914 -> [ `Set_iso885914 ]
    | `Enc_iso885915 -> [ `Set_iso885915 ]
    | `Enc_iso885916 -> [ `Set_iso885916 ]
    | `Enc_koi8r -> [ `Set_koi8r ]
    | `Enc_jis0201 -> [ `Set_jis0201 ]
    | `Enc_eucjp -> [ `Set_jis0201; `Set_jis0208; `Set_jis0212 ]
    | `Enc_euckr -> [ `Set_ks1001 ]
    | `Enc_windows1250 -> [ `Set_windows1250 ]
    | `Enc_windows1251 -> [ `Set_windows1251 ]
    | `Enc_windows1252 -> [ `Set_windows1252 ]
    | `Enc_windows1253 -> [ `Set_windows1253 ]
    | `Enc_windows1254 -> [ `Set_windows1254 ]
    | `Enc_windows1255 -> [ `Set_windows1255 ]
    | `Enc_windows1256 -> [ `Set_windows1256 ]    
    | `Enc_windows1257 -> [ `Set_windows1257 ]
    | `Enc_windows1258 -> [ `Set_windows1258 ]
    | `Enc_cp437 -> [ `Set_cp437 ]
    | `Enc_cp737 -> [ `Set_cp737 ]
    | `Enc_cp775 -> [ `Set_cp775 ]
    | `Enc_cp850 -> [ `Set_cp850 ]
    | `Enc_cp852 -> [ `Set_cp852 ]
    | `Enc_cp855 -> [ `Set_cp855 ]
    | `Enc_cp856 -> [ `Set_cp856 ]
    | `Enc_cp857 -> [ `Set_cp857 ]
    | `Enc_cp860 -> [ `Set_cp860 ]
    | `Enc_cp861 -> [ `Set_cp861 ]
    | `Enc_cp862 -> [ `Set_cp862 ]
    | `Enc_cp863 -> [ `Set_cp863 ]
    | `Enc_cp864 -> [ `Set_cp864 ]
    | `Enc_cp865 -> [ `Set_cp865 ]
    | `Enc_cp866 -> [ `Set_cp866 ]
    | `Enc_cp869 -> [ `Set_cp869 ]
    | `Enc_cp874 -> [ `Set_cp874 ]
    | `Enc_cp1006 -> [ `Set_cp1006 ]
    | `Enc_cp037 -> [ `Set_cp037 ]
    | `Enc_cp424 -> [ `Set_cp424 ]
    | `Enc_cp500 -> [ `Set_cp500 ]
    | `Enc_cp875 -> [ `Set_cp875 ]
    | `Enc_cp1026 -> [ `Set_cp1026 ]
    | `Enc_cp1047 -> [ `Set_cp1047 ]
    | `Enc_adobe_standard_encoding -> [ `Set_adobe_standard_encoding ]
    | `Enc_adobe_symbol_encoding -> [ `Set_adobe_symbol_encoding ]
    | `Enc_adobe_zapf_dingbats_encoding -> [ `Set_adobe_zapf_dingbats_encoding ]
    | `Enc_macroman -> [ `Set_macroman ]
    | `Enc_subset(e',_) -> required_charsets e'
    | `Enc_empty -> []
;;


let rec same_encoding e1 e2 =
  match (e1,e2) with
      (`Enc_subset(e1_sub, f1), `Enc_subset(e2_sub, f2)) ->
	same_encoding e1_sub e2_sub && f1 == f2
    | (_,_) ->
	e1 = e2
;;


let rec byte_order_mark =
  function
      `Enc_utf16_le -> "\255\254"
    | `Enc_utf16_be -> "\254\255"
    | `Enc_subset(e,_) -> byte_order_mark e
    | _ -> ""
;;



let available_input_encodings() =
  let l = ref [] in
  List.iter
    (fun (e,_) ->
       let charsets = required_charsets e in
       if List.for_all 
	    (fun cs -> Netdb.exists_db ("cmapf." ^ internal_name cs)) charsets
       then
	 l := e :: !l
    )
    names;
  !l
;;


let available_output_encodings() =
  let exclude = [ `Enc_utf16 ] in
  let l = ref [] in
  List.iter
    (fun (e,_) ->
       if not (List.mem e exclude) then begin
	 let charsets = required_charsets e in
	 if List.for_all 
	      (fun cs -> Netdb.exists_db ("cmapr." ^ internal_name cs)) charsets
	 then
	   l := e :: !l
       end
    )
    names;
  !l
;;


let (win32_code_pages : (_ * encoding) list) =
  [  65001, `Enc_utf8;
     1200,  `Enc_utf16_le;
     1201,  `Enc_utf16_be;
     20127, `Enc_usascii;
     28591, `Enc_iso88591;
     28592, `Enc_iso88592;
     28593, `Enc_iso88593;
     28594, `Enc_iso88594;
     28595, `Enc_iso88595;
     28596, `Enc_iso88596;
     28597, `Enc_iso88597;
     28598, `Enc_iso88598;
     28599, `Enc_iso88599;
     (* `Enc_iso885910 *)
     (* `Enc_iso885911 *)
     28603, `Enc_iso885913;
     (* `Enc_iso885914 *)
     28605, `Enc_iso885915;
     (* `Enc_iso885916 *)
     20866, `Enc_koi8r;
     (* `Enc_jis0201 *)
     20932, `Enc_eucjp;
     51949, `Enc_euckr;
     1250, `Enc_windows1250;
     1251, `Enc_windows1251;
     1252, `Enc_windows1252;
     1253, `Enc_windows1253;
     1254, `Enc_windows1254;
     1255, `Enc_windows1255;
     1256, `Enc_windows1256;
     1257, `Enc_windows1257;
     1258, `Enc_windows1258;
     437, `Enc_cp437;
     737, `Enc_cp737;
     775, `Enc_cp775;
     850, `Enc_cp850;
     852, `Enc_cp852;
     855, `Enc_cp855;
     (* `Enc_cp856 *)
     857, `Enc_cp857;
     860, `Enc_cp860;
     861, `Enc_cp861;
     862, `Enc_cp862;
     863, `Enc_cp863;
     864, `Enc_cp864;
     865, `Enc_cp865;
     866, `Enc_cp866;
     869, `Enc_cp869;
     874, `Enc_cp874;
     (* `Enc_cp1006 *)
     37, `Enc_cp037;
     20424, `Enc_cp424;
     500, `Enc_cp500;
     875, `Enc_cp875;
     1026, `Enc_cp1026;
     1047, `Enc_cp1047;
     (* `Enc_adobe_standard_encoding *)
     (* `Enc_adobe_symbol_encoding *)
     (* `Enc_adobe_zapf_dingbats_encoding *)
     10000, `Enc_macroman;
  ]

let user_encoding() =
  match Sys.os_type with
    | "Win32" ->
	let cp = Netsys_win32.get_active_code_page() in
	( try Some(List.assoc cp win32_code_pages)
	  with Not_found -> None
	)
    | _ ->
	( try
	    let codeset = 
	      (Netsys_posix.query_langinfo "").Netsys_posix.nl_CODESET in
	    Some(encoding_of_string codeset)
	  with
	    | _ -> None
	)

(* Internal conversion interface:
 *
 * let (n_char, n_byte, enc') = read_XXX slice_char slice_blen s_in p_in l_in:
 *
 *  - Scans the bytes from position p_in until the slice is decoded, but at
 *    most until the last position p_in+l_in-1 of the input string s_in, and 
 *    decodes the character for the selected encoding.
 *  - "slice_char" is a preallocated array of ints storing the code points
 *    of the characters. It is allowed that "slice_char" is only partially
 *    filled with characters. In this case, there must be a -1 after the
 *    last valid code point.
 *  - "slice_blen" is another "int array" with the same size as "slice_char".
 *    It contains the byte length of every character. It is initialized with
 *    a sequence of ones, so single-byte readers don't have to worry about
 *    this array.
 *  - Returns:
 *      * n_char: the number of decoded characters
 *      * n_byte: the number of scanned bytes ( <= l_in )
 *      * enc': the new encoding
 *  - In the case of multi-byte encodings it is possible that
 *    the last byte to read at position p_in+l_in-1 is the beginning of
 *    a character. This character is excluded from being decoded.
 *  - Errors: If an invalid byte sequence is found, the exception
 *    Malformed_code_read(_,_,_) is raised. The exception returns the
 *    triple (n_char, n_byte, enc') describing how much could be read
 *    before the reader ran into the bad sequence. slice_char and slice_blen
 *    are only partially initialized, with a (-1) at the end of slice_char.
 *
 * let (n_char, n_byte) = 
 *        write_XXX slice_char slice_pos slice_length s_out p_out l_out subst
 *
 *  - Writes the characters found in slice_char to s_out. Only the elements
 *    from slice_pos to slice_pos + slice_length -1 are written. The resulting
 *    bytes are written to s_out from byte position p_out to p_out+l_out-1.
 *  - There must not be a -1 (EOF mark) in the first slice_length characters
 *    of slice_char.
 *  - Only whole characters must be written.
 *  - For code points p that cannot be represented in the output
 *    encoding, the function subst is called. The function must return
 *    the (already encoded) string to substitute. This must be a small string.
 *  - Of course, p >= 0. As special case, p >= 0x110000 may be used to force
 *    that subst is called (it is assumed that max_int can be never
 *    represented).
 *  - Returns:
 *      * n_char: the number of processed characters
 *      * n_byte: the number of written bytes ( <= l_in )
 *
 * let (n_char, n_byte) =
 *        back_XXX s_in range_in range_in_len p_in n_char:
 *
 *  - The substring of s_in beginning at range_in and with length
 *    range_in_len is considered as the valid range
 *  - The cursor is at byte position p_in and goes n_char characters back
 *  - The routine returns:
 *      * n_char: the characters the cursor was actually moved backwards
 *      * n_byte: the bytes the cursor was actually moved backwards
 *  - The validity of the input encoding needs not to be checked
 *)

exception Malformed_code_read of (int * int * encoding);;
  (* not exported! *)

Callback.register_exception "Netconversion.Malformed_code_read"
			    (Malformed_code_read(0,0,`Enc_empty));;
  (* Needed by netaccel_c.c *)




(* UNSAFE_OPT: A number of functions have been optimized by using
 * unsafe features of O'Caml (unsafe_get, unsafe_set, unsafe_chr).
 * These functions have been checked very carefully, and there are
 * a lot of comments arguing about the correctness of the array
 * and string accesses.
 *)


let read_iso88591 maxcode enc slice_char slice_blen s_in p_in l_in =
  (* UNSAFE_OPT *)
  assert(Array.length slice_char = Array.length slice_blen);
  assert(p_in >= 0 && p_in + l_in <= String.length s_in && l_in >= 0);
  let m = min l_in (Array.length slice_char) in
  for k = 0 to m-1 do
    (* let ch = Char.code s_in.[ p_in + k ] in *)
    let ch = Char.code (String.unsafe_get s_in (p_in + k)) in
    if ch > maxcode then (
      slice_char.(k) <- (-1);
      raise(Malformed_code_read(k,k,enc))
    );
    (* slice_char.(k) <- ch *)
    Array.unsafe_set slice_char k ch;
  done;
  if m < Array.length slice_char then (
    slice_char.(m) <- (-1);
  );
  (m,m,enc)
;;


let read_iso88591_ref = ref read_iso88591;;


let get_8bit_to_unicode_map enc =
  let cs = 
    match required_charsets enc with
	[ cs ] -> cs 
      | _ -> failwith "get_8bit_to_unicode_map" in
  let to_unicode = Netmappings.get_to_unicode (internal_name cs) in
  assert(Array.length to_unicode = 256);
  to_unicode
;;


let read_8bit enc =
  let m_to_unicode = get_8bit_to_unicode_map enc in
  (* the 256-byte array mapping the character set to unicode *)

  fun slice_char slice_blen s_in p_in l_in ->
    (* UNSAFE_OPT *)
    assert(Array.length slice_char = Array.length slice_blen);
    assert(p_in >= 0 && p_in + l_in <= String.length s_in && l_in >= 0);
    let m = min l_in (Array.length slice_char) in
    for k = 0 to m-1 do
      (* let ch_local = Char.code s_in.[ p_in + k ] in *)
      let ch_local = Char.code (String.unsafe_get s_in (p_in + k)) in
      let ch_uni = Array.unsafe_get m_to_unicode ch_local in  (* ok *)
      if ch_uni < 0 then (
	slice_char.(k) <- (-1);
	raise(Malformed_code_read(k,k,enc));
      );
      (* slice_char.(k) <- ch_uni *)
      Array.unsafe_set slice_char k ch_uni;
    done;
    if m < Array.length slice_char then (
      slice_char.(m) <- (-1);
    );
    (m,m,enc)
;;


let read_utf8 is_java slice_char slice_blen s_in p_in l_in =
  (* UNSAFE_OPT *)
  assert(Array.length slice_char = Array.length slice_blen);
  assert(p_in >= 0 && p_in + l_in <= String.length s_in && l_in >= 0);

  (* k: counts the bytes
   * n: counts the characters
   *)

  let p = ref p_in in
  let p_max = p_in + l_in in
  let n = ref 0 in
  let n_ret = ref (-1) in

  let malformed_code() =
    slice_char.( !n ) <- (-1);
    raise(Malformed_code_read(!n, !p - p_in, `Enc_utf8));
  in

  let slice_length = Array.length slice_char in

  while !p < p_max && !n < slice_length do
    let k_inc =
      (* length of the character in bytes; 0 means: stop *)

      (* We know:
       * (1) p_in >= 0 ==> !p >= 0
       * (2) !p < p_max = p_in + l_in <= String.length s_in
       * ==> unsafe get ok
       *)
      (* match s_in.[k_in + k] with *)
      match String.unsafe_get s_in !p with
	  '\000' ->
	    if is_java then malformed_code();
	    (* slice_char.(n) <- 0; *)
	    Array.unsafe_set slice_char !n 0;     (* ok *)
	    1
	| ('\001'..'\127' as x) ->
	    (* slice_char.(n) <- Char.code x; *)
	    Array.unsafe_set slice_char !n (Char.code x);     (* ok *)
	    1
	| ('\128'..'\223' as x) ->
	    if !p+1 >= p_max then
	      0
	    else begin
	      (* ==> !p+1 < p_max = p_in + l_in <= String.length s_in
	       * ==> unsafe get ok
	       *)
	      let n1 = Char.code x in
	      let n2 = (* Char.code (s_in.[!p + 1]) *)
		Char.code(String.unsafe_get s_in (!p + 1)) in
	      if is_java && (n1 = 0x80 && n2 = 0xc0) then begin
		(* slice_char.(n) <- 0; *)
		Array.unsafe_set slice_char !n 0;     (* ok *)
		2
	      end
	      else begin
		if n2 < 128 or n2 > 191 then malformed_code();
		let p = ((n1 land 0b11111) lsl 6) lor (n2 land 0b111111) in
		if p < 128 then malformed_code();
		(* slice_char.(n) <- p; *)
		Array.unsafe_set slice_char !n p;     (* ok *)
		2
	      end
	    end
	| ('\224'..'\239' as x) ->
	    if !p + 2 >= p_max then
	      0
	    else begin
	      (* ==> !p+2 < p_max = p_in + l_in <= String.length s_in
	       * ==> unsafe get ok
	       *)
	      let n1 = Char.code x in
	      let n2 = (* Char.code (s_in.[!p + 1]) *)
		Char.code(String.unsafe_get s_in (!p + 1)) in
	      let n3 = (* Char.code (s_in.[!p + 2]) *)
		Char.code(String.unsafe_get s_in (!p + 2)) in
	      if n2 < 128 or n2 > 191 then malformed_code();
	      if n3 < 128 or n3 > 191 then malformed_code();
	      let p =
		((n1 land 0b1111) lsl 12) lor
		((n2 land 0b111111) lsl 6) lor
		(n3 land 0b111111)
	      in
	      if p < 0x800 then malformed_code();
	      if (p >= 0xd800 && p < 0xe000) then
		(* Surrogate pairs are not supported in UTF-8 *)
		malformed_code();
	      if (p >= 0xfffe && p <= 0xffff) then
		malformed_code();
	      (* slice_char.(n) <- p; *)
	      Array.unsafe_set slice_char !n p;     (* ok *)
	      3
	    end
	| ('\240'..'\247' as x) ->
	    if !p + 3 >= p_max then
	      0
	    else begin
	      (* ==> !p+3 < p_max = p_in + l_in <= String.length s_in
	       * ==> unsafe get ok
	       *)
	      let n1 = Char.code x in
	      let n2 = (* Char.code (s_in.[!p + 1]) *)
		Char.code(String.unsafe_get s_in (!p + 1)) in
	      let n3 = (* Char.code (s_in.[!p + 2]) *)
		Char.code(String.unsafe_get s_in (!p + 2)) in
	      let n4 = (* Char.code (s_in.[!p + 3]) *)
		Char.code(String.unsafe_get s_in (!p + 3)) in
	      if n2 < 128 or n2 > 191 then malformed_code();
	      if n3 < 128 or n3 > 191 then malformed_code();
	      if n4 < 128 or n4 > 191 then malformed_code();
	      let p = ((n1 land 0b111) lsl 18) lor
		      ((n2 land 0b111111) lsl 12) lor
		      ((n3 land 0b111111) lsl 6) lor
		      (n4 land 0b111111)
	      in
	      if p < 0x10000 then malformed_code();
	      if p >= 0x110000 then
		(* These code points are not supported. *)
		malformed_code();
	      (* slice_char.(n) <- p; *)
	      Array.unsafe_set slice_char !n p;     (* ok *)
	      4
	    end
	| _ ->
	    (* Outside the valid range of XML characters *)
	    malformed_code();
    in
    (* If k_inc = 0, the character was partially outside the processed
     * range of the string, and could not be decoded.
     *)

    if k_inc > 0 then begin
      (* We know:
       * (1) n >= 0, because n starts with 0 and is only increased
       * (2) n < Array.length slice_char = Array.length slice_blen
       * ==> unsafe set ok
       *)
      (* slice_blen.(n) <- k_inc; *)
      Array.unsafe_set slice_blen !n k_inc;
      (* next iteration: *)
      p := !p + k_inc;
      incr n;
    end
    else begin
      (* Stop loop: *)
      n_ret := !n;
      n := slice_length;
    end
  done; 

  if (!n_ret = (-1)) then n_ret := !n;
  if !n_ret < slice_length then (
    (* EOF marker *)
    slice_char.(!n_ret) <- (-1);  
  );
  (!n_ret,!p-p_in,`Enc_utf8)
;;


let read_utf8_ref = ref read_utf8;;


let surrogate_offset = 0x10000 - (0xD800 lsl 10) - 0xDC00;;

let read_utf16_lebe lo hi n_start enc slice_char slice_blen s_in p_in l_in =
  (* lo=0, hi=1: little endian
   * lo=1, hi=0: big endian
   * n_start: First cell in slice to use
   *)
  assert(Array.length slice_char = Array.length slice_blen);
  assert(p_in >= 0 && p_in + l_in <= String.length s_in && l_in >= 0);

  let malformed_code k n =
    slice_char.(n) <- (-1);
    raise(Malformed_code_read(n,k,enc))
  in

  (* k: counts the bytes
   * n: counts the characters
   *)
  let rec put_loop k n =
    if k+1 < l_in && n < Array.length slice_char then begin
      let p = (Char.code s_in.[p_in + k + lo]) lor 
	      ((Char.code s_in.[p_in + k + hi]) lsl 8) in

      if p >= 0xd800 & p < 0xe000 then begin
	(* This is a surrogate pair. *)
	if k+3 < l_in then begin
	  if p <= 0xdbff then begin
	    let q = (Char.code s_in.[p_in + k + 2 + lo ]) lor
		    ((Char.code s_in.[p_in + k + 2 + hi]) lsl 8) in
	    if q < 0xdc00 or q > 0xdfff then malformed_code k n;
	    let eff_p = (p lsl 10) + q + surrogate_offset in
	    slice_char.(n) <- eff_p;
	    slice_blen.(n) <- 4;
	    put_loop (k+4) (n+1)
	  end
	  else
	    (* Malformed pair: *)
	    malformed_code k n;
	end
	else 
	  (n,k)
      end
      else
	(* Normal 2-byte character *)
	if p = 0xfffe then 
	  (* Wrong byte order mark: It is illegal here *)
	  malformed_code k n
	else begin
	  (* A regular code point *)
	  slice_char.(n) <- p;
	  slice_blen.(n) <- 2;
	  put_loop (k+2) (n+1)
	end
    end
    else
      (n,k)
  in
  let (n,k) = put_loop 0 n_start in
  if n < Array.length slice_char then (
    (* EOF marker *)
    slice_char.(n) <- (-1);
  );
  (n,k,enc)
;;


let get_endianess s_in p_in =
  let c0 = s_in.[p_in + 0] in
  let c1 = s_in.[p_in + 1] in
  if c0 = '\254' && c1 = '\255' then
    `Big_endian
  else
    if c0 = '\255' && c1 = '\254' then
      `Little_endian
    else
      `No_BOM
;;


(* expose_bom: when true, the BOM is considered as a character and
 * put as value (-3) into slice_char
 *)

let read_utf16 expose_bom slice_char slice_blen s_in p_in l_in =
  assert(Array.length slice_char = Array.length slice_blen);
  assert(p_in >= 0 && p_in + l_in <= String.length s_in && l_in >= 0);
  (* Expect a BOM at the beginning of the text *)
  if l_in >= 2 then begin
    if expose_bom then (
      slice_char.(0) <- (-3); 
      slice_blen.(0) <- 0;  (* Later corrected *)
    );
    match get_endianess s_in p_in with
	`Big_endian ->
	  let n_start = if expose_bom then 1 else 0 in
	  let (n, k, enc') = 
	    read_utf16_lebe
	      1 0 n_start `Enc_utf16_be 
	      slice_char slice_blen s_in (p_in+2) (l_in-2) in
	  if n > 0 then slice_blen.(0) <- slice_blen.(0) + 2;
	  (n, k+2, enc')
      | `Little_endian ->
	  let n_start = if expose_bom then 1 else 0 in
	  let (n, k, enc') = 
	    read_utf16_lebe 
	      0 1 n_start `Enc_utf16_le 
	      slice_char slice_blen s_in (p_in+2) (l_in-2) in
	  if n > 0 then slice_blen.(0) <- slice_blen.(0) + 2;
	  (n, k+2, enc')
      | `No_BOM ->
	  (* byte order mark missing *)
	  slice_char.(0) <- (-1);
	  raise(Malformed_code_read(0,0,`Enc_utf16))
  end
  else (
    slice_char.(0) <- (-1);
    (0, 0, `Enc_utf16)
  )
;;


let read_euc len1 len2 len3 map1 map2 map3 enc =
  (* Code set 0 is US-ASCII.
   * Code sets 1, 2, 3 may be anything. lenX = 0: code set is not supported.
   * lenX is either 0, 1, or 2.
   *)
  (* UNSAFE_OPT *)

  assert(len1 >= 0 && len1 <= 2);
  assert(len2 >= 0 && len2 <= 2);
  assert(len3 >= 0 && len3 <= 2);

  fun slice_char slice_blen s_in p_in l_in ->
    assert(Array.length slice_char = Array.length slice_blen);
    assert(p_in >= 0 && p_in + l_in <= String.length s_in && l_in >= 0);

    (* k: counts the bytes
     * n: counts the characters
     *)

    let p = ref p_in in
    let p_max = p_in + l_in in
    let n = ref 0 in
    let n_ret = ref (-1) in

    let malformed_code() =
      slice_char.( !n ) <- (-1);
      raise(Malformed_code_read(!n, !p - p_in, enc));
    in

    let slice_length = Array.length slice_char in

    while !p < p_max && !n < slice_length do
      let k_inc =
	(* length of the character in bytes; 0 means: stop *)

	(* We know:
	 * (1) p_in >= 0 ==> !p >= 0
	 * (2) !p < p_max = p_in + l_in <= String.length s_in
	 * ==> unsafe get ok
	 *)
	(* match s_in.[k_in + k] with *)
	match String.unsafe_get s_in !p with
	    '\000'..'\127' as x ->
	      (* US-ASCII *)
	      Array.unsafe_set slice_char !n (Char.code x);     (* ok *)
	      1
	  | '\142' ->
	      (* Code set 2 *)
	      if len2 = 0 then malformed_code();
	      if !p+len2 >= p_max then
		0
	      else begin
		let x1 = Char.code (s_in.[!p + 1]) in
		let x2 = if len2=1 then 256 else Char.code (s_in.[!p + 2]) in
		if x1 < 160 || x2 < 160 then malformed_code();
		let uni = map2 x1 x2 in
		Array.unsafe_set slice_char !n uni;     (* ok *)
		len2+1
	      end
	  | '\143' ->
	      (* Code set 3 *)
	      if len3 = 0 then malformed_code();
	      if !p+len3 >= p_max then
		0
	      else begin
		let x1 = Char.code (s_in.[!p + 1]) in
		let x2 = if len3=1 then 256 else Char.code (s_in.[!p + 2]) in
		if x1 < 160 || x2 < 160 then malformed_code();
		let uni = map3 x1 x2 in
		Array.unsafe_set slice_char !n uni;     (* ok *)
		len3+1
	      end
	  | '\160'..'\255' as x1_code ->
	      (* Code set 1 *)
	      if !p+len1 > p_max then
		0
	      else begin
		let x1 = Char.code x1_code in
		let x2 = if len1=1 then 256 else Char.code (s_in.[!p + 1]) in
		if x2 < 160 then malformed_code();
		let uni = map1 x1 x2 in
		Array.unsafe_set slice_char !n uni;     (* ok *)
		len1
	      end
	  | _ ->
	      (* illegal *)
	      malformed_code()
      in
      (* If k_inc = 0, the character was partially outside the processed
       * range of the string, and could not be decoded.
       *)
      
      if k_inc > 0 then begin
	(* We know:
	 * (1) n >= 0, because n starts with 0 and is only increased
	 * (2) n < Array.length slice_char = Array.length slice_blen
	 * ==> unsafe set ok
	 *)
	(* slice_blen.(n) <- k_inc; *)
	Array.unsafe_set slice_blen !n k_inc;
	(* next iteration: *)
	p := !p + k_inc;
	incr n;
      end
      else begin
	(* Stop loop: *)
	n_ret := !n;
	n := slice_length;
      end
    done; 
    
    if (!n_ret = (-1)) then n_ret := !n;
    if !n_ret < slice_length then (
      (* EOF marker *)
      slice_char.(!n_ret) <- (-1);  
    );
    (!n_ret,!p-p_in,enc)
;;
      

let read_eucjp () =
  let jis0201 = Netmappings.get_to_unicode "jis0201" in
  let jis0208 = Netmappings.get_to_unicode "jis0208" in
  let jis0212 = lazy (Netmappings.get_to_unicode "jis0212") in (* seldom *)
  let map1 x1 x2 =
    jis0208.( (x1-160) * 96 + x2 - 160 ) in
  let map2 x1 _ =
    jis0201.( x1 ) in
  let map3 x1 x2 =
    (Lazy.force jis0212).( (x1-160) * 96 + x2 - 160 ) in
  read_euc 2 1 2 map1 map2 map3 `Enc_eucjp
;;


let read_euckr () =
  let ks1001 = Netmappings.get_to_unicode "ks1001" in
  let map x1 x2 =
    ks1001.( (x1-160) * 96 + x2 - 160 ) in
  read_euc 2 0 0 map map map `Enc_euckr
;;


let read_subset inner_read def slice_char slice_blen s_in p_in l_in =
  assert(Array.length slice_char = Array.length slice_blen);
  assert(p_in >= 0 && p_in + l_in <= String.length s_in && l_in >= 0);

  let (n,k,enc') = inner_read slice_char slice_blen s_in p_in l_in in

  (* check codepoints: *)
  for j = 0 to n-1 do
    if not(def(slice_char.(j))) then (
      (* raise Malformed_code_read... *)
      (* to get enc'' read again: *)
      let slice_char' = Array.make j (-1) in
      let slice_blen' = Array.make j 1 in
      let (n', k', enc'') = 
	try
	  inner_read slice_char' slice_blen' s_in p_in l_in 
	with
	    Malformed_code_read(_,_,_) -> assert false
      in
      assert(n' = j);
      int_blit slice_char' 0 slice_char 0 j;
      int_blit slice_blen' 0 slice_blen 0 j;
      slice_char.(j) <- (-1);
      raise (Malformed_code_read(j, k', enc''))
    );
  done;

  (n,k,enc')
;;

(*
 * let (n_char, b_byte) = 
 *        write_XXX slice_char slice_length s_out p_out l_out subst
 *)

let write_iso88591 maxcode slice_char slice_pos slice_length 
                   s_out p_out l_out subst =
  (* UNSAFE_OPT *)
  (* Use maxcode=255 for ISO-8859-1, and maxcode=127 for US-ASCII,
   * and maxcode=(-1) for `Enc_empty.
   *)
  assert(p_out >= 0 && p_out + l_out <= String.length s_out && l_out >= 0);
  assert(slice_pos >= 0 && slice_pos+slice_length <= Array.length slice_char);
  assert(maxcode <= 255);

  let n = ref slice_pos in     (* index of slice *)
  let n_ret = ref (-1) in      (* returned number of characters *)
  let n_max = slice_pos + slice_length in

  let p = ref p_out in         (* current output position *)
  let p_max = p_out + l_out in (* maximum output position *)

  while ( !n < n_max ) && ( !p < p_max ) do
    (* We know:
     * (1) !n >= 0, because it starts with 0 and is only increased
     * (2) !n < n_max = slice_pos + slice_length <= Array.length slice_char
     * ==> unsafe get ok
     *)
    let ch = Array.unsafe_get slice_char !n in
    if ch >= 0 && ch <= maxcode then begin
      (* Because !p < p_max:
       * !p < p_max = p_out + l_out <= String.length s_out
       * Furthermore, p_out >= 0, !p >= 0.
       * ==> unsafe set ok
       *)
      (* s_out.[ !p ] <- Char.chr ch; *)
      String.unsafe_set s_out !p (Char.unsafe_chr ch);
      incr n;
      incr p;
    end
    else begin
      assert(ch >= 0);
      let replacement = subst ch in
      let l_repl = String.length replacement in
      if l_repl > multibyte_limit then
	failwith "Netconversion.write_iso88591: Substitution string too long";
      if !p + l_repl <= p_max then begin
	(* Enough space to store 'replacement': *)
	String.blit replacement 0 s_out !p l_repl;
	p := !p + l_repl;
	incr n
      end
      else begin
	(* Exit whole conversion *)
	n_ret := !n;
	n := n_max;
      end
    end
  done;
  if !n_ret >= 0 then (!n_ret - slice_pos, !p - p_out) 
                 else (!n - slice_pos,     !p - p_out)
;;


let get_8bit_from_unicode_map enc =
  let cs = 
    match required_charsets enc with
	[ cs ] -> cs 
      | _ -> failwith "get_8bit_from_unicode_map" in
  let from_unicode = Netmappings.get_from_unicode (internal_name cs) in
  assert(Array.length from_unicode = 256);
  from_unicode
;;


let write_8bit enc =
  (* UNSAFE_OPT *)

  let m_from_unicode = get_8bit_from_unicode_map enc in
  let m_mask = Array.length m_from_unicode - 1 in

  fun slice_char slice_pos slice_length s_out p_out l_out subst ->
    assert(p_out >= 0 && p_out + l_out <= String.length s_out && l_out >= 0);
    assert(slice_pos >= 0 && slice_pos+slice_length <= Array.length slice_char);

    let n = ref slice_pos in     (* index of slice *)
    let n_max = slice_pos + slice_length in

    let k = ref 0 in             (* written bytes *)
    let n_ret = ref (-1) in      (* returned number of characters *)

    while ( !n < n_max ) && ( !k < l_out ) do
      (* We know:
       * (1) !n >= 0, because it starts with 0 and is only increased
       * (2) !n < n_max = slice_pos + slice_length <= Array.length slice
       * ==> unsafe get ok
       *)
      let p = (* slice_char.( !n ) *) 
              Array.unsafe_get slice_char !n in
      let p' =
	match Array.unsafe_get m_from_unicode (p land m_mask) with
	    Netmappings.U_nil -> -1
	  | Netmappings.U_single (p0,q0) ->
	      if p0 = p then q0 else -1
	  | Netmappings.U_double (p0,q0,p1,q1) ->
	      if p0 = p then q0 else
		if p1 = p then q1 else -1
	  | Netmappings.U_array pq ->
	      let r = ref (-1) in
	      let h = ref 0 in
	      while !r < 0 && !h < Array.length pq do
		if pq.( !h ) = p then
		  r := pq.( !h+1 )
		else
		  h := !h + 2
	      done;
	      !r
      in
      (* If p=-1 ==> p'=-1, because -1 is never mapped to any code point *)

      if p' < 0 then begin
	if p < 0 then
	  assert false   (* EOF mark found *)
	else begin
	  let replacement = subst p in
	  let l_repl =  String.length replacement in
	  if l_repl > multibyte_limit then
	  failwith "Netconversion.write_8bit: Substitution string too long";
	
	  if !k + l_repl <= l_out then begin
	    (* Enough space to store 'replacement': *)
	    String.blit replacement 0 s_out (p_out + !k) l_repl;
	    k := !k + l_repl;
	    incr n
	  end
	  else begin
	    (* Exit whole conversion *)
	    n_ret := !n;
	    n := n_max;
	  end
	end
      end
      else begin
	(* Because !k < l_out:
	 * p_out + !k < p_out + l_out <= String.length s_out
	 * Furthermore, p_out >= 0, !k >= 0.
	 * ==> unsafe set ok
	 *)
	(* s_out.[ p_out + !k ] <- Char.chr p'; *)
	String.unsafe_set s_out (p_out + !k) (Char.unsafe_chr(p' land 0xff));
	incr n;
	incr k
      end;
    done;
  if !n_ret >= 0 then (!n_ret - slice_pos, !k) 
                 else (!n - slice_pos,     !k)
;;


let write_utf8 is_java
               slice_char slice_pos slice_length s_out p_out l_out subst =
  (* UNSAFE_OPT *)
  assert(p_out >= 0 && p_out + l_out <= String.length s_out && l_out >= 0);
  assert(slice_pos >= 0 && slice_pos+slice_length <= Array.length slice_char);

  let n = ref slice_pos in     (* index of slice *)
  let n_max = slice_pos + slice_length in

  let k = ref 0 in             (* written bytes *)
  let n_ret = ref (-1) in      (* returned number of characters *)

  while ( !n < n_max ) do
    (* We know:
     * (1) !n >= 0, because it starts with 0 and is only increased
     * (2) !n < n_max = slice_pos + slice_length <= Array.length slice
     * ==> unsafe get ok
     *)
    let p = (* slice.( !n ) *)
            Array.unsafe_get slice_char !n in

    let index = p_out + !k in

    let k_inc =
      (* k_inc: how many bytes are written. (-1) means: stop *)
      if p <= 127 && (not is_java || p <> 0) then begin
	if p < 0 then assert false;  (* EOF mark *)
	if !k < l_out then begin
	  (* (1) index = p_out + !k < p_out + l_out <= 
	   *     String.length s_out
	   * (2) p_out, !n >= 0
	   * ==> unsafe set ok
	   *
	   * 0 <= p <= 127 ==> unsafe_chr ok
	   *)
	  (* s_out.[index] <- Char.chr p; *)
	  String.unsafe_set s_out index (Char.unsafe_chr p);
	  1
	end
	else (-1)
      end
      else if p <= 0x7ff then begin
	if !k + 1 < l_out then begin
	  (* (1) index+1 = p_out + !k + 1 < p_out + l_out <= 
	   *     String.length s_out
	   * (2) p_out, !k >= 0
	   * ==> unsafe set ok
	   *
	   * p <= 0x7ff ==> p lsr 6 <= 0x1f 
	   *            ==> 0xc0 lor (p lsr 6) <= df
	   * p land 0x3f <= 0x3f ==> 0x80 lor (p land 0x3f) <= 0xbf
	   * ==> unsafe_chr ok
	   *)
	  (* s_out.[index]     <- Char.chr (0xc0 lor (p lsr 6)); *)
	  (* s_out.[index + 1] <- Char.chr (0x80 lor (p land 0x3f)); *)
	  String.unsafe_set s_out index 
	    (Char.unsafe_chr (0xc0 lor (p lsr 6)));
	  String.unsafe_set s_out (index+1)
	    (Char.unsafe_chr (0x80 lor (p land 0x3f)));
	  2
	end
	else (-1)
      end
      else if p <= 0xffff then begin
	(* Refuse writing surrogate pairs, and fffe, ffff *)
	if (p >= 0xd800 && p < 0xe000) || (p >= 0xfffe) then
	  failwith "Netconversion.write_utf8";
	if !k + 2 < l_out then begin
	  (* (1) index+2 = p_out + !k + 2 < p_out + l_out <= 
	   *     String.length s_out
	   * (2) p_out, !k >= 0
	   * ==> unsafe set ok
	   *
	   * Well, and it can be proven that unsafe_chr is ok, too...
	   *)
	  (* s_out.[index]     <- Char.chr (0xe0 lor (p lsr 12)); *)
	  (* s_out.[index + 1] <- Char.chr (0x80 lor ((p lsr 6) land 0x3f)); *)
	  (* s_out.[index + 2] <- Char.chr (0x80 lor (p land 0x3f)); *)
	  String.unsafe_set s_out index
	    (Char.unsafe_chr (0xe0 lor (p lsr 12)));
	  String.unsafe_set s_out (index+1)
	    (Char.unsafe_chr (0x80 lor ((p lsr 6) land 0x3f)));
	  String.unsafe_set s_out (index+2)
	    (Char.unsafe_chr (0x80 lor (p land 0x3f)));
	  3
	end
	else (-1)
      end
      else if p <= 0x10ffff then begin
	if !k + 3 < l_out then begin
	  (* No such characters are defined... *)
	  s_out.[index]     <- Char.chr (0xf0 lor (p lsr 18));
	  s_out.[index + 1] <- Char.chr (0x80 lor ((p lsr 12) land 0x3f));
	  s_out.[index + 2] <- Char.chr (0x80 lor ((p lsr 6)  land 0x3f));
	  s_out.[index + 3] <- Char.chr (0x80 lor (p land 0x3f));
	  4
	end
	else (-1)
      end
      else begin
	(* Higher code points are not possible in XML; call subst *)
	let replacement = subst p in
	let l_repl =  String.length replacement in
	if l_repl > multibyte_limit then
	  failwith "Netconversion.write_utf8: Substitution string too long";
	if !k + l_repl <= l_out then begin
	  (* Enough space to store 'replacement': *)
	  String.blit replacement 0 s_out (p_out + !k) l_repl;
	  l_repl  (* may be 0! *)
	end
	else 
	  (-1) (* Exit whole conversion *)
      end

    in
    if k_inc >= 0 then (
      k := !k + k_inc;
      incr n
    )
    else (
      n_ret := !n;
      n := n_max
    );
  done;
  if !n_ret >= 0 then (!n_ret - slice_pos, !k) 
                 else (!n - slice_pos,     !k)
;;


let write_utf16_lebe lo hi 
                     slice_char slice_pos slice_length s_out p_out l_out subst =
  (* lo=0, hi=1: little endian
   * lo=1, hi=0: big endian
   *)
  assert(p_out >= 0 && p_out + l_out <= String.length s_out && l_out >= 0);
  assert(slice_pos >= 0 && slice_pos+slice_length <= Array.length slice_char);

  let n = ref slice_pos in     (* index of slice *)
  let n_max = slice_pos + slice_length in

  let k = ref 0 in             (* written bytes *)
  let n_ret = ref (-1) in      (* returned number of characters *)

  while ( !n < n_max ) do
    let p = slice_char.( !n ) in

    let index = p_out + !k in

    let k_inc =
      if p >= 0xfffe then begin
	if p <= 0x10ffff then begin
	  if p <= 0xffff then failwith "Netconversion.write_utf16_le";
	  (* Must be written as surrogate pair *)
	  if !k + 3 < l_out then begin
	    let high = ((p - 0x10000) lsr 10) + 0xd800 in
	    let low  = (p land 0x3ff) + 0xdc00 in
	    s_out.[index + lo ] <- Char.chr (high land 0xff);
	    s_out.[index + hi ] <- Char.chr (high lsr 8);
	    s_out.[index + 2 + lo ] <- Char.chr (low land 0xff);
	    s_out.[index + 2 + hi ] <- Char.chr (low lsr 8);
	    4
	  end
	  else (-1)
	end
	else begin
	  (* Higher code points are not possible in XML; call subst *)
	  let replacement = subst p in
	  let l_repl =  String.length replacement in
	  if l_repl > multibyte_limit then
	    failwith "Netconversion.write_utf16_le: Substitution string too long";
	  if !k + l_repl <= l_out then begin
	    (* Enough space to store 'replacement': *)
	    String.blit replacement 0 s_out (p_out + !k) l_repl;
	    l_repl  (* may be 0! *)
	  end
	  else 
	    (-1) (* Exit whole conversion *)
	end
      end
      else begin
	(* 2-byte character *)
	if !k + 1 < l_out then begin
	  s_out.[index + lo ] <- Char.unsafe_chr (p land 0xff);
	  s_out.[index + hi ] <- Char.unsafe_chr ((p lsr 8) land 0xff);
	  2
	end
	else (-1)
      end
    in
    if k_inc >= 0 then (
      k := !k + k_inc;
      incr n
    )
    else (
      n_ret := !n;
      n := n_max
    );
  done;
  if !n_ret >= 0 then (!n_ret - slice_pos, !k) 
                 else (!n - slice_pos,     !k)
;;


let write_euc map enc =
  (* Code set 0 is US-ASCII.
   * let (set, byte1, byte2) = map unicode:
   *  - set is 1, 2, 3, or 4. 4 means that the code point cannot be mapped.
   *  - byte1 >= 160, <= 255
   *  - byte2 >= 160, <= 255, or byte2=256 meaning that it is not used
   *)
  (* UNSAFE_OPT *)

  fun slice_char slice_pos slice_length s_out p_out l_out subst ->
    assert(p_out >= 0 && p_out + l_out <= String.length s_out && l_out >= 0);
    assert(slice_pos >= 0 && slice_pos+slice_length <= Array.length slice_char);

    let n = ref slice_pos in     (* index of slice *)
    let n_max = slice_pos + slice_length in

    let k = ref 0 in             (* written bytes *)
    let n_ret = ref (-1) in      (* returned number of characters *)

    while ( !n < n_max ) do
      (* We know:
       * (1) !n >= 0, because it starts with 0 and is only increased
       * (2) !n < n_max = slice_pos + slice_length <= Array.length slice
       * ==> unsafe get ok
       *)
      let p = (* slice.( !n ) *)
              Array.unsafe_get slice_char !n in
      assert (p >= 0);

      let index = p_out + !k in

      let (set, b1, b2) = 
	if p <= 127 then (0, p, 256) else map p in

      let k_inc =
	(* k_inc: how many bytes are written *)
	match set with
	    0 ->
	      if !k < l_out then begin
		(* s_out.[index] <- Char.chr p; *)
		String.unsafe_set s_out index (Char.unsafe_chr (b1 land 127));
		1
	      end
	      else (-1)
	  | 1 ->
	      let bl = if b2 = 256 then 1 else 2 in
	      if !k + bl < l_out then begin
		assert(b1 >= 160 && b1 <= 255 && b2 >= 160 && b2 <= 256);
		s_out.[index] <- Char.chr b1;
		if b2 <> 256 then s_out.[index+1] <- Char.chr b2;
		bl
	      end
	      else (-1)
	  | 2 ->
	      let bl = if b2 = 256 then 2 else 3 in
	      if !k + bl < l_out then begin
		assert(b1 >= 160 && b1 <= 255 && b2 >= 160 && b2 <= 256);
		s_out.[index] <- '\142';
		s_out.[index+1] <- Char.chr b1;
		if b2 <> 256 then s_out.[index+2] <- Char.chr b2;
		bl
	      end
	      else (-1)
	  | 3 ->
	      let bl = if b2 = 256 then 2 else 3 in
	      if !k + bl < l_out then begin
		assert(b1 >= 160 && b1 <= 255 && b2 >= 160 && b2 <= 256);
		s_out.[index] <- '\143';
		s_out.[index+1] <- Char.chr b1;
		if b2 <> 256 then s_out.[index+2] <- Char.chr b2;
		bl
	      end
	      else (-1)
	  | 4 ->
	      let replacement = subst p in
	      let l_repl =  String.length replacement in
	      if l_repl > multibyte_limit then
		failwith "Netconversion.write_euc: Substitution string too long";
	      if !k + l_repl <= l_out then begin
		(* Enough space to store 'replacement': *)
		String.blit replacement 0 s_out (p_out + !k) l_repl;
		l_repl
	      end
	      else 
		(-1) (* Exit whole conversion *)
	  | _ ->
	      assert false
      in
      if k_inc >= 0 then (
	k := !k + k_inc;
	incr n
      )
      else (
	n_ret := !n;
	n := n_max
      );
    done;
    if !n_ret >= 0 then (!n_ret - slice_pos, !k) 
                   else (!n - slice_pos,     !k)
;;


let write_eucjp () =
  let jis0201 = Netmappings.get_from_unicode "jis0201" in
  let jis0208 = Netmappings.get_from_unicode "jis0208" in
  let jis0212 = Netmappings.get_from_unicode "jis0212" in

  let jis0201_mask = Array.length jis0201 - 1 in
  let jis0208_mask = Array.length jis0208 - 1 in
  let jis0212_mask = Array.length jis0212 - 1 in

  let map p =
    (* Try in order: jis0208, jis0201, jis0212 *)
    let map_tbl jistbl jistbl_mask =
      match jistbl.(p land jistbl_mask) with
	  Netmappings.U_nil -> -1
	| Netmappings.U_single (p0,q0) ->
	    if p0 = p then q0 else -1
	| Netmappings.U_double (p0,q0,p1,q1) ->
	    if p0 = p then q0 else
	      if p1 = p then q1 else -1
	| Netmappings.U_array pq ->
	    let r = ref (-1) in
	    let h = ref 0 in
	    while !r < 0 && !h < Array.length pq do
	      if pq.( !h ) = p then
		r := pq.( !h+1 )
	      else
		h := !h + 2
	    done;
	    !r
    in
    let cp_0208 = map_tbl jis0208 jis0208_mask in
    if cp_0208 >= 0 then
      let row = cp_0208 / 96 in
      let col = cp_0208 - row * 96 in
      (1, row + 160, col + 160)
    else
      let cp_0201 = map_tbl jis0201 jis0201_mask in
      if cp_0201 >= 128 then   (* Ignore especially 0x5c, 0x7e *)
	(2, cp_0201, 256)
      else
	let cp_0212 = map_tbl jis0212 jis0212_mask in
	if cp_0212 >= 0 then
	  let row = cp_0212 / 96 in
	  let col = cp_0212 - row * 96 in
	  (3, row + 160, col + 160)
	else
	  (4,256,256)
  in
  write_euc map `Enc_eucjp
;;


let write_euckr () =
  let ks1001 = Netmappings.get_from_unicode "ks1001" in

  let ks1001_mask = Array.length ks1001 - 1 in

  let map p =
    let map_tbl kstbl kstbl_mask =
      match kstbl.(p land kstbl_mask) with
	  Netmappings.U_nil -> -1
	| Netmappings.U_single (p0,q0) ->
	    if p0 = p then q0 else -1
	| Netmappings.U_double (p0,q0,p1,q1) ->
	    if p0 = p then q0 else
	      if p1 = p then q1 else -1
	| Netmappings.U_array pq ->
	    let r = ref (-1) in
	    let h = ref 0 in
	    while !r < 0 && !h < Array.length pq do
	      if pq.( !h ) = p then
		r := pq.( !h+1 )
	      else
		h := !h + 2
	    done;
	    !r
    in
    let cp_1001 = map_tbl ks1001 ks1001_mask in
    if cp_1001 >= 0 then
      let row = cp_1001 / 96 in
      let col = cp_1001 - row * 96 in
      (1, row + 160, col + 160)
    else
      (4,256,256)
  in
  write_euc map `Enc_euckr
;;


let special_cpoint = 0x110000;;


let write_subset inner_writer def 
                 slice_char slice_pos slice_length s_out p_out l_out subst =
  assert(p_out >= 0 && p_out + l_out <= String.length s_out && l_out >= 0);
  assert(slice_pos >= 0 && slice_pos+slice_length <= Array.length slice_char);

  (* Force that the subst' function is called for all undefined code
   * points
   *)

  let slice_char' = Array.sub slice_char slice_pos slice_length in
  for n = 0 to slice_length - 1 do
    let ch = slice_char'.(n) in
    if ch >= special_cpoint || not (def ch) then 
      slice_char'.(n) <- special_cpoint + n
  done;

  let subst' ch =
    if ch >= special_cpoint then
      subst (slice_char.(slice_pos + ch - special_cpoint))
    else
      subst ch
  in

  inner_writer slice_char' 0 slice_length s_out p_out l_out subst'
;;


let back_8bit s_in range_in p_in n_char =
  let p_rel = p_in - range_in in
  let n = min p_rel n_char in
  (n,n)
;;


let back_utf8 s_in range_in p_in n_char =
  let n = ref 0 in
  let k = ref 0 in
  let k_out = ref 0 in
  while p_in - !k > range_in && !n < n_char do
    incr k;
    let ch = Char.code s_in.[ p_in - !k ] in
    if ch < 0x80 || ( ch >= 0xc0 && ch <=0xfd) then ( incr n; k_out := !k )
  done;
  ( !n, !k_out )
;;


let back_utf16_lebe lo hi s_in range_in p_in n_char =
  (* lo=0, hi=1: little endian
   * lo=1, hi=0: big endian
   *)
  let n = ref 0 in
  let k = ref 0 in
  let k_out = ref 0 in
  while p_in - !k > range_in + 1 && !n < n_char do
    incr k;
    incr k;
    let ch = (Char.code s_in.[p_in - !k + lo]) lor 
	     ((Char.code s_in.[p_in - !k + hi]) lsl 8) in
    if ch < 0xdc00 || ch >= 0xe000 then ( incr n; k_out := !k );
    (* else: ch is the second half of a surrogate pair *)
  done;
  ( !n, !k_out )
;;


let back_euc s_in range_in p_in n_char =
  (* Works for 1-byte and 2-byte encodings *)
  let n = ref 0 in
  let k = ref 0 in
  let k_out = ref 0 in
  while p_in - !k > range_in && !n < n_char do 
    incr k;
    let ch1 = Char.code s_in.[ p_in - !k ] in
    if ch1 < 0x80 then (
      incr n; k_out := !k
    ) 
    else if p_in - !k > range_in then (
      incr k;
      let ch2 = Char.code s_in.[ p_in - !k ] in
      (* ch2 < 0x80: wrong, but we do not report errors here *)
      if ch2 < 0x80 then (
	incr n; k_out := !k
      ) 
      else if ch2 = 142 || ch2 = 143 then (
	incr n; k_out := !k
      )
      else if p_in - !k > range_in then (
	let ch3 = Char.code s_in.[ p_in - !k - 1 ] in
	if ch3 = 142 || ch3 = 143 then (
	  incr k; incr n; k_out := !k
	)
	else (
	  incr n; k_out := !k
	)
      )
      else (
	(* At the beginning of the string *)
	incr n; k_out := !k
      )
    )
  done;
  ( !n, !k_out )
;;


let check_unicode p =
  if p < 0 || (p > 0xd7ff && p < 0xe000) || p = 0xfffe || p = 0xffff || p > 0x10ffff then
    raise Malformed_code
;;


let rec to_unicode cs =
  match cs with
      `Set_iso88591 -> (fun p -> 
			  if p < 0 || p > 255 then raise Malformed_code;
			  p)
    | `Set_usascii  -> (fun p -> 
			  if p < 0 || p > 127 then raise Malformed_code;
			  p)
    | `Set_unicode  -> (fun p -> check_unicode p; p)
    | _ -> 
	let m_to_uni = Netmappings.get_to_unicode (internal_name cs) in
	(fun p ->
	   if p < 0 || p >= Array.length m_to_uni then raise Malformed_code;
	   let uni = m_to_uni.(p) in
	   if uni < 0 then raise Malformed_code;
	   uni
	)
;;


let rec from_unicode cs =
  match cs with
      `Set_iso88591 -> (fun p -> 
			  check_unicode p;
			  if p > 255 then raise (Cannot_represent p);
			  p)
    | `Set_usascii  -> (fun p -> 
			  check_unicode p;
			  if p > 127 then raise (Cannot_represent p);
			  p)
    | `Set_unicode  -> (fun p -> check_unicode p; p)
    | _ -> 
	let m_from_unicode = Netmappings.get_from_unicode (internal_name cs) in
	let m_mask = Array.length m_from_unicode - 1 in
	(fun p ->
	   check_unicode p;
	   let p' =
	     match Array.unsafe_get m_from_unicode (p land m_mask) with
		 Netmappings.U_nil -> -1
	       | Netmappings.U_single (p0,q0) ->
		   if p0 = p then q0 else -1
	       | Netmappings.U_double (p0,q0,p1,q1) ->
		   if p0 = p then q0 else
		     if p1 = p then q1 else -1
	       | Netmappings.U_array pq ->
		   let r = ref (-1) in
		   let h = ref 0 in
		   while !r < 0 && !h < Array.length pq do
		     if pq.( !h ) = p then
		       r := pq.( !h+1 )
		     else
		       h := !h + 2
		   done;
		   !r
	   in
	   if p' < 0 then raise(Cannot_represent p);
	   p'
	)
;;


type encoding1 =
  [ encoding | `Enc_utf16_bom ] ;;

  (* `Enc_utf16_bom considers the BOM as a character with code point -3.
   * This encoding is only internally used.
   *)

let rec get_reader1 (enc : encoding1) =
  (* get_reader1 supports the additional internal encodings of
   * encoding1. get_reader (below) only supports the exported
   * encodings.
   *)
  match enc with
      `Enc_iso88591 -> !read_iso88591_ref 255 `Enc_iso88591
    | `Enc_usascii  -> !read_iso88591_ref 127 `Enc_usascii
    | `Enc_empty    -> !read_iso88591_ref (-1) `Enc_empty
    | `Enc_utf8     -> !read_utf8_ref false
    | `Enc_java     -> !read_utf8_ref true
    | `Enc_utf16    -> read_utf16 false
    | `Enc_utf16_bom  -> read_utf16 true
    | `Enc_utf16_le -> read_utf16_lebe 0 1 0 `Enc_utf16_le
    | `Enc_utf16_be -> read_utf16_lebe 1 0 0 `Enc_utf16_be
    | `Enc_eucjp    -> read_eucjp ()
    | `Enc_euckr    -> read_euckr ()
    | `Enc_subset(e,def) ->
	let reader' = get_reader1 (e :> encoding1) in
	read_subset reader' def
    | #encoding as e -> 
	read_8bit (e :> encoding)
;;


let get_reader = 
  (get_reader1 : encoding1 -> 'a :> encoding -> 'a);;


let rec get_writer enc =
  match enc with
      `Enc_iso88591 -> write_iso88591 255
    | `Enc_usascii  -> write_iso88591 127
    | `Enc_empty    -> write_iso88591 (-1)
    | `Enc_utf8     -> write_utf8 false
    | `Enc_java     -> write_utf8 true
    | `Enc_utf16    -> failwith "Netconversion: Cannot output text as `Enc_utf16, use `Enc_utf16_le or `Enc_utf16_be"
    | `Enc_utf16_le -> write_utf16_lebe 0 1
    | `Enc_utf16_be -> write_utf16_lebe 1 0
    | `Enc_eucjp    -> write_eucjp ()
    | `Enc_euckr    -> write_euckr ()
    | `Enc_subset(e,def) ->
	let writer' = get_writer e in
	write_subset writer' def
    | _ -> 
	write_8bit enc
;;


let rec get_back_fn enc =
  match enc with
    | `Enc_utf8
    | `Enc_java     -> back_utf8
    | `Enc_utf16    -> failwith "Netconversion: Cannot go back in text encoded as `Enc_utf16, use `Enc_utf16_le or `Enc_utf16_be"
    | `Enc_utf16_le -> back_utf16_lebe 0 1
    | `Enc_utf16_be -> back_utf16_lebe 1 0
    | `Enc_eucjp    -> back_euc
    | `Enc_euckr    -> back_euc
    | `Enc_subset(e,def) ->
	get_back_fn e
    | _ -> 
	back_8bit
;;


let recode ~in_enc
           ~in_buf
	   ~in_pos
	   ~in_len
	   ~out_enc
	   ~out_buf
           ~out_pos
	   ~out_len
	   ~max_chars
	   ~subst =
  if (in_pos < 0  || in_len < 0  || in_pos  + in_len  > String.length in_buf ||
      out_pos < 0 || out_len < 0 || out_pos + out_len > String.length out_buf)
  then
    invalid_arg "Netconversion.recode";

  (* An array with 250 elements can be allocated in the minor heap. *)

  let slice_length = big_slice  in
  let slice_char = Array.make slice_length (-1) in
  let slice_blen = Array.make slice_length 1 in

  let in_k = ref 0 in     (* read bytes *)
  let in_n = ref 0 in     (* read characters *)
  let in_eof = ref (!in_k >= in_len) in
  let out_k = ref 0 in    (* written bytes *)
  let out_n = ref 0 in    (* written characters *)
  let out_eof = ref (!out_k >= out_len || !out_n >= max_chars) in

  let rd_enc = ref in_enc in
  let reader = ref (get_reader in_enc) in
  let writer = get_writer out_enc in

  while not !in_eof && not !out_eof do
    let in_n_inc, in_k_inc, rd_enc' =
      try
	!reader slice_char slice_blen in_buf (in_pos + !in_k) (in_len - !in_k) 
      with
	  Malformed_code_read(in_n_inc, in_k_inc, rd_enc') ->
	    if in_n_inc = 0 then raise Malformed_code;
	    (in_n_inc, in_k_inc, rd_enc')
    in

    let out_n_inc_max = min in_n_inc (max_chars - !out_n) in
        (* do not write more than max_chars *)
    let out_n_inc, out_k_inc =
      if out_n_inc_max > 0 then
	writer 
	  slice_char 0 out_n_inc_max out_buf (out_pos + !out_k) 
	  (out_len - !out_k) subst
      else
	(0,0)
    in

    let in_n_inc', in_k_inc' =
      if in_n_inc > out_n_inc then begin
	(* Not all read characters could be written *)
	let sum = ref 0 in
	for j = 0 to out_n_inc - 1 do
	  sum := !sum + slice_blen.(j)
	done;
	(out_n_inc, !sum)
      end
      else
	(in_n_inc, in_k_inc)
    in

    in_k := !in_k + in_k_inc';
    in_n := !in_n + in_n_inc';
    out_k := !out_k + out_k_inc;
    out_n := !out_n + out_n_inc;

    (* Detect change of input encoding: *)

    if rd_enc' <> !rd_enc then begin
      rd_enc := rd_enc';
      reader := get_reader rd_enc';
      Array.fill slice_blen 0 slice_length 1;
    end;

    (* EOF criteria:
     * - It is possible that !in_k never reaches in_len because there is a
     *   multibyte character at the end that is partially outside the input
     *   range
     * - For the same reason it is possible that !out_k never reaches out_len
     * - It is accepted as reader EOF if not even one character can be
     *   scanned
     * - It is accepted as writer EOF if fewer than in_n_inc characters
     *   could be written
     *)

    in_eof := 
      (!in_k >= in_len || in_n_inc = 0);

    out_eof := 
      (!out_k >= out_len || !out_n >= max_chars || out_n_inc < in_n_inc);

  done;

  ( !in_k, !out_k, !rd_enc )
;;


let rec ustring_of_uchar enc =
  let multi_byte writer n p =
    let s = String.create n in
    let _,n_act = writer [|p|] 0 1 s 0 n 
		    (fun _ -> raise (Cannot_represent p)) in
    String.sub s 0 n_act
  in
  match enc with
      `Enc_iso88591 -> 
	(fun p ->
	   if p > 255 then raise (Cannot_represent p);
	   String.make 1 (Char.chr p))
    | `Enc_usascii ->
	(fun p ->if p > 127 then raise (Cannot_represent p);
	   String.make 1 (Char.chr p))
    | `Enc_utf8 -> multi_byte (write_utf8 false) 4
    | `Enc_java -> multi_byte (write_utf8 true) 4
    | `Enc_utf16_le -> multi_byte (write_utf16_lebe 0 1) 4
    | `Enc_utf16_be -> multi_byte (write_utf16_lebe 1 0) 4
    | `Enc_utf16 ->
	invalid_arg "Netconversion.ustring_of_uchar: UTF-16 not possible"
    | `Enc_eucjp -> multi_byte (write_eucjp()) 3
    | `Enc_euckr -> multi_byte (write_euckr()) 2
    | `Enc_subset(e,def) ->
	(fun p -> if def p then ustring_of_uchar e p else raise (Cannot_represent p))
    | _ ->
	let writer = write_8bit enc in
	multi_byte writer 1
;;


let makechar enc =
  let us = ustring_of_uchar enc in
  (fun p -> try us p with Cannot_represent _ -> raise Not_found)
;;


(* The following algorithms assume that there is an upper limit of the length
 * of a multibyte character. Currently, UTF8 is the encoding with the longest
 * multibyte characters (6 bytes).
 * Because of this limit, it is allowed to allocate a buffer that is "large
 * enough" in order to ensure that at least one character is recoded in every
 * loop cycle. If the buffer was not large enough, no character would be
 * processed in a cycle, and the algorithm would hang.
 *)

let convert ?(subst = (fun p -> raise (Cannot_represent p))) 
            ~in_enc ~out_enc ?(range_pos=0) ?range_len s =

  let range_len = 
    match range_len with
	Some l -> l
      | None -> String.length s - range_pos in

  if range_pos < 0 || range_len < 0 || range_pos+range_len > String.length s
  then invalid_arg "Netconversion.convert";

  (* Estimate the size of the output string: 
   * length * 2 is just guessed. It is assumed that this number is usually
   * too large, and to avoid that too much memory is wasted, the buffer is
   * limited by 10000.
   *)
  let size = ref (max multibyte_limit (min 10000 (range_len * 2))) in
  let out_buf = ref (String.create !size) in

  let k_in = ref 0 in
  let k_out = ref 0 in

  while !k_in < range_len do
    let in_len = range_len - !k_in in
    let out_len = !size - !k_out in
    assert (out_len >= multibyte_limit);  (* space for at least one char *)
    let k_in_inc, k_out_inc, in_enc' =
      recode ~in_enc   ~in_buf:s           ~in_pos:(range_pos + !k_in) ~in_len
             ~out_enc  ~out_buf:(!out_buf) ~out_pos:(!k_out)           ~out_len
             ~max_chars:max_int            ~subst in
    if k_in_inc = 0 then raise Malformed_code;
    (* Reasons for k_in_inc = 0:
     * (1) There is not enough space in out_buf to add a single character
     * (2) in_buf ends with a prefix of a multi-byte character
     * Because there is always space for at least one character
     * ( = multibyte_limit ), reason (1) can be excluded. So it must
     * be (2), and we can raise Malformed_code.
     *)
    k_in  := !k_in  + k_in_inc;
    k_out := !k_out + k_out_inc;
    (* double the size of out_buf: *)
    let size' = min Sys.max_string_length (!size + !size) in
    if size' < !size + multibyte_limit then 
      failwith "Netconversion.convert: string too long";
    let out_buf' = String.create size' in
    String.blit !out_buf 0 out_buf' 0 !k_out;
    out_buf := out_buf';
    size := size';
  done;

  String.sub !out_buf 0 !k_out
;;


let recode_string ~in_enc ~out_enc
                  ?(subst = (fun p -> raise Not_found)) s =
  convert ~subst ~in_enc ~out_enc s
;;


class conversion_pipe ?(subst = (fun p -> raise (Cannot_represent p))) 
                    ~in_enc ~out_enc () =
  let current_in_enc = ref in_enc in
  let conv in_netbuf at_eof out_netbuf =
    if at_eof then
      (* TODO: avoid the extra allocations *)
      let s = recode_string 
	        ~subst ~in_enc:!current_in_enc ~out_enc
	        (Netbuffer.contents in_netbuf) in
      Netbuffer.add_string out_netbuf s
    else
      let in_buf = Netbuffer.unsafe_buffer in_netbuf in
      let in_pos = 0 in
      let in_len = Netbuffer.length in_netbuf in
      let n =
	Netbuffer.add_inplace 
	  out_netbuf
	  (fun out_buf out_pos out_len ->
	     let (in_n,out_n,in_enc') =
	       recode
	         ~in_enc:!current_in_enc ~in_buf ~in_pos ~in_len
	         ~out_enc ~out_buf ~out_pos ~out_len
	         ~max_chars:out_len
	         ~subst
	     in
	     Netbuffer.delete in_netbuf 0 in_n;
	     current_in_enc := in_enc';
	     out_n
	  )
      in
      if n = 0 && in_len > 0 then begin
	(* To avoid endless loops, ensure here that there is enough space
	 * in out_netbuf
	 *)
	ignore(Netbuffer.add_inplace 
	       ~len:multibyte_limit
		 out_netbuf
		 (fun _ _ _ -> 0));
      end;
      ()
  in
  Netchannels.pipe ~conv ()
;;


class recoding_pipe ?(subst = (fun p -> raise Not_found)) 
                    ~in_enc ~out_enc () =
  conversion_pipe ~subst ~in_enc ~out_enc ()
;;


(**********************************************************************)
(* Cursors                                                            *)
(**********************************************************************)

(* The "read_*" functions are used to scan the string, and to move
 * the cursor forward. The slice array stores the scanned characters.
 * If the read call raises Malformed_code, the size of the slice
 * is decreased to 1, so the exact position can be calculated.
 *)

(* Notes UTF-16:
 * 
 * cursor_enc is updated after the first slice has been read. This
 * usually changes this field to either the big or little endian
 * encoding variant. No update is needed when previous slices are
 * scanned, because the BOM is only allowed at the beginning of the
 * string, so can at most go back to exactly the BOM.
 *
 * cursor_encoding returns `Enc_utf16 when the cursor is over the
 * BOM, and cursor_enc otherwise.
 *)

exception End_of_string;;
exception Cursor_out_of_range;;
exception Partial_character;;
exception Byte_order_mark;;


type cursor =
    { (* configuration: *)
      mutable cursor_target : string;
      mutable cursor_range_pos : int;
      mutable cursor_range_len : int;
      mutable cursor_offset : int;
      mutable cursor_enc : encoding;
        (* `Enc_utf16: Only used if the slice or string is too short to
         *    recognize the endianess. Otherwise, the encoding is set
         *    to the endian-aware variant.
         *)
      mutable cursor_has_bom : bool;
        (* Whether there is a BOM. Only when initially cursor_enc=`Enc_utf16 *)
      (* conversion: *)
      mutable cursor_slice_char : int array;
        (* Contains the characters of the slice. Special values:
	 * -1: EOF
	 * -2: Incomplete multi-byte character at end
	 * -3: BOM at the beginning
	 *)
      mutable cursor_slice_blen : int array;
        (* Contains the byte length of the characters.
	 * Recall that this array must contain 1s when single-byte
	 * encodings are scanned, and must not be modified. The
	 * "reader" does not fill this array for single-byte encodings,
	 * so modifications would persist!
	 *)
      mutable cursor_imbchar_len : int;
        (* This is always 0, except in the special case when the first
	 * character of this slice is EOF, and EOF is preceded by an
	 * incomplete multi-byte character (imbchar). In this case,
	 * the length of the imbchar is stored here.
	 *)
      mutable cursor_slice_char_pos : int;
        (* char pos of the beginning of the slice *)
      mutable cursor_slice_byte_pos : int;
        (* byte pos of the beginning of the slice *)
      mutable cursor_slice_length : int;
        (* number of characters *)
      mutable cursor_slice_bytes : int;
        (* number of bytes *)
      (* bookkeeping: *)
      mutable cursor_char_pos : int;
      mutable cursor_byte_pos : int;
      mutable cursor_index : int;     (* index in the slice array *)
      mutable cursor_problem_range_start : int;
      mutable cursor_problem_range_end : int;
      (* The character positions >= range_start and < range_end are
       * considered as problem range. It is known that there is a
       * coding error (Malformed_code), so slices with length 1 must
       * be used.
       *)
      (* methods: *)
      mutable load_next_slice : unit -> unit;   
        (* Precondition: cursor is one char right of the end of the slice
	 * Action: load the next slice
	 * Postcondition: cursor is at the beginning of the new slice
	 *)
      mutable load_prev_slice : unit -> unit;  
        (* Precondition: cursor is at the beginning of current slice 
	 * Action: load the previous slice
	 * Postcondition: the cursor is at the end of the new slice
	 *                or the function raises Cursor_out_of_range
	 * Note that this function actually moves the cursor one character
	 * back (in contrast to load_next_slice that only reloads the
	 * slice array, but does not move the cursor). The function may
	 * choose to allocate a new, shorter slice array.
	 *)
    }
;;


let cursor_target cs = cs.cursor_target;;

let cursor_range cs = (cs.cursor_range_pos, cs.cursor_range_len);;

let cursor_initial_rel_pos cs = cs.cursor_offset;;

let cursor_char_count cs = cs.cursor_char_pos;;

let cursor_pos cs = cs.cursor_byte_pos;;

let cursor_encoding cs = 
  let enc = cs.cursor_enc in
  match enc with
    ( `Enc_utf16_le
    | `Enc_utf16_be ) when cs.cursor_has_bom ->
	if cs.cursor_byte_pos = cs.cursor_range_pos then
	  `Enc_utf16
	else
	  enc
    | _ ->
	enc
;;


exception Failing_in_Netconversion_uchar_at

let uchar_at cs =
  let ch = cs.cursor_slice_char.(cs.cursor_index) in
  if ch < 0 then
    match ch with
	-1 -> raise End_of_string
      | -2 -> raise Partial_character
      | -3 -> raise Byte_order_mark
      | _  -> (* assert false *) raise Failing_in_Netconversion_uchar_at
	      (* "assert false" isn't inlined! *)
  else
    ch
;;


let cursor_byte_length cs =
  let ch = cs.cursor_slice_char.(cs.cursor_index) in
  if ch = -1 then
    raise End_of_string
  else
    cs.cursor_slice_blen.(cs.cursor_index)
;;



let cursor_at_end cs =
  let ch = cs.cursor_slice_char.(cs.cursor_index) in
  ch = (-1)
;;

let move_right num cs =
  let rec go num =
    let sl = Array.length cs.cursor_slice_char in
    if num >= sl - cs.cursor_index then begin
      (* Case: go at least to the next slice *)

      (* If the current slice is not completely filled, we will be 
       * definitely outside of the valid range
       *)
      if cs.cursor_slice_length < sl then begin
	(* move to rightmost position, and raise the approriate exception *)
	cs.cursor_byte_pos <- cs.cursor_slice_byte_pos + cs.cursor_slice_bytes;
	cs.cursor_char_pos <- cs.cursor_slice_char_pos + cs.cursor_slice_length;
	cs.cursor_index    <- cs.cursor_slice_length;
	assert(cs.cursor_slice_char.(cs.cursor_index) = (-1));
	raise Cursor_out_of_range;
      end;
      assert(cs.cursor_slice_length = sl);
      let n = sl - cs.cursor_index in
      cs.cursor_byte_pos <- cs.cursor_slice_byte_pos + cs.cursor_slice_bytes;
      cs.cursor_char_pos <- cs.cursor_slice_char_pos + cs.cursor_slice_length;
      cs.cursor_index    <- sl;
      cs.load_next_slice();     (* may raise Malformed_code *)
      go (num - n);
    end
    else begin
      (* Case: do not leave this slice *)
      let bl_sum = ref 0 in
      for k = cs.cursor_index to cs.cursor_index + num - 1 do
	let bl = cs.cursor_slice_blen.(k) in
	if k >= cs.cursor_slice_length then begin
	  (* Cursor is beyond EOF *)
	  cs.cursor_byte_pos <- cs.cursor_byte_pos + !bl_sum;
	  cs.cursor_char_pos <- cs.cursor_char_pos + (k - cs.cursor_index);
	  cs.cursor_index    <- k;
	  raise Cursor_out_of_range
	end;
	bl_sum := !bl_sum + bl
      done;
      cs.cursor_byte_pos <- cs.cursor_byte_pos + !bl_sum;
      cs.cursor_char_pos <- cs.cursor_char_pos + num;
      cs.cursor_index    <- cs.cursor_index + num;
    end
  in
  assert(num >= 0);
  try
    go num
  with
      Malformed_code ->
	(* This happens when load_next_slice fails to decode the next slice
	 * of length 1. In this case, load_next_slice keeps the state of
	 * the cursor, so we have the chance to correct it now.
	 *)
	cs.cursor_index    <- cs.cursor_index - 1;
	cs.cursor_char_pos <- cs.cursor_char_pos - 1;
	cs.cursor_byte_pos <- cs.cursor_byte_pos - 
	                      cs.cursor_slice_blen.(cs.cursor_index);
	raise Malformed_code
;;

let move_left num cs =
  let rec go num =
    if num > cs.cursor_index then begin
      let n = cs.cursor_index in
      cs.cursor_byte_pos <- cs.cursor_slice_byte_pos;
      cs.cursor_char_pos <- cs.cursor_slice_char_pos;
      cs.cursor_index    <- 0;
      (* cursor is now at the beginning of the slice *)
      cs.load_prev_slice();   (* go another character back *)
      go (num-n-1)            (* so we went n+1 characters in this round *)
    end
    else begin
      (* num <= cs.cursor_index *)
      let bl_sum = ref 0 in
      let n = cs.cursor_index - num in
      for k = cs.cursor_index - 1 downto n do
	bl_sum := !bl_sum + cs.cursor_slice_blen.(k)
      done;
      cs.cursor_byte_pos <- cs.cursor_byte_pos - !bl_sum;
      cs.cursor_char_pos <- cs.cursor_char_pos - num;
      cs.cursor_index    <- n;
    end
  in
  assert(num < 0);
  go (-num)
;;


let move ?(num = 1) cs =
  if num >= 0 then
    move_right num cs
  else
    move_left num cs
;;


let init_load_slice cs enc =

  let reader0 = get_reader enc in
  let back0   = lazy(get_back_fn enc) in
  (* For most encodings, [reader] and [back] never change.
   * For UTF-16, there may be refinements, however.
   *)

  let reader() =
    match cs.cursor_enc with
      ( `Enc_utf16
      | `Enc_utf16_le
      | `Enc_utf16_be ) when cs.cursor_has_bom ->
	  (* Ensure we use `Enc_utf16_bom when we read the beginning
	   * of the range
	   *)
	  (fun slice_char slice_blen s bp bl ->
	     if bp = cs.cursor_range_pos then
	       get_reader1 `Enc_utf16_bom slice_char slice_blen s bp bl
	     else
	       get_reader cs.cursor_enc slice_char slice_blen s bp bl
	  )
      | _ ->
	  reader0
  in

  let back() =
    match cs.cursor_enc with
      ( `Enc_utf16
      | `Enc_utf16_le
      | `Enc_utf16_be ) when cs.cursor_has_bom ->
	  get_back_fn cs.cursor_enc
      | _ ->
	  Lazy.force back0
  in

  let record_imbchar rd_chars rd_bytes scan_bytes =
    (* Put a (-2) at position rd_chars of the current slice *)
    cs.cursor_slice_char.(rd_chars) <- (-2);
    cs.cursor_slice_blen.(rd_chars) <- scan_bytes - rd_bytes;
    if rd_chars+1 < Array.length cs.cursor_slice_char then (
      cs.cursor_slice_char.(rd_chars+1) <- (-1);
    );
    cs.cursor_slice_length <- rd_chars+1;
    cs.cursor_slice_bytes <- scan_bytes;
  in

  let repair_slice slice_size =
    (* When the reader raises Malformed_code, the slice has been
     * modified. This function reloads the old slice again.
     *
     * This function repairs these fields from the other fields:
     * - cursor_slice_char
     * - cursor_slice_blen
     * - cursor_slice_length
     * - cursor_slice_bytes
     *)
    let bp = cs.cursor_slice_byte_pos in
    let ep = cs.cursor_range_pos + cs.cursor_range_len in
    let (slice_char, slice_blen) =
      (Array.create slice_size (-1), Array.create slice_size 1) in
    let rd_chars, rd_bytes, _ =
      try
	reader 
          () slice_char slice_blen cs.cursor_target bp (ep-bp) 
      with
	  (* should not happen: *)
	  Malformed_code_read(_,_,_) -> raise Malformed_code
    in
    cs.cursor_slice_length <- rd_chars;
    cs.cursor_slice_bytes <- rd_bytes;
    cs.cursor_slice_char <- slice_char;
    cs.cursor_slice_blen <- slice_blen;
    (* Check for imbchars: *)
    if rd_chars < slice_size && rd_bytes < ep-bp then (
      record_imbchar rd_chars rd_bytes (ep-bp);
    );
  in

  let load_next_slice() =
    let cp = cs.cursor_char_pos in
    let bp = cs.cursor_byte_pos in
    let ep = cs.cursor_range_pos + cs.cursor_range_len in
    let load slice_size =
      let old_partial_len =
	(* Handle the case that the last character is (-2), and thus the first
	 * character of the next slice will be (-1). Then, cursor_imbchar_len
	 * must be set to the length of the (-2) character.
	 *)
	if cs.cursor_slice_char.(cs.cursor_slice_length-1) = (-2) then
	  cs.cursor_slice_blen.(cs.cursor_slice_length-1)
	else
	  0
      in
      (* Check if the current array can be reused, or if we need new
       * arrays with different sizes
       *)
      let (slice_char, slice_blen) =
	if slice_size = Array.length cs.cursor_slice_char then 
	  (cs.cursor_slice_char, cs.cursor_slice_blen)
	  (* use old arrays again *)
	else
	  (Array.create slice_size (-1), Array.create slice_size 1)
	  (* create new arrays with different size *)
      in
      (* Use the reader to decode the bytes and to put the characters into
       * slice_char. 
       *)
      let rd_chars, rd_bytes, enc' =
	reader () slice_char slice_blen cs.cursor_target bp (ep-bp) in
        (* may raise Malformed_code_read *)
      (* Update cursor record: *)
      cs.cursor_index <- 0;
      cs.cursor_slice_char <- slice_char;
      cs.cursor_slice_blen <- slice_blen;
      cs.cursor_slice_length <- rd_chars;
      cs.cursor_slice_bytes <- rd_bytes;
      cs.cursor_slice_char_pos <- cp;
      cs.cursor_slice_byte_pos <- bp;
      cs.cursor_imbchar_len <- old_partial_len;
      cs.cursor_enc <- enc';
      (* Check for imbchars: *)
      if rd_chars < slice_size && rd_bytes < ep-bp then (
	record_imbchar rd_chars rd_bytes (ep-bp);
      );
    in
    (* Is the cursor positioned in a problem range? If yes, decode only
     * one character. If not, try to decode a block of characters.
     * If the latter fails, the current position turns out to be
     * problematic, and is remembered as such.
     *)
    let old_slice_size = Array.length cs.cursor_slice_char in
    if cp >= cs.cursor_problem_range_start && 
       cp < cs.cursor_problem_range_end then begin
	 try
	   load 1
	 with
	     Malformed_code_read(_,_,_) ->
	       repair_slice old_slice_size; 
	       raise Malformed_code
    end
    else begin
      try load big_slice
      with
	  Malformed_code_read(_,_,_) ->
	    cs.cursor_problem_range_start <- cp;
	    cs.cursor_problem_range_end   <- cp+big_slice;
	    try
	      load 1
	    with
		Malformed_code_read(_,_,_) ->
		  repair_slice old_slice_size; 
		  raise Malformed_code
    end
  in

  let load_prev_slice() =
    let cp = cs.cursor_char_pos in
    let bp = cs.cursor_byte_pos in
    let ep = cs.cursor_range_pos + cs.cursor_range_len in
    let load slice_size =
      (* Check if the current array can be reused, or if we need new
       * arrays with different sizes
       *)
      let (slice_char, slice_blen) =
	if slice_size = Array.length cs.cursor_slice_char then 
	  (cs.cursor_slice_char, cs.cursor_slice_blen)
	  (* use old arrays again *)
	else
	  (Array.create slice_size (-1), Array.create slice_size 1)
	  (* create new arrays with different size *)
      in
      (* Go back 1 character (must always succeed): *)
      if bp = cs.cursor_range_pos then raise Cursor_out_of_range;      
      let bk1_chars, bk1_bytes =
	if cs.cursor_imbchar_len > 0 then
	  (* Special case: the last character of this slice is an imbchar.
	   * Assume imbchar_len 
	   *)
	  (1, cs.cursor_imbchar_len)
	else
	  back 
	    () cs.cursor_target cs.cursor_range_pos bp 1 in
      if bk1_chars = 0 then raise Malformed_code;
      (* bk1_chars = 0: this means there is a multi-byte suffix at the
       * beginning of the range
       * ==> bk1_chars = 1
       *)
      (* Go back further slice_size-1 characters (or less): *)
      let bk_chars, bk_bytes =
	back 
	  () cs.cursor_target cs.cursor_range_pos (bp-bk1_bytes) (slice_size-1) in
      let bp' = bp - bk1_bytes - bk_bytes in
      (* Use the reader to decode the bytes and to put the characters into
       * slice_char. 
       *)
      let rd_chars, rd_bytes, _ =
	reader () slice_char slice_blen cs.cursor_target bp' (ep-bp') in
        (* may raise Malformed_code_read *)
      assert(rd_chars >= bk_chars);
      (* Update cursor record: *)
      cs.cursor_index <- bk_chars;
      cs.cursor_slice_char <- slice_char;
      cs.cursor_slice_blen <- slice_blen;
      cs.cursor_slice_length <- rd_chars;
      cs.cursor_slice_bytes <- rd_bytes;
      cs.cursor_slice_char_pos <- cp - bk_chars - 1;
      cs.cursor_slice_byte_pos <- bp';
      cs.cursor_imbchar_len <- 0; (* Cannot happen *)
      (* Don't need to update cursor_enc! *)
      (* Check for imbchars: *)
      if rd_chars < slice_size && rd_bytes < ep-bp' then (
	record_imbchar rd_chars rd_bytes (ep-bp');
      );
      (* Implicitly go one character back: *)
      cs.cursor_char_pos <- cp - 1;
      cs.cursor_byte_pos <- bp - bk1_bytes;
    in
    (* Is the cursor positioned in a problem range? If yes, decode only
     * one character. If not, try to decode a block of characters.
     * If the latter fails, the current position turns out to be
     * problematic, and is remembered as such.
     *)
    let old_slice_size = Array.length cs.cursor_slice_char in
    if cp > cs.cursor_problem_range_start && 
       cp <= cs.cursor_problem_range_end then begin
	 try
	   load 1
	 with
	     Malformed_code_read(_,_,_) ->
	       repair_slice old_slice_size;
	       raise Malformed_code
    end
    else begin
      try load big_slice
      with
	  Malformed_code_read(_,_,_) ->
	    cs.cursor_problem_range_start <- cp-big_slice;
	    cs.cursor_problem_range_end   <- cp;
	    try
	      load 1
	    with
		Malformed_code_read(_,_,_) ->
		  repair_slice old_slice_size;
		  raise Malformed_code
    end
  in

  (* Important note: These two functions either modify the cursor state
   * as requested, or they raise an exception, and keep the cursor state
   * as before. Considered exceptions are Malformed_code, and
   * Cursor_out_of_range.
   *)

  cs.load_next_slice <- load_next_slice;
  cs.load_prev_slice <- load_prev_slice
;;


let create_cursor ?(range_pos = 0) ?range_len ?(initial_rel_pos = 0) enc s =
  if range_pos < 0 || range_pos > String.length s then
    invalid_arg "Netconversion.create_cursor";
  
  let range_len =
    match range_len with
	Some l -> l
      | None   -> String.length s - range_pos in
  if range_len < 0 || range_pos + range_len > String.length s then
    invalid_arg "Netconversion.create_cursor";

  if initial_rel_pos < 0 || initial_rel_pos > range_len then
    invalid_arg "Netconversion.create_cursor";

  if enc = `Enc_utf16 && initial_rel_pos <> 0 then
    failwith "Netconversion.create_cursor: The encoding `Enc_utf16 only supported when initial_rel_pos=0";

  let cs =
    { cursor_target = s;
      cursor_range_pos = range_pos;
      cursor_range_len = range_len;
      cursor_offset = initial_rel_pos;
      cursor_enc = enc;
      cursor_has_bom = (enc = `Enc_utf16);
      cursor_slice_char = [| 1 |];
      cursor_slice_blen = [| 1 |];
      cursor_imbchar_len = 0;
      cursor_slice_char_pos = 0;
      cursor_slice_byte_pos = range_pos + initial_rel_pos;
      cursor_slice_length = 1;
      cursor_slice_bytes = 0;
      cursor_char_pos = 0;
      cursor_byte_pos = range_pos + initial_rel_pos;
      cursor_index = 1;
      cursor_problem_range_start = max_int;
      cursor_problem_range_end = max_int;
      load_next_slice = (fun () -> assert false);
      load_prev_slice = (fun () -> assert false);
    } in

  init_load_slice cs enc;
  
  (* load the next slice to do the rest of the initialization: *)
  cs.load_next_slice();

  cs
;;


let reinit_cursor ?(range_pos = 0) ?range_len ?(initial_rel_pos = 0) ?enc s cs =
  if range_pos < 0 || range_pos > String.length s then
    invalid_arg "Netconversion.reinit_cursor";
  
  let range_len =
    match range_len with
	Some l -> l
      | None   -> String.length s - range_pos in
  if range_len < 0 || range_pos + range_len > String.length s then
    invalid_arg "Netconversion.reinit_cursor";

  if initial_rel_pos < 0 || initial_rel_pos > range_len then
    invalid_arg "Netconversion.reinit_cursor";

  let enc = 
    match enc with
	None -> cs.cursor_enc
      | Some e -> e
  in
  if enc = `Enc_utf16 && initial_rel_pos <> 0 then
    failwith "Netconversion.reinit_cursor: The encoding `Enc_utf16 only supported when initial_rel_pos=0";

  let old_enc = cs.cursor_enc in

  cs.cursor_target <- s;
  cs.cursor_range_pos <- range_pos;
  cs.cursor_range_len <- range_len;
  cs.cursor_offset <- initial_rel_pos;
  cs.cursor_enc <- enc;
  cs.cursor_has_bom <- (enc = `Enc_utf16);
  cs.cursor_imbchar_len <- 0;
  cs.cursor_slice_char_pos <- 0;
  cs.cursor_slice_byte_pos <- range_pos + initial_rel_pos;
  cs.cursor_slice_length <- 1;
  cs.cursor_slice_bytes <- 0;
  cs.cursor_char_pos <- 0;
  cs.cursor_byte_pos <- range_pos + initial_rel_pos;
  cs.cursor_index <- 1;
  cs.cursor_problem_range_start <- max_int;
  cs.cursor_problem_range_end <- max_int;
  cs.load_next_slice <- (fun () -> assert false);
  cs.load_prev_slice <- (fun () -> assert false);
  
  cs.cursor_slice_char.(0) <- 1;
  if not (same_encoding enc old_enc) then
    (* slice_blen: It might have happened that the new encoding is an
     * 8 bit charset, but the old was not. Re-initialize this array
     * to ensure it contains only "1" in this case.
     *)
    Array.fill cs.cursor_slice_blen 0 (Array.length cs.cursor_slice_blen) 1;

  init_load_slice cs enc;
  
  (* load the next slice to do the rest of the initialization: *)
  cs.load_next_slice();

;;


let copy_cursor ?enc cs =
  let enc = 
    match enc with
	None -> cs.cursor_enc
      | Some e -> e
  in

  if same_encoding enc cs.cursor_enc then
    { cs with
	cursor_slice_char = Array.copy cs.cursor_slice_char;
	cursor_slice_blen = Array.copy cs.cursor_slice_blen;
    }
  else begin
    if enc = `Enc_utf16 then
      failwith "Netconversion.copy_cursor: The encoding `Enc_utf16 is not supported";
    let cs' =
      { cs with
	  cursor_enc = enc;
	  cursor_slice_char = [| 1 |];
	  cursor_slice_blen = [| 1 |];
	  cursor_slice_length = 1;
	  cursor_problem_range_start = max_int;
	  cursor_problem_range_end = max_int;
      } in
    init_load_slice cs' enc;
  
    (* load the next slice to do the rest of the initialization: *)
    cs'.load_next_slice();

    cs'
  end
;;


let cursor_blit_maxlen cs = 
  let l = cs.cursor_slice_length - cs.cursor_index in
  (* Test on special situations: *)
  match cs.cursor_slice_char.(cs.cursor_index) with
      -1 -> (* EOF *) raise End_of_string
    | -3 -> (* BOM *) 0
    | _ ->
	if cs.cursor_slice_char.(cs.cursor_slice_length - 1) = (-2) then
	  (* Partial character *)
	  l-1
	else
	  l
;;


let cursor_blit cs ua pos len =
  if pos < 0 || len < 0 || pos+len > Array.length ua then
    invalid_arg "Netconversion.cursor_blit";
  let cs_len = cursor_blit_maxlen cs in
  let l = min cs_len len in
  int_blit cs.cursor_slice_char cs.cursor_index ua pos l;
  l
;;


let cursor_blit_positions cs ua pos len =
  if pos < 0 || len < 0 || pos+len > Array.length ua then
    invalid_arg "Netconversion.cursor_blit_positions";
  let cs_len = cursor_blit_maxlen cs in
  let l = min cs_len len in
  let p = cs.cursor_byte_pos in
  let blen = cs.cursor_slice_blen in
  let cidx = cs.cursor_index in
  assert(pos+l <= Array.length ua);
  assert(cidx+l <= Array.length cs.cursor_slice_blen);
  Netaux.ArrayAux.int_series blen cidx ua pos l p;
  l
;;


(**********************************************************************)
(* String functions                                                   *)
(**********************************************************************)

(* CHECK
 * - ustring_length: Count imbchars? No!
 *
 * DOC:
 * - imbchar handling (additional exceptions)
 *)


let ustring_length enc =
  if is_single_byte enc then
    fun ?(range_pos=0) ?range_len s ->
      let range_len = 
	match range_len with
	    None -> String.length s - range_pos
	  | Some l -> l in
      if range_pos < 0 || range_len < 0 || range_pos+range_len > String.length s
      then invalid_arg "Netconversion.ustring_length";
      range_len
  else
    fun ?range_pos ?range_len s ->
      (* Assumption: There is no string that has more than max_int
       * characters
       *)
      let cs = create_cursor ?range_pos ?range_len enc s in
      ( try move ~num:max_int cs with Cursor_out_of_range -> ());
      let n = cursor_char_count cs in
      (* Check that the last char is not an imbchar *)
      ( try
	  move ~num:(-1) cs;
	  let _ = uchar_at cs in ()
	with
	    Cursor_out_of_range -> ()
	  | Partial_character -> raise Malformed_code
      );
      n
;;


exception Malformed_code_at of int;;

let verify enc ?range_pos ?range_len s =
  let cs = 
    try create_cursor ?range_pos ?range_len enc s with
	Malformed_code ->
	  raise (Malformed_code_at 0)
      | _ ->
	  assert false
  in
  ( try move ~num:max_int cs with 
	Cursor_out_of_range -> ()
      | Malformed_code ->
	  (* Now cursor_pos is the byte position of the last valid
	   * character. Add the length of this character.
	   *)
	  let n = cs.cursor_slice_blen.(cs.cursor_index) in
	  raise (Malformed_code_at (cs.cursor_byte_pos + n))
  );
  (* Now we are at EOF. Check this. Furthermore, check whether there is
   * an imbchar just before EOF:
   *)
  ( try
      let _ = uchar_at cs in
      assert false
    with
	End_of_string -> ()
  );
  ( try
      move ~num:(-1) cs;
      let _ = uchar_at cs in
      ()
    with
	Cursor_out_of_range -> ()  (* empty string *)
      | Partial_character ->
	  raise (Malformed_code_at (cs.cursor_byte_pos))
  );
  ()
;;
  
  

let ustring_iter enc f ?range_pos ?range_len s =
  let cs = create_cursor ?range_pos ?range_len enc s in
  try
    while true do
      let ch = uchar_at cs in   (* or End_of_string *)
      f ch;
      move cs
    done;
    assert false
  with
      End_of_string ->
	()
    | Partial_character ->
	raise Malformed_code
;;


let ustring_map enc f ?range_pos ?range_len s =
  (* The following algorithm works only if the mapped lists are short:
     let mkch = ustring_of_uchar enc in
     let subst p =
     let p' = f p in
       String.concat "" (List.map mkch p')
     in
     convert ~subst ~in_enc:enc ~out_enc:`Enc_empty ?range_pos ?range_len s
  *)
  let buf = Buffer.create 250 in
  ustring_iter
    enc
    (fun p ->
       let l = f p in
       Buffer.add_string 
	 buf (String.concat "" (List.map (ustring_of_uchar enc) l))
    )
    ?range_pos
    ?range_len
    s;
  Buffer.contents buf
;;


let ustring_sub enc pos len ?range_pos ?range_len s =
  try
    if pos < 0 || len < 0 then raise Cursor_out_of_range;
    let cs = create_cursor ?range_pos ?range_len enc s in
    move ~num:pos cs;
    let byte_pos_0 = cursor_pos cs in
    move ~num:len cs;
    let byte_pos_1 = cursor_pos cs in
    (* Check: The last character of the string must not be an imbchar *)
    if len > 0 then (
      move ~num:(-1) cs;
      let _ = uchar_at cs in ();  (* or Partial_character *)
    );
    String.sub s byte_pos_0 (byte_pos_1 - byte_pos_0)
  with
      Cursor_out_of_range -> invalid_arg "Netconversion.ustring_sub"
    | Partial_character -> raise Malformed_code
;;


let ustring_compare enc f ?range_pos:rp1 ?range_len:rl1 s1 
                          ?range_pos:rp2 ?range_len:rl2 s2 =
  let cs1 = create_cursor ?range_pos:rp1 ?range_len:rl1 enc s1 in
  let cs2 = create_cursor ?range_pos:rp2 ?range_len:rl2 enc s2 in
  let r = ref 0 in
  try
    while !r = 0 do
      let ch1 = uchar_at cs1 in  (* or End_of_string *)
      let ch2 = uchar_at cs2 in  (* or End_of_string *)
      r := f ch1 ch2
    done;
    !r
  with 
      End_of_string ->
	( match cursor_at_end cs1, cursor_at_end cs2 with
	      true, false -> (-1)
	    | false, true -> 1
	    | true, true -> 0
	    | _ -> assert false
	)
    | Partial_character -> raise Malformed_code
;;


let uarray_of_ustring enc ?(range_pos=0) ?range_len s =
  let range_len =
    match range_len with
	Some l -> l
      | None   -> String.length s - range_pos in

  if range_pos < 0 || range_len < 0 || range_pos+range_len > String.length s 
  then invalid_arg "Netconversion.uarray_of_ustring";

  let slice_length = big_slice  in
  let slice_char = Array.make slice_length (-1) in
  let slice_blen = Array.make slice_length 1 in

  let k = ref 0 in
  let e = ref enc in
  let reader = ref (get_reader enc) in
  let buf = ref [] in

  while !k < range_len do
    let (n_inc, k_inc, enc') = 
      try
	!reader slice_char slice_blen s (range_pos + !k) (range_len - !k)
      with
	  Malformed_code_read(_,_,_) -> raise Malformed_code
    in
    
    k := !k + k_inc;
    buf := (Array.sub slice_char 0 n_inc) :: !buf ;

    if enc' <> !e then begin
      e := enc';
      reader := get_reader enc';
      Array.fill slice_blen 0 slice_length 1;
    end;

    if n_inc < slice_length then (
      (* EOF *)
      if !k < range_len then raise Malformed_code; 
                             (* s ends with multi-byte prefix*)
      k := range_len;  
    );

  done;

  Array.concat (List.rev !buf)
;;


let ustring_of_uarray ?(subst = fun code -> raise (Cannot_represent code)) 
                      enc ?(pos=0) ?len ua =
  let len =
    match len with
	Some l -> l
      | None   -> Array.length ua - pos in

  if pos < 0 || len < 0 || pos+len > Array.length ua then
    invalid_arg "Netconversion.ustring_of_uarray";

  (* Estimate the size of the output string: 
   * length * 2 is just guessed. It is assumed that this number is usually
   * too large, and to avoid that too much memory is wasted, the buffer is
   * limited by 10000.
   *)
  let size = ref (max multibyte_limit (min 10000 (len * 2))) in
  let out_buf = ref (String.create !size) in

  let writer = get_writer enc in

  let k_in = ref 0 in
  let k_out = ref 0 in

  while !k_in < len do
    let k_in_inc, k_out_inc = 
      writer 
	ua (pos + !k_in) (len - !k_in) !out_buf !k_out (!size - !k_out) subst 
    in
    k_in  := !k_in  + k_in_inc;
    k_out := !k_out + k_out_inc;

    (* double the size of out_buf: *)
    let size' = min Sys.max_string_length (!size + !size) in
    if size' < !size + multibyte_limit then 
      failwith "Netconversion.ustring_of_uarray: string too long";
    let out_buf' = String.create size' in
    String.blit !out_buf 0 out_buf' 0 !k_out;
    out_buf := out_buf';
    size := size';
  done;

  String.sub !out_buf 0 !k_out
;;
