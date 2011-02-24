(* $Id: netconversion.mli 1509 2010-12-17 02:01:12Z gerd $
 * ----------------------------------------------------------------------
 *)

(** Conversion between character encodings 
 *
 * {b Contents}
 * {ul
 *   {- {!Netconversion.preliminaries}
 *     {ul
 *       {- {!Netconversion.unicode}}
 *       {- {!Netconversion.subsets}}
 *       {- {!Netconversion.linking}}
 *       {- {!Netconversion.domain}}
 *       {- {!Netconversion.problems}}}}
 *   {- {!Netconversion.interface}
 *     {ul
 *       {- {!Netconversion.direct_conv}}
 *       {- {!Netconversion.cursors}
 *           {ul {- {!Netconversion.bom}}}}
 *       {- {!Netconversion.unicode_functions}}
 *     }
 *   }
 * }
 *)


(** {1:preliminaries Preliminaries}
 *
 * A {b character set} is a set of characters where every character is
 * identified by a {b code point}. An {b encoding} is a way of 
 * representing characters from a set in byte strings. For example,
 * the Unicode character set has more than 96000 characters, and
 * the code points have values from 0 to 0x10ffff (not all code points
 * are assigned yet). The UTF-8 encoding represents the code points
 * by sequences of 1 to 4 bytes. There are also encodings that 
 * represent code points from several sets, e.g EUC-JP covers four
 * sets.
 *
 * Encodings are enumerated by the type [encoding], and names follow
 * the convention [`Enc_*], e.g. [`Enc_utf8]. 
 * Character sets are enumerated by the type
 * [charset], and names follow the convention [`Set_*], e.g.
 * [`Set_unicode].
 *
 * This module deals mainly with encodings. It is important to know
 * that the same character set may have several encodings. For example,
 * the Unicode character set can be encoded as UTF-8 or UTF-16.
 * For the 8 bit character sets, however, there is usually only one
 * encoding, e.g [`Set_iso88591] is always encoded as [`Enc_iso88591].
 *
 * In a {b single-byte encoding} every code point is represented by
 * one byte. This is what many programmers are accustomed at, and
 * what the O'Caml language specially supports: A [string] is
 * a sequence of [char]s, where [char] means an 8 bit quantity
 * interpreted as character. For example, the following piece of code allocates
 * a [string] of four [char]s, and assigns them individually:
 *
 * {[
 * let s = String.create 4 in
 * s.[0] <- 'G';
 * s.[1] <- 'e';
 * s.[2] <- 'r';
 * s.[3] <- 'd';
 * ]}
 * 
 * In a {b multi-byte encoding} there are code points that are represented
 * by several bytes. As we still represent such text as [string], the
 * problem arises that a single [char], actually a byte, often represents 
 * only a fraction of a full multi-byte character. There are two solutions:
 * - Give up the principle that text is represented by [string].
 *   This is, for example, the approach chosen by [Camomile], another O'Caml
 *   library dealing with Unicode. Instead, text is represented as
 *   [int array]. This way, the algorithms processing the text can
 *   remain the same.
 * - Give up the principle that individual characters can be directly
 *   accessed in a text. This is the primary way chosen by Ocamlnet.
 *   This means that there is not any longer the possibility to read
 *   or write the [n]th character of a text. One can, however, still 
 *   compose texts by just concatenating the strings representing
 *   individual characters. Furthermore, it is possible to define
 *   a cursor for a text that moves sequentially along the text.
 *   The consequence is that programmers are restricted to sequential
 *   algorithms. Note that the majority of text processing falls into
 *   this class.
 *
 * The corresponding piece of code for Ocamlnet's Unicode implementation
 * is:
 * {[
 * let b = Buffer.create 80 in
 * Buffer.add b (ustring_of_uchar `Enc_utf8 71);  (* 71 = code point of 'G' *)
 * Buffer.add b (ustring_of_uchar `Enc_utf8 101); (* 101 = code point of 'e' *)
 * Buffer.add b (ustring_of_uchar `Enc_utf8 114); (* 114 = code point of 'r' *)
 * Buffer.add b (ustring_of_uchar `Enc_utf8 100); (* 100 = code point of 'd' *)
 * let s = Buffer.contents b
 * ]}
 *
 * It is important to always remember that a [char] is no longer 
 * a character but simply a byte. In many of the following explanations,
 * we strictly distinguish between {b byte positions} or {b byte counts},
 * and {b character positions} or {b character counts}.
 *
 * There a number of special effects that usually only occur in
 * multi-byte encodings:
 *
 * - Bad encodings: Not every byte sequence is legal. When scanning
 *   such text, the functions will raise the exception [Malformed_code]
 *   when they find illegal bytes.
 * - Unassigned code points: It may happen that a byte sequence is
 *   a correct representation for a code point, but that the code point
 *   is unassigned in the character set. When scanning, this is also
 *   covered by the exception [Malformed_code]. When converting from
 *   one encoding to another, it is also possible that the code point
 *   is only unassigned in the target character set. This case is
 *   usually handled by a substitution function [subst], and if no such
 *   function is defined, by the exception [Cannot_represent].
 * - Incomplete characters: The trailing bytes of a string may be the
 *   correct beginning of a byte sequence for a character, but not a
 *   complete sequence. Of course, if that string is the end of a
 *   text, this is just illegal, and also a case for [Malformed_code].
 *   However, when text is processed chunk by chunk, this phenomenon
 *   may happen legally for all chunks but the last. For this reason,
 *   some of the functions below handle this case specially.
 * - Byte order marks: Some encodings have both big and little endian
 *   variants. A byte order mark at the beginning of the text declares
 *   which variant is actually used. This byte order mark is a 
 *   declaration written like a character, but actually not a 
 *   character.
 *
 * There is a special class of encodings known as {b ASCII-compatible}.
 * They are important because there are lots of programs and protocols
 * that only interpret bytes from 0 to 127, and treat the bytes from
 * 128 to 255 as data. These programs can process texts as long as
 * the bytes from 0 to 127 are used as in ASCII. Fortunately, many
 * encodings are ASCII-compatible, including UTF-8.
 *
 * {2:unicode Unicode}
 *
 * [Netconversion] is centred around Unicode.
 * The conversion from one encoding to another works by finding the
 * Unicode code point of the character
 * to convert, and by representing the code point in the target encoding,
 * even if neither encodings have to do with Unicode.
 * Of course, this approach requires that all character sets handled
 * by [Netconversion] are subsets of Unicode.
 *
 * The supported range of Unicode code points: 0 to 0xd7ff, 0xe000 to 0xfffd,
 * 0x10000 to 0x10ffff. All these code points can be represented in 
 * UTF-8 and UTF-16. [Netconversion] does not know which of the code
 * points are assigned and which not, and because of this, it simply
 * allows all code points of the mentioned ranges (but for other character
 * sets, the necessary lookup tables exist).
 *
 * {b UTF-8:} The UTF-8 representation can have one to four bytes. Malformed 
 *   byte sequences are always rejected, even those that want to cheat the
 *   reader like "0xc0 0x80" for the code point 0. There is special support
 *   for the Java variant of UTF-8 ([`Enc_java]). UTF-8 strings must not
 *   have a byte order mark (it would be interpreted as "zero-width space"
 *   character).
 *
 * {b UTF-16:} When reading from a string encoded as [`Enc_utf16], a byte
 *   order mark is expected at the beginning. The detected variant 
 *   ([`Enc_utf16_le] or [`Enc_utf16_be]) is usually returned by the parsing
 *   function. The byte order mark is not included into the output string. - 
 *   Some functions of this
 *   module cannot cope with [`Enc_utf16] (i.e. UTF-16 without endianess
 *   annotation), and will fail.
 *
 *   Once the endianess is determined, the code point 0xfeff is no longer
 *   interpreted as byte order mark, but as "zero-width non-breakable space".
 *
 *   Some code points are represented by pairs of 16 bit values, these
 *   are the so-called "surrogate pairs". They can only occur in UTF-16.
 *
 * {2:subsets Subsets of Unicode}
 *
 * The non-Unicode character sets are subsets of Unicode. Here, it may
 * happen that a Unicode code point does not have a corresponding 
 * code point. In this case, certain rules are applied to handle
 * this (see below). It is, however, ensured that every non-Unicode
 * code point has a corresponding Unicode code point. (In other words,
 * character sets cannot be supported for which this property does
 * not hold.)
 *
 * It is even possible to create further subsets artificially. The
 * encoding [`Enc_subset(e,def)] means to derive a new encoding from
 * the existing one [e], but to only accept the code points for which
 * the definition function [def] yields the value [true]. For example,
 * the encoding 
 * {[ `Enc_subset(`Enc_usascii, 
 *             fun i -> i <> 34 && i <> 38 && i <> 60 && i <> 62) ]}
 * is ASCII without the bracket angles, the quotation mark, and the
 * ampersand character, i.e. the subset of ASCII that can be included
 * in HTML text without escaping.
 *
 * If a code point is not defined by the encoding but found in a text, 
 * the reader will raise the exception [Malformed_code]. When text is
 * output, however, the [subst] function will be called for undefined code 
 * points (which raises [Cannot_represent] by default). The [subst]
 * function is an optional argument of many conversion functions that
 * allows it to insert a substitution text for undefined code points.
 * Note, however, that the substitution text is restricted to at most
 * 50 characters (because unlimited length would lead to difficult
 * problems we would like to avoid).
 *
 * {2:linking Linking this module}
 *
 * Many encodings require lookup tables. The following encodings
 * are built-in and always supported:
 *
 * - Unicode: [`Enc_utf8], [`Enc_java], [`Enc_utf16], [`Enc_utf16_le], 
     [`Enc_utf16_be]
 * - Other: [`Enc_usascii], [`Enc_iso88591], [`Enc_empty]
 *
 * The lookup tables for the other encodings are usually loaded at
 * runtime, but it is also possible to embed them in the generated
 * binary executable. See the file [INSTALL] for details. The functions
 * [available_input_encodings] and [available_output_encodings] can
 * be invoked to find out which encodings can be loaded, or are available
 * otherwise.
 *
 * {2:domain Supported Encodings, Restrictions}
 *
 * I took the mappings from [www.unicode.org], and the standard names of
 * the character sets from IANA. Obviously, many character sets are missing
 * that can be supported; especially ISO646 character sets, and many EBCDIC 
 * code pages. Stateful encodings like generic ISO-2022 have been omitted
 * (stateless subsets of ISO-2022 like EUC can be supported, however;
 * currently we support EUC-JP and EUC-KR).
 *
 * Because of the copyright statement from Unicode, I cannot put the
 * source tables that describe the mappings into the distribution. They
 * are publicly available from [www.unicode.org].
 *
 * {2:problems Known Problems}
 *
 * - The following charsets do not have a bijective mapping to Unicode:
 *   adobe_standard_encoding, adobe_symbol_encoding, 
 *   adobe_zapf_dingbats_encoding, cp1002 (0xFEBE). The current implementation
 *   simply removes one of the conflicting code point pairs - this might
 *   not what you want.
 * - Japanese encodings: 
 *   JIS X 0208: The character 1/32 is mapped to 0xFF3C, and not
 *   to 0x005C.
 *)


(** {1:interface Interface}
 *
 * {b Naming conventions:}
 *
 * As it is possible to refer to substrings by either giving a byte
 * offset or by counting whole characters, these naming conventions
 * are helpful:
 *
 * - Labels called [range_pos] and [range_len] refer to byte positions of
 *   characters, or substrings
 * - Labels called [count] refer to positions given as the number of characters
 *   relative to an origin
 *
 * Furthermore:
 * 
 * - A [uchar] is a single Unicode code point represented as int
 * - A [ustring] is a string of encoded characters
 * - A [uarray] is an [array of int] representing a string
 *)

exception Malformed_code
  (** Raised when an illegal byte sequence is found *)

exception Cannot_represent of int
  (** Raised when a certain Unicode code point cannot be represented in
   * the selected output encoding
   *)


(** The polymorphic variant enumerating the supported encodings. We have:
 * - [`Enc_utf8]: UTF-8
 * - [`Enc_java]: The UTF-8 variant used by Java (the only difference is
 *   the representation of NUL)
 * - [`Enc_utf16]: UTF-16 with unspecified endianess (restricted)
 * - [`Enc_utf16_le]: UTF-16 little endian
 * - [`Enc_utf16_be]: UTF-16 big endian
 * - [`Enc_usascii]: US-ASCII (7 bits)
 * - [`Enc_iso8859]{i n}: ISO-8859-{i n}
 * - [`Enc_koi8r]: KOI8-R
 * - [`Enc_jis0201]: JIS-X-0201 (Roman and Katakana)
 * - [`Enc_eucjp]: EUC-JP (code points from US-ASCII, JIS-X-0202, -0208, and
 *   -0212)
 * - [`Enc_euckr]: EUC-KR (code points from US-ASCII, KS-X-1001)
 * - [`Enc_windows]{i n}: WINDOWS-{i n}
 * - [`Enc_cp]{i n}: IBM code page {i n}. Note that there are both ASCII-
 *   and EBCDIC-based code pages
 * - [`Enc_adobe_*]: Adobe-specific encodings, e.g. used in Adobe fonts
 * - [`Enc_mac*]: Macintosh-specific encodings
 * - [`Enc_subset(e,def)]: The subset of [e] by applying the definition 
 *   function [def]
 * - [`Enc_empty]: The empty encoding (does not represent any character)
 *)
type encoding =
  [  `Enc_utf8       (* UTF-8 *)
  |  `Enc_java       (* The variant of UTF-8 used by Java *)
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
  |  `Enc_koi8r      (* KOI8-R *)
  |  `Enc_jis0201    (* JIS-X-0201 (Roman in lower half; Katakana upper half *)
  |  `Enc_eucjp      (* EUC-JP (includes US-ASCII, JIS-X-0201, -0208, -0212) *)
    (* Japanese, TODO: *)
(*|  `Enc_iso2022jp of jis_state = [ `Enc_usascii | `Enc_jis0201 |
                                     `Enc_jis0208_1978 | `Enc_jis0208_1893 ]
      It is very likely that ISO-2022 will be handled in a different module.
      This encoding is too weird.
  |  `Enc_sjis
*)
  |  `Enc_euckr      (* EUC-KR (includes US-ASCII, KS-X-1001) *)
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
  |  `Enc_cp866
  |  `Enc_cp869
  |  `Enc_cp874
  |  `Enc_cp1006
   (* IBM, EBCDIC-based: *)
  |  `Enc_cp037
  |  `Enc_cp424
  |  `Enc_cp500
  |  `Enc_cp875
  |  `Enc_cp1026
  |  `Enc_cp1047
   (* Adobe: *)
  |  `Enc_adobe_standard_encoding
  |  `Enc_adobe_symbol_encoding
  |  `Enc_adobe_zapf_dingbats_encoding
   (* Apple: *)
  |  `Enc_macroman
   (* Encoding subset: *)
  |  `Enc_subset of (encoding * (int -> bool))
  |  `Enc_empty     (* does not encode any character *)

  ]


(** A [charset] is simply a set of code points. It does not say how
 * the code points are encoded as bytes. Every encoding implies a certain
 * charset (or several charsets) that can be encoded, but the reverse is 
 * not true.
 *)
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


(** {b Pre-evaluation of the encoding argument:}
 * 
 * A number of the following functions can be made run faster if they are
 * called several times for the same encoding. In this case, it is recommended
 * to apply the function once partially with the encoding argument, and to
 * call the resulting closure instead. For example, [ustring_of_uchar] supports
 * this technique:
 *
 * {[
 *   let my_ustring_of_uchar = ustring_of_uchar my_enc in
 *   let s1 = my_ustring_of_uchar u1 ...
 *   let s2 = my_ustring_of_uchar u2 ... ]}
 *
 * This is {b much} faster than
 *
 * {[
 *   let s1 = ustring_of_uchar my_enc u1 ...
 *   let s2 = ustring_of_uchar my_enc u2 ... ]}
 *
 * The availability of this optimization is indicated by the predicate
 * PRE_EVAL({i arg}) where {i arg} identifies the encoding argument.
 *
 * {b Inlining}
 *
 * When a function can be inlined across module/library boundaries,
 * this is indicated by the predicate INLINED. Of course, this works
 * only for the ocamlopt compiler.
 *)


val encoding_of_string : string -> encoding;;
    (** Returns the encoding of the name of the encoding. Fails if the 
     * encoding is unknown.
     * E.g. [encoding_of_string "iso-8859-1" = `Enc_iso88591] 
     *
     * Punctuation characters (e.g. "-") and year suffixes (e.g.
     * ":1991") are ignored.
     *)


val string_of_encoding : encoding -> string;;
    (** Returns the name of the encoding. *)


val is_ascii_compatible : encoding -> bool;;
    (** "ASCII compatible" means: The bytes 1 to 127 represent the ASCII
     * codes 1 to 127, and no other representation of a character contains
     * the bytes 1 to 127.
     * 
     * For example, ISO-8859-1 is ASCII-compatible because the byte 1 to
     * 127 mean the same as in ASCII, and all other characters use bytes
     * greater than 127. UTF-8 is ASCII-compatible for the same reasons,
     * it does not matter that there are multi-byte characters.
     * EBCDIC is not ASCII-compatible because the bytes 1 to 127 do not mean
     * the same as in ASCII. UTF-16 is not ASCII-compatible because the bytes
     * 1 to 127 can occur in multi-byte representations of non-ASCII
     * characters.
     *
     * The byte 0 has been excluded from this definition because the C
     * language uses it with a special meaning that has nothing to do with
     * characters, so it is questionable to interpret the byte 0 anyway.
     *)


val is_single_byte : encoding -> bool
  (** Returns whether the encoding is a single-byte encoding *)


val same_encoding : encoding -> encoding -> bool
  (** Whether both encodings are the same. [`Enc_subset] encodings are only
   * considered as equal when the definition functions are physically the same.
   *
   * Warning: Don't use ( = ) to compare encodings because this may
   * fail.
   *)


val byte_order_mark : encoding -> string
  (** Returns the byte order mark that must occur at the beginning of
   * files to indicate whether "little endian" or "big endian" is used.
   * If this does not apply to the encoding, an empty string is returned.
   *
   * See also the section about "{!Netconversion.bom}" below.
   *)


val makechar : encoding -> int -> string
  (** [makechar enc i:]
   * Creates the string representing the Unicode code point [i] in encoding
   * [enc]. Raises [Not_found] if the character is legal but cannot be 
   * represented in [enc].
   * 
   * Possible encodings: everything but [`Enc_utf16].
   *
   * Evaluation hints:
   * - PRE_EVAL(encoding)
   *
   * @deprecated This function is deprecated since ocamlnet-0.96. Use
   *   [ustring_of_uchar] instead.
   *)


val ustring_of_uchar : encoding -> int -> string
  (** [ustring_of_uchar enc i]:
   * Creates the string representing the Unicode code point [i] in encoding
   * [enc]. Raises [Cannot_represent i] if the character is legal but cannot be 
   * represented in [enc].
   * 
   * Possible encodings: everything but [`Enc_utf16].
   *
   * Evaluation hints:
   * - PRE_EVAL(encoding)
   *)


val to_unicode : charset -> int -> int
  (** Maps the code point of the charset to the corresponding 
   * Unicode code point, or raises [Malformed_code], when the
   * input number does not correspond to a code point.
   *
   * Note [`Set_jis0208] and [`Set_jis0212]: Code points are usually
   * given by a row and column number. The numeric code point returned by
   * this function is computed by multiplying the row number (1..94) with 96,
   * and by adding the column number (1..94), i.e. row*96+column.
   *
   * Evaluation hints:
   * - PRE_EVAL(charset)
   *)


val from_unicode : charset -> int -> int
  (** Maps the Unicode code point to the corresponding code point of
   * the charset, or raises [Cannot_represent] when there is no such
   * corresponding code point.
   *
   * Note [`Set_jis0208] and [`Set_jis0212]: Code points are usually
   * given by a row and column number. The numeric code point returned by
   * this function is computed by multiplying the row number (1..94) with 96,
   * and by adding the column number (1..94), i.e. row*96+column.
   *
   * Evaluation hints:
   * - PRE_EVAL(charset)
   *)

val available_input_encodings : unit -> encoding list
  (** Returns the list of all available encodings that can be used for
   * input strings. The list reflects the set of loadable/linked [Netmapping]
   * modules.
   *)


val available_output_encodings : unit -> encoding list
  (** Returns the list of all available encodings that can be used for
   * output strings. The list reflects the set of loadable/linked [Netmapping]
   * modules.
   *)

val user_encoding : unit -> encoding option
  (** Determines the preferred user encoding:

      - Unix: This is the character set from the current locale
      - Win32: This is derived from the current ANSI code page

      If an error occurs while determining the result, the value
      [None] is returned.
   *)

val win32_code_pages : (int * encoding) list
  (** Mapping between Win32 code page numbers and Ocamlnet encodings.
      This is incomplete. The official list:
      http://msdn.microsoft.com/en-us/library/dd317756%28v=VS.85%29.aspx
   *)



(**********************************************************************)
(* Conversion between character encodings                             *)
(**********************************************************************)

(** {2:direct_conv Direct Conversion} *)

(** In order to convert a string from one encoding to another, call
 * [convert] like in
 *
 * {[ let s_utf8 = 
 *    convert ~in_enc:`Enc_iso88591 ~out_enc:`Enc_utf8 s_latin1 ]}
 *
 * which converts the ISO-8859-1 string [s_latin1] to the UTF-8 string
 * [s_utf8].
 *
 * It is also possible to convert while reading from or writing to a file.
 * This use case is effectively handled by the class 
 * {!Netconversion.conversion_pipe}.
 * See the explanations of this class for examples.
 *)


val convert : ?subst:(int -> string) ->
              in_enc:encoding -> 
              out_enc:encoding ->
              ?range_pos:int -> ?range_len:int ->
	      string ->
                string 
  (** Converts the string from [in_enc] to [out_enc], and returns it.
   * The string must consist of a whole number of characters. If it
   * ends with an incomplete multi-byte character, however, this is
   * detected, and the exception [Malformed_code] will be raised.
   * This exception is also raised for other encoding errors in the
   * input string.
   *
   * @param subst This function is invoked for code points of [in_enc] that
   *   cannot be represented in [out_enc], and the result of the function 
   *   invocation is substituted (directly, without any further conversion).
   *   Restriction: The string returned by [subst] must not be longer than 50
   *   bytes.
   *   If [subst] is missing, [Cannot_represent] is raised in this case.
   *
   * @param range_pos Selects a substring for conversion. [range_pos]
   *   is the byte position of the first character of the substring.
   *   (Default: 0)
   *
   * @param range_len Selects a substring for conversion. [range_len]
   *   is the length of the substring in bytes (Default: Length
   *   of the input string minus [range_pos])
   *)


val recode_string : in_enc:encoding -> 
                    out_enc:encoding ->
		    ?subst:(int -> string) ->
		    string ->
                    string 
  (** Recodes a complete string from [in_enc] to [out_enc], and returns it.
   * The function [subst] is invoked for code points of [in_enc] that cannot
   * be represented in [out_enc], and the result of the function invocation
   * is substituted.
   * Restriction: The string returned by [subst] must not be longer than 50
   * bytes.
   * If [subst] is missing, [Not_found] is raised in this case.
   *
   * @deprecated This function is obsolete since ocamlnet-0.96. Use
   *   [convert] instead.
   *)


val recode : in_enc:encoding -> 
             in_buf:string -> 
	     in_pos:int ->
	     in_len:int -> 
	     out_enc:encoding -> 
	     out_buf:string -> 
	     out_pos:int ->
	     out_len:int ->
	     max_chars:int ->
             subst:(int -> string) -> (int * int * encoding)
  (**
   * Converts the character sequence contained in the at most [in_len] bytes
   * of [in_buf] starting at byte position [in_pos], and writes the result 
   * into at most [out_len] bytes of [out_buf] starting at byte position
   * [out_pos]. At most [max_chars] characters are converted from 
   * [in_buf] to [out_buf].
   *
   * The characters in [in_buf] are assumed to be encoded as [in_enc], and the 
   * characters in [out_buf] will be encoded as [out_enc]. The case
   * [in_enc = out_enc] is not handled specially, and is carried out as
   * fast as any other conversion.
   *
   * If there is a code point which cannot be represented in [out_enc],
   * the function [subst] is called with the code point as argument, and the
   * resulting string (which must already be encoded as [out_enc]) is
   * inserted instead. 
   * It is possible that [subst] is called several times for the same
   * character. Restriction: The string returned by subst must not be longer
   * than 50 bytes.
   *
   * It is allowed that the input buffer ends with an incomplete
   * multi-byte character. This character is not converted, i.e. the
   * conversion ends just before this character. This special condition
   * is not indicated to the caller.
   *
   * @return The triple [(in_n, out_n, in_enc')] is returned:
   * - [in_n] is the actual number of bytes that have been converted from
   *   [in_buf]; [in_n] may be smaller than [in_len] because of incomplete
   *   multi-byte characters, or because the output buffer has less space
   *   for characters than the input buffer, or because of a change
   *   of the encoding variant.
   * - [out_n] is the actual number of bytes written into [out_buf].
   * - [in_enc'] is normally identical to [in_enc]. However, there are cases
   *   where the encoding can be refined when looking at the byte
   *   sequence; for example whether a little endian or big endian variant
   *   of the encoding is used. [in_enc'] is the variant of [in_enc] that was
   *   used for the last converted character.
   *
   * If there is at least one complete character in [in_buf], and at least
   * space for one complete character in [out_buf], and [max_chars >= 1], it is 
   * guaranteed that [in_n > 0 && out_n > 0].
   *)


class conversion_pipe : 
        ?subst:(int -> string) ->
        in_enc:encoding -> 
	out_enc:encoding -> 
	unit ->
	  Netchannels.io_obj_channel
  (** This pipeline class (see [Netchannels] for more information) can be used
   * to recode a netchannel while reading or writing. The argument [in_enc]
   * is the input encoding, and [out_enc] is the output encoding.
   *
   * The channel must consist of a whole number of characters. If it
   * ends with an incomplete multi-byte character, however, this is
   * detected, and the exception [Malformed_code] will be raised.
   * This exception is also raised for other encoding errors in the
   * channel data.
   *
   * {b Example.} Convert ISO-8859-1 to UTF-8 while writing to the file
   * ["output.txt"]:
   * 
   * {[
   *    let ch = new output_channel (open_out "output.txt") in
   *    let encoder = 
   *      new conversion_pipe ~in_enc:`Enc_iso88591 ~out_enc:`Enc_utf8 () in
   *    let ch' = new output_filter encoder ch in
   *    ... (* write to ch' *)
   *    ch' # close_out();
   *    ch  # close_out();  (* you must close both channels! *)
   * ]}
   *
   * If you write as UTF-16, don't forget to output the byte order
   * mark yourself, as the channel does not do this.
   *
   * {b Example.} Convert UTF-16 to UTF-8 while reading from the file
   * ["input.txt"]:
   *
   * {[
   *    let ch = new input_channel (open_in "input.txt") in
   *    let encoder = 
   *      new conversion_pipe ~in_enc:`Enc_utf16 ~out_enc:`Enc_utf8 () in
   *    let ch' = new input_filter ch encoder in
   *    ... (* read from ch' *)
   *    ch' # close_in();
   *    ch  # close_in();  (* you must close both channels! *)
   * ]}
   *
   * @param subst This function is invoked for code points of [in_enc] that
   *   cannot be represented in [out_enc], and the result of the function 
   *   invocation is substituted (directly, without any further conversion).
   *   Restriction: The string returned by [subst] must not be longer than 50
   *   bytes.
   *   If [subst] is missing, [Cannot_represent] is raised in this case.
   *)


class recoding_pipe : 
        ?subst:(int -> string) ->
        in_enc:encoding -> 
	out_enc:encoding -> 
	unit ->
	  Netchannels.io_obj_channel
  (** Recodes a channel like [conversion_pipe]. The difference is that
   * [subst] raises [Not_found] by default, and not [Cannot_represent].
   *
   * @deprecated This class is deprecated since ocamlnet-0.96. Use
   *   [conversion_pipe] instead.
   *)

(**********************************************************************)
(* Cursors                                                            *)
(**********************************************************************)

(** {2:cursors Reading Text Using Cursors}
 * 
 * A cursor is a reference to a character in an encoded string. The
 * properties of the current character can be obtained, and the cursor
 * can be moved relative to its current position.
 *
 * For example, the following loop outputs the Unicode code points
 * of all characters of the UTF-8 input string [s]:
 *
 * {[
 * let cs = create_cursor `Enc_utf8 s in
 * while not (cursor_at_end cs) do
 *   let n = cursor_char_count cs in
 *   let ch = uchar_at cs in
 *   printf "At position %d: %d\n" n ch;
 *   move cs;
 * done
 * ]}
 *
 * For a more exact definition, cursors are modeled as follows: The reference
 * to the encoded string is contained in the cursor. This
 * can be a complete string, or an arbitrary substring (denoted by a
 * range of valid byte positions). The cursor
 * position can be initially set to an arbitrary byte position of the
 * encoded string.
 *
 * Cursor positions can be denoted by
 * - byte positions [p] in the encoded string, or by
 * - character counts [n] relative to the initial position.
 *
 * Valid cursor positions are:
 * - [n=0]: This is always the initial cursor position
 * - [n>0]: Positive char counts refer to characters right to the initial
 *   character. The rightmost position is the position [n_max] past the
 *   rightmost character. The rightmost position does not have a
 *   code point.
 * - [n<0]: Negative char counts refer to characters left to the initial
 *   character. The leftmost position is the position [n_min] of the
 *   leftmost character.
 *
 * For the empty string we have [n_min = n_max = 0], complementing the
 * above definition.
 *
 * Cursors are moved to the left or right of their current position
 * by a whole number of characters. When it is tried to move them
 * past the leftmost or rightmost position, the cursor is placed to the
 * leftmost or rightmost position, respectively, and the exception
 * [Cursor_out_of_range] is raised.
 *
 * There are two cases of illegal encodings:
 * - When the last byte sequence of the encoded string is an incomplete
 *   multi-byte character, this is detected, and the special exception
 *   [Partial_character] is raised when the code point of this character
 *   is read. Note that this can only happen at position [n_max-1]. It
 *   is allowed to move beyond this character to [n_max].
 * - When an illegal byte sequence occurs in the encoded string (including
 *   an incomplete multi-byte character at the beginning of the string),
 *   it is not possible to move the cursor to this character, or across
 *   this character. When it is tried to do so, the cursor stops just
 *   before the bad sequence, and the exception [Malformed_code] is
 *   raised.
 *
 * It is undefined what happens when the encoded string is modified
 * while a cursor is in use referring to it.
 *)

type cursor
  (** A cursor denotes a character position in an encoded string *)

exception End_of_string
  (** Raised when it is tried to access the character after the end of the
   * string (at position [n_max])
   *)

exception Cursor_out_of_range
  (** Raised when it is tried to move the cursor beyond the beginning of the
   * string or beyond the end of the string. In the latter case, it is
   * legal to move the cursor to the position following the last character,
   * but it is not possible to move it further.
   *)

exception Partial_character
  (** Raised when the last character of the string is an incomplete
   * multi-byte character, and it is tried to get the code point
   * (using [uchar_at]).
   *)

exception Byte_order_mark
  (** Raised when it is tried to get the code point of the BOM at the
   * beginning of the string
   *)


val create_cursor : ?range_pos:int -> ?range_len:int -> 
                    ?initial_rel_pos:int -> 
                    encoding -> string -> cursor
  (** Creates a new cursor for the passed string and the passed encoding.
   * By default, the allowed range of the cursor is the whole string,
   * and the cursor is intially positioned at the beginning of the string.
   * The {b range} is the part of the string the cursor can move within.
   *
   * {b Special behaviour for [`Enc_utf16]:} UTF-16 with unspecified
   * endianess is handled specially. First, this encoding is only
   * accepted when [initial_rel_pos=0]. Second, the first two bytes
   * must be a byte order mark (BOM) (if the string has a length of two
   * bytes or more). The BOM counts as character without code point.
   * The function [uchar_at] raises the exception [Byte_order_mark]
   * when the BOM is accessed. Third, when the cursor is moved to the
   * next character, the encoding as returned by [cursor_encoding] is
   * changed to either [`Enc_utf16_le] or [`Enc_utf16_be] according
   * to the BOM. The encoding changes back to [`Enc_utf16] when the
   * cursor is moved back to the initial position.
   *
   * @param range_pos Restricts the range of the cursor to a substring.
   *   The argument [range_pos] is the byte position of the beginning
   *   of the range. (Defaults to 0)
   * @param range_len Restricts the range of the cursor to a substring.
   *   The argument [range_len] is the length of the range.
   *   (Default: Length of the input string minus [range_pos])
   * @param initial_rel_pos The initial position of the cursor, given
   *   as bytes relative to [range_pos]. The character at this position
   *   is considered as the zeroth character of the string (as reported
   *   by [cursor_char_count])
   *)

val reinit_cursor : ?range_pos:int -> ?range_len:int -> 
                    ?initial_rel_pos:int -> 
                    ?enc:encoding -> string -> cursor -> unit
  (** Reuses an existing cursor for a new purpose. The arguments are
   * as in [create_cursor].
   *)

val copy_cursor : ?enc:encoding -> cursor -> cursor
  (** Copies the cursor. The copy can be moved independently of the original
   * cursor, but is applied to the same string. The copy starts at the
   * byte position of the string where the original cursor is currently
   * positioned.
   *
   * @param enc Optionally, the assumed
   *   encoding can be changed to a different one by passing [enc].
   *)

val cursor_target : cursor -> string
  (** Returns the string of the cursor
   *
   * Evaluation hints:
   * - INLINED
   *)

val cursor_range : cursor -> (int * int)
  (** Returns the valid range of the cursor as pair [(range_pos, range_len)] 
   *
   * Evaluation hints:
   * - INLINED
   *)

val cursor_initial_rel_pos : cursor -> int
  (** Returns the initial relative byte position of the cursor 
   *
   * Evaluation hints:
   * - INLINED
   *)

val cursor_char_count : cursor -> int
  (** Returns the character count of the cursor. The initial position
   * (when [create_cursor] was called) has the number 0, positions to the
   * right denote positive numbers, and positions to the left negative numbers.
   *
   * Evaluation hints:
   * - INLINED
   *)

val cursor_pos : cursor -> int
  (** Returns the byte position of the cursor, i.e. the byte index of
   * the string that corresponds to the cursor position. The function
   * returns the absolute position (i.e. NOT relative to [cursor_range]).
   *
   * Evaluation hints:
   * - INLINED
   *)

val uchar_at : cursor -> int
  (** Returns the Unicode code point of the character at the cursor.
   * Raises [End_of_string] if the cursor is positioned past the last
   * character.
   * Raises [Partial_character] if the last character of the analysed 
   * string range is an incomplete multi-byte character.
   * Raises [Byte_order_mark] if the first character of the string
   * is a BOM (when the encoding has BOMs).
   *
   * Evaluation hints:
   * - INLINED
   *)

val cursor_byte_length : cursor -> int
  (** Returns the byte length of the representation of the character at the
   * cursor. This works also for incomplete multi-byte characters and
   * BOMs.
   * Raises [End_of_string] if the cursor is positioned past the last
   * character. 
   *
   * Evaluation hints:
   * - INLINED
   *)

val cursor_at_end : cursor -> bool
  (** Returns whether the cursor is positioned past the last character.
   *
   * Evaluation hints:
   * - INLINED
   *)

val move : ?num:int -> cursor -> unit
  (** Moves the cursor one character to the right, or if [num] is passed,
   * this number of characters to the right. [num] can be negative in
   * which case the cursor is moved to the left.
   *
   * If the cursor were placed outside the valid range, the cursor
   * would go into an illegal state, and because of this, this is
   * handled as follows: the cursor moves to the
   * leftmost or rightmost position (depending on the direction),
   * and the exception [Cursor_out_of_range] is raised.
   *)

val cursor_encoding : cursor -> encoding
  (** Returns the encoding of the cursor. For some encodings, the
   * returned encoding depends on the position of the cursor (see
   * the note about UTF-8 in [create_cursor])
   *
   * Evaluation hints:
   * - INLINED
   *)

val cursor_blit : cursor -> int array -> int -> int -> int
 (** [cursor_blit cs ua pos len]: Copies at most [len] characters as code
  * points from
  * the cursor position and the following positions to the array [ua]
  * at index [pos]. The number of copied characters is returned.
  * If the cursor is already at the end of the string when this
  * function is called, the exception [End_of_string] will be raised instead,
  * and no characters are copied. The cursor positions containing byte
  * order marks and partial characters are never copied; this is ensured
  * by stopping the copying procedure just before these positions. This
  * may even make the function return the number 0.
  *
  * The function tries to copy as many characters as currently available
  * in the already decoded part of the string the cursor is attached to.
  * In the current implementation, this number is not higher than 250.
  * You can call [cursor_blit_maxlen] to get an upper limit.
  *
  * The function does not move the cursor.
 *)

val cursor_blit_maxlen : cursor -> int
  (** Returns the maximum number of characters [cursor_blit] can copy
   * at the current cursor position. This is the number of characters
   * [cursor_blit] would copy if the [len] argument were arbitrarily
   * large.
   *
   * Note that the value depends on the cursor position and on the
   * contents of the cursor string.
   *
   * This function raises [End_of_string] if the cursor is positioned
   * at the end of the string.
   *)

val cursor_blit_positions : cursor -> int array -> int -> int -> int
  (** Works like [cursor_blit], but copies the byte positions of the
   * characters into [ua] instead of the code points.
   *
   * When called directly after [cursor_blit] for the same cursor and
   * with the same value of [len], this function copies as many characters
   * and thus returns the same number:
   *
   * {[let n1 = cursor_blit     cs ua ua_pos len in
   * let n2 = cursor_blit_pos cs pa pa_pos len in
   * assert (n1 = n2)]}
   *)

(** {3:bom Byte Order Marks}
 *
 * Because UTF-16 allows both little and big endian, files and other
 * permanent representations of UTF-16 text are usually prepended by
 * a byte order mark (BOM). There is confusion about the BOM among
 * Unicode users, so the following explanations may be helpful.
 *
 * Of course, the BOM is only used for external representations like
 * files, as the endianess is always known for in-memory representations
 * by the running program. This module has three encoding identifiers:
 * - [`Enc_utf16]: UTF-16 where the endianess is unknown
 * - [`Enc_utf16_le]: UTF-16 little endian
 * - [`Enc_utf16_be]: UTF-16 big endian
 *
 * When a file is read, the endianess is unknown at the beginning.
 * This is expressed by [`Enc_utf16]. When the BOM is read, the encoding
 * is refined to either [`Enc_utf16_le] or [`Enc_utf16_be], whatever
 * the BOM says. This works as follows: The BOM is the representation
 * of the code point 0xfeff as little or big endian, i.e. as byte sequences
 * "0xfe 0xff" (big endian) or "0xff 0xfe" (little endian). As the "wrong"
 * code point 0xfffe is intentionally unused, the reader can determine
 * the endianess.
 *
 * There is one problem, though. Unfortunately, the code point 0xfeff
 * is also used for the "zero width non-breakable space" character.
 * When this code point occurs later in the text, it is interpreted as
 * this character. Of course, this means that one must know whether
 * there is a BOM at the beginning, and if not, one must know the
 * endianess. One cannot program in the style "well, let's see what is
 * coming and guess".
 *
 * Furthermore, the BOM is only used for encodings where one can specify
 * the endianess. It must not be used for UTF-8, for example, as the
 * byte order is fixed for this encoding. When a UTF-8 text begins with
 * the code point 0xfeff, it is always the "zero width non-breakable space"
 * character.
 *
 * The functions of this module can all deal with BOMs when reading
 * encoded text. In most cases, the BOM is hidden from the caller,
 * and just handled automatically. Cursors, however, treat BOMs as special
 * characters outside of the code set (exception [Byte_order_mark] is
 * raised). The writing functions of this module do not generate BOMs,
 * however, as there is no way to tell them that a BOM is needed. The
 * function [byte_order_mark] can be used to output the BOM manually.
 *
 * {3 Examples for Cursors}
 *
 * Create the cursor:
 *
 * [ let cs = create_cursor `Enc_utf8 "B\195\164r";; ]
 *
 * The cursor is now positioned at the 'B':
 *
 * [ uchar_at cs ] {i returns} [66] (i.e. B)
 *
 * Move the cursor one character to the right. In UTF-8, this is a
 * two-byte character consisting of the bytes 195 and 164:
 *
 * [ move cs ;; ]
 *
 * [ uchar_at cs ] {i returns} [228] (i.e. a-Umlaut)
 *
 * One can easily move the cursor to the end of the string:
 *
 * [ move ~num:max_int cs ;; ]
 *
 * This raises [Cursor_out_of_range], but places the cursor at the end.
 * This is the position past the last letter 'r':
 * 
 * [ uchar_at cs ] {i raises} [End_of_string]
 *
 * Go one character to the left:
 *
 * [ move ~num:(-1) cs ;; ]
 *
 * [ uchar_at cs ] {i returns} [114] (i.e. r)
 *
 * Cursors can only move relative to their current position. Of course,
 * one can easily write a function that moves to an absolute position,
 * like
 *
 * {[ let move_abs n cs = 
 *    let delta = n - cursor_pos cs in
 *    move ~num:delta cs ]}
 *
 * However, this operation is expensive (O(string length)), and should
 * be avoided for efficient algorithms. Cursors are not arrays, and an
 * algorithm should only be based on cursors when it is possible to
 * iterate over the characters of the string one after another.
 *)

(**********************************************************************)
(* String functions                                                   *)
(**********************************************************************)

(** {2:unicode_functions Unicode String Functions} *)

val ustring_length : 
        encoding -> ?range_pos:int -> ?range_len:int -> string -> int
  (** Returns the length of the string in characters. The function fails
   * when illegal byte sequences or incomplete characters are found in the
   * string with [Malformed_code].
   *
   * Evaluation hints:
   * - PRE_EVAL(encoding)
   *
   * @param range_pos The byte position of the substring to measure
   *   (default: 0)
   * @param range_len The byte length of the substring to measure
   *   (default: byte length of the input string minus [range_pos])
   *)

val ustring_iter : 
       encoding ->
       (int -> unit) ->
       ?range_pos:int -> ?range_len:int ->
       string ->
	 unit
  (** Iterates over the characters of a string, and calls the passed function
   * for every code point. The function raises [Malformed_code] when
   * illegal byte sequences or incomplete characters are found.
   *
   * @param encoding specifies the encoding
   * @param range_pos The byte position of the substring to iterate over
   *   (default: 0)
   * @param range_len The byte length of the substring to iterate over
   *   (default: byte length of the input string minus [range_pos])
   *)

val ustring_map :
       encoding ->
       (int -> int list) ->
       ?range_pos:int -> ?range_len:int ->
       string ->
	 string
  (** Maps every character of a string to a list of characters, and returns
   * the concatenated string. 
   * The [encoding] argument determines the encoding of both the argument
   * and the result string.
   * The map function gets every character as its Unicode code point, and
   * must return the list of code points to map to.
   *
   * The function raises [Malformed_code] when
   * illegal byte sequences or incomplete characters are found.
   *
   * @param range_pos The byte position of the substring to map
   *   (default: 0)
   * @param range_len The byte length of the substring to map
   *   (default: byte length of the input string minus [range_pos])
   *) 

val ustring_sub :
       encoding ->
       int ->
       int ->
       ?range_pos:int -> ?range_len:int ->
       string ->
	 string
  (** [ustring_sub enc start length s]: Returns the substring of [s] starting
   * at character count [start] and consisting of [length] characters. Note
   * that [start] and [length] select the substring by multiples of
   * (usually multibyte) characters, not bytes.
   *
   * If the optional byte-based [range_pos] and [range_len] arguments are
   * present, these arguments are taken to determine a first substring
   * before [start] and [length] are applied to extract the final
   * substring.
   *
   * The function raises [Malformed_code] when
   * illegal byte sequences or incomplete characters are found.
   *
   * @param range_pos The byte position of the substring to extract
   *   (default: 0)
   * @param range_len The byte length of the substring to extract
   *   (default: byte length of the input string minus [range_pos])
   *)

val ustring_compare :
      encoding ->
      (int -> int -> int) ->
       ?range_pos:int -> ?range_len:int ->
      string ->
       ?range_pos:int -> ?range_len:int ->
      string ->
	int
  (** Compares two strings lexicographically. The first argument is the
   * encoding of both strings (which must be the same). The second argument
   * is the function that compares two Unicode code points. It must return
   * 0 if both characters are the same, a negative value if the first
   * character is the smaller one, and a positive value if the second
   * character is the smaller one.
   *
   * The function raises [Malformed_code] when
   * illegal byte sequences or incomplete characters are found.
   *
   * @param range_pos The byte position of the substring to compare
   *   (default: 0), referring to the following string argument
   * @param range_len The byte length of the substring to compare
   *   (default: byte length of the input string minus [range_pos]),
   *   referring to the following string argument
   *)

val uarray_of_ustring : 
    encoding -> 
    ?range_pos:int -> ?range_len:int ->
    string -> 
      int array
  (** Returns the characters of the string as array of Unicode code points.
   * 
   * @param range_pos The byte position of the substring to extract
   *   (default: 0)
   * @param range_len The byte length of the substring to extract
   *   (default: byte length of the input string minus [range_pos])
   *)


val ustring_of_uarray :
    ?subst:(int -> string) ->
    encoding ->
    ?pos:int -> ?len:int ->
    int array ->
      string
  (** Returns the array of Unicode code points as encoded string.
   * 
   * @param pos Selects a subarray: [pos] is the first array position
   *   to encode (default: 0)
   * @param len Selects a subarray: [len] is the length of the subarray
   *   to encode (default: array length minus [pos])
   * @param subst This function is called when a code point cannot be represented
   *   in the chosen character encoding. It must returns the (already encoded)
   *   string to substitute for this code point. By default (if ~subst is
   *   not passed), the exception [Cannot_represent] will be raised in this
   *   case.
   *)

exception Malformed_code_at of int
  (** An illegal byte sequence is found at this byte position *)

val verify : encoding -> ?range_pos:int -> ?range_len:int -> string -> unit
  (** Checks whether the string is properly encoded. If so, () is returned.
   * If not, the exception [Malformed_code_at] will be raised indicating 
   * the byte position where the problem occurs.
   *
   * @param range_pos The byte position of the substring to verify
   *   (default: 0)
   * @param range_len The byte length of the substring to verify
   *   (default: byte length of the input string minus [range_pos])
   *)

(**********************************************************************)

(* Internal *)

(**/**)

val big_slice : int
  (* The length of the normal cursor slices. A "small slice" has always
   * length 1.
   *)

val read_iso88591_ref :
  (int -> encoding -> int array -> int array -> string -> 
     int -> int -> (int*int*encoding)) ref

val read_utf8_ref :
  (bool -> int array -> int array -> string -> int -> int -> (int*int*encoding)) 
  ref

 (* The two read_* variables are initialised with default implementations.
  * They are overriden by Netaccel (if linked)
  *)

