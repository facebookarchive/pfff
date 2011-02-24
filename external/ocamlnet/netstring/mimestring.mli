(* $Id: mimestring.mli 1003 2006-09-24 15:17:15Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

(** Low-level functions to parse and print mail and MIME messages 
 *
 * [Mimestring] contains a lot of functions to scan and print strings
 * formatted as MIME messages. For a higher-level view on this topic,
 * see the [Netmime] module.
 *
 * {b Contents}
 * - {!Mimestring.headers}
 * - {!Mimestring.structured_values}
 * - {!Mimestring.parsers_for_structured_values}
 * - {!Mimestring.printers_for_structured_values}
 * - {!Mimestring.scanning_mime}
 * - {!Mimestring.helpers_mime}
 * 
 *)

(* *********************************************************************)
(* Collection of auxiliary functions to parse MIME headers             *)
(* *********************************************************************)

(* See also the module Netmime for high-level MIME functions *)

(** {1:headers Parsing and Printing Mail Headers} *)


val scan_header : 
       ?downcase:bool ->              (* default: true *)
       ?unfold:bool ->                (* default: true *)
       ?strip:bool ->                 (* default: false *)
       string -> start_pos:int -> end_pos:int -> 
         ((string * string) list * int)
    (** [let params, header_end_pos = scan_header s start_pos end_pos]:
     *
     * Scans the mail header that begins at position [start_pos] in the string 
     * [s] and that must end somewhere before position [end_pos]. It is intended
     * that in [end_pos] the character position following the end of the body of
     * the MIME message is passed.
     *
     * Returns the parameters of the header as [(name,value)] pairs (in
     * [params]), and in [header_end_pos] the position of the character following
     * directly after the header (i.e. after the blank line separating
     * the header from the body).
     *
     * The following normalizations have already been applied:
     * - (D) The names are converted to lowercase characters
     * - (U) Newline characters (CR and LF) in the middle of the header fields
     *     have been removed
     * - (S) Whitespace at the beginning and at the end of field values has been
     *     removed 
     *
     * The default is to apply all three normalizations (D), (U), and (S)
     * (for historic reasons). The three arguments [downcase], [unfold],
     * and [strip] control which normalizations are performed (and for
     * historic reasons, too, this is not what you would expect - backwards
     * compatibility can sometimes be a burden):
     *
     * - If [downcase], do (D); if [not downcase], don't do (D).
     * - If [unfold], do (U); if [not unfold], don't do (U).
     * - If [unfold || strip], do (S); if [not unfold && not strip],
     *   don't do (S)
     * - Defaults: [downcase], [unfold], [not strip].
     *
     * This means that [unfold] not only removes CR/LF from the field value,
     * but also removes whitespace at the beginning and at the end of the
     * field value. [strip] causes not to remove CR/LF if it occurs
     * somewhere within the field value, but all whitespace (including
     * CR/LF) at the beginning of the field value and at the end of the
     * field value is still deleted. Note that if you only want (S)
     * you have to pass [~unfold:false] and [~strip:true].
     *
     * The rules to postprocess mail messages in MIME format are {b not}
     * applied (e.g. encoding transformations as indicated by RFC 2047).
     *
     * The function fails if the header violates the header format
     * strongly. (Some minor deviations are tolerated, e.g. it is sufficient
     * to separate lines by only LF instead of CRLF.)
     *
     * {b The Format of Mail Messages}
     *
     * Messages
     * consist of a header and a body; the first empty line separates both
     * parts. The header contains lines "{i param-name}[:] {i param-value}" where
     * the param-name must begin on column 0 of the line, and the "[:]"
     * separates the name and the value. So the format is roughly:
     *
     * {[
     * param1-name: param1-value
     * ...
     * paramN-name: paramN-value
     * _
     * body ]}
     *
     * (Where "_" denotes an empty line.)
     *
     * This function wants in [start_pos] the position of the first character of
     * [param1-name] in the string, and in [end_pos] the position of the character
     * following [body]. It returns as [header_end_pos] the position where
     * [body] begins. Furthermore, in [params] all parameters are returned the
     * function finds in the header.
     *
     * {b Details}
     *
     * Note that parameter values are restricted; you cannot represent
     * arbitrary strings. The following problems can arise:
     * - Values cannot begin with whitespace characters, because there
     *   may be an arbitrary number of whitespaces between the "[:]" and the
     *   value.
     * - Values (and names of parameters, too) must only be formed of
     *   7 bit ASCII characters. (If this is not enough, the MIME standard
     *   knows the extension RFC 2047 that allows that header values may
     *   be composed of arbitrary characters of arbitrary character sets.
     *   See below how to decode such characters in values returned by
     *   this function.)
     * - Header values may be broken into several lines. Continuation
     *   lines must begin with whitespace characters. This means that values
     *   must not contain line breaks as semantic part of the value.
     *   And it may mean that {i one} whitespace character is not distinguishable
     *   from {i several} whitespace characters.
     * - Header lines must not be longer than 78 characters (soft limit) or
     *   998 characters (hard limit). Values that
     *   would result into longer lines must be broken into several lines.
     *   This means that you cannot represent strings that contain too few
     *   whitespace characters.
     *   (Note: The soft limit is to avoid that user agents have problems
     *   with long lines. The hard limit means that transfer agents sometimes
     *   do not transfer longer lines correctly.)
     * - Some old gateways pad the lines with spaces at the end of the lines.
     *
     * This implementation of a mail scanner tolerates a number of
     * deviations from the standard: long lines are not rejected; 8 bit
     * values are generally accepted; lines may be ended only with LF instead of
     * CRLF.
     *
     * Furthermore, the transformations (D), (U), and (S) can be performed
     * resulting in values that are simpler to process.
     *
     * {b Compatibility}
     *
     * This function can parse all mail headers that conform to RFC 822 or
     * RFC 2822.
     *
     * But there may be still problems, as RFC 822 allows some crazy
     * representations that are actually not used in practice.
     * In particular, RFC 822 allows it to use backslashes to "indicate"
     * that a CRLF sequence is semantically meant as line break. As this
     * function normally deletes CRLFs, it is not possible to recognize such
     * indicators in the result of the function.
     *)

val read_header : 
      ?downcase:bool ->
      ?unfold:bool -> 
      ?strip:bool ->
      Netstream.in_obj_stream -> 
	(string * string) list
    (** This function expects that the current position of the passed
     * [in_obj_stream] is the first byte of the header. The function scans the
     * header and returns it. After that, the stream position is after
     * the header and the terminating empty line (i.e. at the beginning of
     * the message body).
     *
     * The options [downcase], [unfold], and [strip] have the same meaning
     * as in [scan_header].
     *
     * {b Example}
     *
     * To read the mail message "[file.txt]":
     *
     * {[
     * let ch = Netchannels.input_channel (open_in "file.txt") in
     * let stream = Netstream.input_stream ch in
     * let header = read_header stream in
     * stream#close_in()  (* no need to close ch *)
     * ]}
     *)

val write_header :
      ?soft_eol:string ->                          (* default: "\r\n" *)
      ?eol:string ->                               (* default: "\r\n" *)
      Netchannels.out_obj_channel ->
      (string * string) list ->
	unit
    (** This function writes the header to the passed [out_obj_channel]. The
     * empty line following the header is also written.
     *
     * Exact output format: 
     * {ul
     * {- The header is not folded, i.e. no additional CRLF sequences
     *    are inserted into the header to avoid long header lines.
     *    In order to produce correct headers, the necessary CRLF bytes
     *    must already exist in the field values. (You can use the
     *    function [write_value] below for this.)}
     * {- However, this function helps getting some details right. First,
     *    whitespace at the beginning of field values is suppressed.
     *
     *    {b Example:}
     *
     *    [write_header ch ["x","Field value"; "y","   Other value"]] outputs:
     * {[ x: Field value\r\n
     * y: Other value\r\n
     * \r\n]}}
     * {- The end-of-line sequences LF, and CRLF, followed by
     *    whitespace are replaced by the passed [soft_eol] string. If the
     *    necessary space or tab character following the eol is missing, an
     *    additional space character will be inserted.
     *
     *    {b Example:}
     *
     *    [write_header ch ["x","Field\nvalue"; "y","Other\r\n\tvalue"]] outputs:
     * {[ x: Field\r\n
     *  value
     * y: Other\r\n
     * \tvalue]}}
     * {- Empty lines (and lines only consisting of whitespace) are suppressed
     *    if they occur inside the header.
     *
     *    {b Example:}
     *
     *    [write_header ch ["x","Field\n\nvalue"]] outputs:
     * {[ x: Field\r\n
     *  value]}}
     * {- Whitespace at the end of a header field is suppressed. One field
     *    is separated from the next field by printing [eol] once.}
     * }
     *
     * These rules ensure that the printed header will be well-formed with
     * two exceptions:
     * - Long lines (> 72 characters) are neither folded nor rejected
     * - True 8 bit characters are neither properly encoded nor rejected
     *
     * These two problems cannot be addressed without taking the syntax
     * of the header fields into account. See below how to create
     * proper header fields from [s_token] lists.
     *)

(* *********************************************************************)

(** {1:structured_values Parsing Structured Values} *)

(** The following types and functions allow it to build scanners for
 * structured mail and MIME values in a highly configurable way.
 *
 * {b Structured Values}
 *
 * RFC 822 (together with some other RFCs) defines lexical rules
 * how formal mail header values should be divided up into tokens. Formal
 * mail headers are those headers that are formed according to some
 * grammar, e.g. mail addresses or MIME types.
 *
 *    Some of the characters separate phrases of the value; these are
 * the "special" characters. For example, '\@' is normally a special
 * character for mail addresses, because it separates the user name
 * from the domain name (as in [user\@domain]). RFC 822 defines a fixed set
 * of special
 * characters, but other RFCs use different sets. Because of this,
 * the following functions allow it to configure the set of special characters.
 *
 *    Every sequence of characters may be embraced by double quotes,
 * which means that the sequence is meant as literal data item;
 * special characters are not recognized inside a quoted string. You may
 * use the backslash to insert any character (including double quotes)
 * verbatim into the quoted string (e.g. "He said: \"Give it to me!\"").
 * The sequence of a backslash character and another character is called
 * a quoted pair.
 *
 *    Structured values may contain comments. The beginning of a comment
 * is indicated by '(', and the end by ')'. Comments may be nested.
 * Comments may contain quoted pairs. A
 * comment counts as if a space character were written instead of it.
 *
 *    Control characters are the ASCII characters 0 to 31, and 127.
 * RFC 822 demands that mail headers are 7 bit ASCII strings. Because
 * of this, this module also counts the characters 128 to 255 as
 * control characters.
 *
 *    Domain literals are strings embraced by '\[' and '\]'; such literals
 * may contain quoted pairs. Today, domain literals are used to specify
 * IP addresses (rare), e.g. [user\@[192.168.0.44]].
 *
 *    Every character sequence not falling in one of the above categories
 * is an atom (a sequence of non-special and non-control characters).
 * When recognized, atoms may be encoded in a character set different than
 * US-ASCII; such atoms are called encoded words (see RFC 2047).
 *
 * {b Scanning Using the Extended Interface}
 *
 * In order to scan a string containing a structured value, you must first
 * create a [mime_scanner] using the function [create_mime_scanner].
 * The scanner contains the reference to the scanned string, and a 
 * specification how the string is to be scanned. The specification
 * consists of the lists [specials] and [scan_options].
 *
 * The character list [specials] specifies the set of special characters.
 * These are the characters that are not regarded as part of atoms, 
 * because they work as delimiters that separate atoms (like [@] in the
 * above example). In addition to this, when '"', '(', and '\[' are
 * seen as regular characters not delimiting quoted string, comments, and
 * domain literals, respectively, these characters must also be added
 * to [specials]. In detail, these rules apply:
 *
 * {ul
 * {- {b Spaces:}
 *    - If [' '] {i in} [specials]: A space character is returned as [Special ' '].
 *       Note that there may also be an effect on how comments are returned
 *       (see below).
 *    - If [' '] {i not in} [specials]: Spaces are not returned, although
 *      they still delimit atoms.
 *
 *   }
 * {- {b Tabs, CRs, LFs:}
 *    - If ['\t'] {i in} [specials]: A tab character is returned as 
 *      [Special '\t'].
 *    - If ['\t'] {i not in} [specials]: Tabs are not returned, although
 *      they still delimit atoms.
 *    - If ['\r'] {i in} [specials]: A CR character is returned as 
 *      [Special '\r'].
 *    - If ['\r'] {i not in} [specials]: CRs are not returned, although
 *      they still delimit atoms.
 *    - If ['\n'] {i in} [specials]: A LF character is returned as
 *      [Special '\n'].
 *    - If ['\n'] {i not in} [specials]: LFs are not returned, although
 *      they still delimit atoms.
 *
 *   }
 * {- {b Comments:}
 *    {ul
 *    {- If ['('] {i in} [specials]: Comments are not recognized. The 
 *       character '(' is returned as [Special '('].}
 *    {- If ['('] {i not in} [specials]: Comments are recognized. How comments
 *       are returned, depends on the following:
 *       + If [Return_comments] {i in} [scan_options]: Outer comments are
 *         returned as [Comment] (note that inner comments are recognized but
 *         are not returned as tokens)
 *       + If otherwise [' '] {i in} [specials]: Outer comments are returned as
 *         [Special ' ']
 *       + Otherwise: Comments are recognized but not returned at all.
 *
 *       }
 *    }
 *  }
 * {- {b Quoted strings:}
 *    - If ['"'] {i in} [specials]: Quoted strings are not recognized, and
 *      double quotes are returned as [Special '"'].
 *    - If ['"'] {i not in} [specials]: Quoted strings are returned as
 *      [QString] tokens.
 *
 *   }
 * {- {b Domain literals:}
 *    {ul
 *    {- If '\[' {i in} [specials]: Domain literals are not recognized, and
 *       left brackets are returned as [Special] '\['.}
 *    {- If '\[' {i not in} [specials]: Domain literals are returned as
 *       [DomainLiteral] tokens.}
 *    }
 *   }
 * }
 *
 * If recognized, quoted strings are returned as [QString s], where
 * [s] is the string without the embracing quotes, and with already
 * decoded quoted pairs.
 *
 * Control characters [c] are returned as [Control c].
 *
 * If recognized, comments may either be returned as spaces (in the case
 * you are not interested in the contents of comments), or as [Comment] tokens.
 * The contents of comments are not further scanned; you must start a
 * subscanner to analyze comments as structured values.
 *
 * If recognized, domain literals are returned as [DomainLiteral s], where
 * [s] is the literal without brackets, and with decoded quoted pairs.
 *
 * Atoms are returned as [Atom s] where [s] is a longest sequence of
 * atomic characters (all characters which are neither special nor control
 * characters nor delimiters for substructures). If the option
 * [Recognize_encoded_words] is on, atoms which look like encoded words
 * are returned as [EncodedWord] tokens. (Important note: Neither '?' nor
 * '=' must be special in order to enable this functionality.)
 *
 * After the [mime_scanner] has been created, you can scan the tokens by
 * invoking [scan_token] which returns one token at a time, or by invoking
 * [scan_token_list] which returns all following tokens.
 *
 * There are two token types: [s_token] is the base type and is intended to
 * be used for pattern matching. [s_extended_token] is a wrapper that 
 * additionally contains information where the token occurs.
 *
 * {b Scanning Using the Simple Interface}
 *
 * Instead of creating a [mime_scanner] and calling the scan functions,
 * you may also invoke [scan_structured_value]. This function returns the
 * list of tokens directly; however, it is restricted to [s_token].
 * 
 * {b Examples}
 *
 * - Simple address: {[
 * scan_structured_value "user\@domain.com" [ '\@'; '.' ] []
 *   = [ Atom "user"; Special '\@'; Atom "domain"; Special '.'; Atom "com" ]
 * ]}
 * - Spaces are not returned: {[
 * scan_structured_value "user \@ domain . com" [ '\@'; '.' ] []
 *   = [ Atom "user"; Special '\@'; Atom "domain"; Special '.'; Atom "com" ]
 * ]}
 * - Comments are not returned: {[
 * scan_structured_value "user(Do you know him?)\@domain.com" [ '\@'; '.' ] []
 *   = [ Atom "user"; Special '\@'; Atom "domain"; Special '.'; Atom "com" ]
 * ]}
 * - Comments are indicated if requested: {[
 * scan_structured_value "user(Do you know him?)\@domain.com" [ '\@'; '.' ] 
 *     [ Return_comments ]
 *   = [ Atom "user"; Comment; Special '\@'; Atom "domain"; Special '.'; 
 *       Atom "com" ]
 * ]}
 * - Spaces are returned if special: {[
 * scan_structured_value "user (Do you know him?) \@ domain . com" 
 *     [ '\@'; '.'; ' ' ] []
 *   = [ Atom "user"; Special ' '; Special ' '; Special ' '; Special '\@'; 
 *       Special ' '; Atom "domain";
 *       Special ' '; Special '.'; Special ' '; Atom "com" ]
 * ]}
 * - Both spaces and comments are requested: {[
 * scan_structured_value "user (Do you know him?) \@ domain . com" 
 *     [ '\@'; '.'; ' ' ] [ Return_comments ]
 *   = [ Atom "user"; Special ' '; Comment; Special ' '; Special '\@'; 
 *       Special ' '; Atom "domain";
 *       Special ' '; Special '.'; Special ' '; Atom "com" ]
 * ]}
 * - Another case: {[
 * scan_structured_value "user \@ domain . com" [ '\@'; '.'; ' ' ] []
 *   = [ Atom "user"; Special ' '; Special '\@'; Special ' '; Atom "domain";
 *       Special ' '; Special '.'; Special ' '; Atom "com" ]
 * ]}
 * - '(' is special: {[
 * scan_structured_value "user(Do you know him?)\@domain.com" ['\@'; '.'; '(']
 *     []
 *   = [ Atom "user"; Special '('; Atom "Do"; Atom "you"; Atom "know";
 *       Atom "him?)"; Special '\@'; Atom "domain"; Special '.'; Atom "com" ]
 * ]}
 * - Quoted strings: {[
 * scan_structured_value "\"My.name\"\@domain.com" [ '\@'; '.' ] []
 *   = [ QString "My.name"; Special '\@'; Atom "domain"; Special '.';
 *       Atom "com" ]
 * ]}
 * - Encoded words are not returned: {[
 * scan_structured_value "=?ISO-8859-1?Q?Keld_J=F8rn_Simonsen?=" 
 *     [ ] [ ] 
 *   = [ Atom "=?ISO-8859-1?Q?Keld_J=F8rn_Simonsen?=" ]
 * ]}
 * - Encoded words are returned if requested: {[
 * scan_structured_value "=?ISO-8859-1?Q?Keld_J=F8rn_Simonsen?=" 
 *     [ ] [ Recognize_encoded_words ] 
 *   = [ EncodedWord(("ISO-8859-1",""), "Q", "Keld_J=F8rn_Simonsen") ]
 * ]}
 *)



type s_token =
    Atom of string
  | EncodedWord of ((string * string) * string * string)
      (** Args: [((charset,lang),encoding,encoded_word)] *)
  | QString of string
  | Control of char
  | Special of char
  | DomainLiteral of string
  | Comment
  | End
      (** *)
(** A token may be one of:
 * - [QString s]: The quoted string [s], i.e a string between double
 *   quotes. Quoted pairs are already decoded in [s].
 * - [Control c]: The control character [c] (0-31, 127, 128-255)
 * - [Special c]: The special character [c], i.e. a character from 
 *   the [specials] list
 * - [DomainLiteral s]: The bracketed string [s], i.e. a string between
 *   brackets.  Quoted pairs are already decoded in [s].
 * - [Comment]: A string between parentheses. This kind of token is only
 *   generated when the option [Return_comments] is in effect.
 * - [EncodedWord((charset,lang),encoding,encoded_word)]: An RFC-2047 style
 *   encoded word: [charset] is the name of the character set; [lang] is
 *   the language specifier (from RFC 2231) or ""; [encoding] is either
 *   "Q" or "B"; and [encoded_word] is the word encoded in [charset] and
 *   [encoding]. This kind of token is only generated when the option
 *   [Recognize_encoded_words] is in effect (if not, [Atom] is generated
 *   instead).
 * - [Atom s]: A string which is neither quoted not bracketed nor 
 *   written in RFC 2047 notation, and which is not a control or special
 *   character, i.e. the "rest"
 * - [End]: The end of the string
 *)

type s_option =
    No_backslash_escaping
      (** Do not handle backslashes in quoted string and comments as escape
       * characters; backslashes are handled as normal characters.
       * For example: The wrong qstring ["C:\dir\file"] will be returned as
       * [QString "C:\dir\file"] when this option is in effect, and not as
       * [QString "C:dirfile"] as by default. 
       * -- This is a common error in many MIME implementations.
       *)
  | Return_comments
      (** Comments are returned as token [Comment] (unless '(' is included
       * in the list of special characters, in which case comments are
       * not recognized at all).
       * You may get the exact location of the comment by applying
       * [get_pos] and [get_length] to the extended token.
       *)
  | Recognize_encoded_words
      (** Enables that encoded words are recognized and returned as
       * [EncodedWord] instead of [Atom].
       *)



type s_extended_token
  (** An opaque type containing the information of [s_token] plus:
   * - where the token occurs
   * - RFC-2047 access functions
   *)

val get_token : s_extended_token -> s_token
    (** Return the [s_token] within the [s_extended_token] *)

val get_decoded_word : s_extended_token -> string
val get_charset : s_extended_token -> string
    (** Return the decoded word (the contents of the word after decoding the
     * "Q" or "B" representation), and the character set of the decoded word
     * (uppercase).
     *
     * These functions not only work for [EncodedWord]. The function
     * [get_decoded_word] returns for the other kinds of token:
     * - [Atom]: Returns the atom without decoding it
     * - [QString]: Returns the characters inside the double quotes, and
     *   ensures that any quoted pairs are decoded
     * - [Control]: Returns the one-character string
     * - [Special]: Returns the one-character string
     * - [DomainLiteral]: Returns the characters inside the brackets, and
     *   ensures that any quoted pairs are decoded
     * - [Comment]: Returns [""]
     *
     * The function [get_charset] returns ["US-ASCII"] for them.
     *)

val get_language : s_extended_token -> string
    (** Returns the language if the token is an [EncodedWord], and [""] for
     * all other tokens.
     *)

val get_pos : s_extended_token -> int
    (** Return the byte position where the token starts in the string 
     * (the first byte has position 0)
     *)

val get_line : s_extended_token -> int
    (** Return the line number where the token starts (numbering begins
     * usually with 1) 
     *)

val get_column : s_extended_token -> int
    (** Return the column of the line where the token starts (first column
     * is number 0)
     *)

val get_length : s_extended_token -> int
    (** Return the length of the token in bytes *)

val separates_adjacent_encoded_words : s_extended_token -> bool
    (** True iff the current token is white space (i.e. [Special ' '], 
     * [Special '\t'], [Special '\r'] or [Special '\n']) and the last
     * non-white space token was [EncodedWord] and the next non-white
     * space token will be [EncodedWord].
     *
     * The background of this function is that white space between
     * encoded words does not have a meaning, and must be ignored
     * by any application interpreting encoded words.
     *)


type mime_scanner
    (** The opaque type of a scanner for structured values *)

val create_mime_scanner : 
      specials:char list -> 
      scan_options:s_option list -> 
      ?pos:int ->
      ?line:int ->
      ?column:int ->
      string -> 
        mime_scanner
    (** Creates a new [mime_scanner] scanning the passed string.
     *
     * @param specials The list of characters recognized as special characters.
     * @param scan_options The list of global options modifying the behaviour
     *   of the scanner
     * @param pos The position of the byte where the scanner starts in the
     *   passed string. Defaults to 0.
     * @param line The line number of this first byte. Defaults to 1.
     * @param column The column number of this first byte. Default to 0.
     *)

(** Note for [create_mime_scanner]:
 *
 * The optional parameters [pos], [line], [column] are intentionally placed after
 * [scan_options] and before the string argument, so you can specify
 * scanners by partially applying arguments to [create_mime_scanner]
 * which are not yet connected with a particular string:
 * {[
 * let my_scanner_spec = create_mime_scanner my_specials my_options in
 * ...
 * let my_scanner = my_scanner_spec my_string in 
 * ...]}
 *)

val get_pos_of_scanner : mime_scanner -> int
val get_line_of_scanner : mime_scanner -> int
val get_column_of_scanner : mime_scanner -> int
    (** Return the current position, line, and column of a [mime_scanner].
     * The primary purpose of these functions is to simplify switching
     * from one [mime_scanner] to another within a string:
     *
     * {[
     * let scanner1 = create_mime_scanner ... s in
     * ... now scanning some tokens from s using scanner1 ...
     * let scanner2 = create_mime_scanner ... 
     *                  ?pos:(get_pos_of_scanner scanner1)
     *                  ?line:(get_line_of_scanner scanner1)
     *                  ?column:(get_column_of_scanner scanner1)
     *                  s in
     * ... scanning more tokens from s using scanner2 ... ]}
     *
     * {b Restriction:} These functions are not available if the option
     * [Recognize_encoded_words] is on. The reason is that this option
     * enables look-ahead scanning; please use the location of the last
     * scanned token instead.
     *
     * Note: To improve the performance of switching, it is recommended to
     * create scanner specs in advance (see the example [my_scanner_spec]
     * above).
     *)

val scan_token : mime_scanner -> (s_extended_token * s_token)
    (** Returns the next token, or [End] if there is no more token. The 
     * token is returned both as extended and as normal token.
     *)

val scan_token_list : mime_scanner -> (s_extended_token * s_token) list
    (** Returns all following tokens as a list (excluding [End]) *)

val scan_structured_value : string -> char list -> s_option list -> s_token list
    (** This function is included for backwards compatibility, and for all
     * cases not requiring extended tokens.
     *
     * It scans the passed string according to the list of special characters
     * and the list of options, and returns the list of all tokens.
     *)

val specials_rfc822 : char list
val specials_rfc2045 : char list
    (** The sets of special characters defined by the RFCs 822 and 2045.
     *)


(* *********************************************************************)

(* Widely used scanners: *)

(** {1:parsers_for_structured_values Parsing Certain Forms of Structured Values} *)

val scan_encoded_text_value : string -> s_extended_token list
    (** Scans a "text" value. The returned token list contains only
     * [Special], [Atom] and [EncodedWord] tokens. 
     * Spaces, TABs, CRs, LFs are returned (as [Special]) unless
     * they occur between adjacent encoded words in which case
     * they are suppressed. The characters '(', '\[', and '"' are also
     * returned as [Special] tokens, and are not interpreted as delimiters.
     *
     * For instance, this function can be used to scan the "Subject"
     * field of mail messages.
     *)


val scan_value_with_parameters : string -> s_option list ->
                                   (string * (string * string) list)
    (** [let name, params = scan_value_with_parameters s options]:
     * Scans values with annotations like
     *    [name ; p1=v1 ; p2=v2 ; ...]
     * For example, MIME types like "text/plain;charset=ISO-8859-1" can
     * be parsed.
     *
     * The values may or may not be quoted. The characters ";", "=", and
     * even "," are only accepted as part of values when they are quoted.
     * On sytax errors, the function fails.
     *
     * RFC 2231: This function supports some features of this RFC:
     * Continued parameter values are concatenated. For example:
     * 
     * {[
     * Content-Type: message/external-body; access-type=URL;
     *    URL*0="ftp://";
     *    URL*1="cs.utk.edu/pub/moore/bulk-mailer/bulk-mailer.tar" ]}
     *
     * This is returned as:
     * {["message/external-body", 
     *   [ ("access-type", "URL");
     *     ("URL", "ftp://cs.utk.edu/pub/moore/bulk-mailer/bulk-mailer.tar") ]
      ) ]}
     *
     * However, encoded parameter values are not handled specially. The
     * parameter
     *   [title*=us-ascii'en-us'This%20is%20%2A%2A%2Afun%2A%2A%2A]
     * would be returned as
     *   [("title*", "us-ascii'en-us'This%20is%20%2A%2A%2Afun%2A%2A%2A")].
     * Use [scan_values_with_parameters_ep] instead (see below).
     *
     * Raises [Failure] on syntax errors.
     *)

type s_param
  (** The type of encoded parameters (RFC 2231) *)

val param_value : s_param -> string
val param_charset : s_param -> string
val param_language : s_param -> string
  (** Return the decoded value of the parameter, the charset (uppercase),
   * and the language.
   * If the charset is not available, [""] will be returned. 
   * If the language is not available, [""] will be returned. 
   *)

val mk_param : ?charset:string -> ?language:string -> string -> s_param
  (** Creates a parameter from a value (in decoded form). The parameter
   * may have a charset and a language.
   *)

val print_s_param : Format.formatter -> s_param -> unit
  (** Prints a parameter to the formatter (as toploop printer) *)


val scan_value_with_parameters_ep : string -> s_option list ->
                                       (string * (string * s_param) list)
    (** [let name, params = scan_value_with_parameters_ep s options]:
     * This version of the scanner copes with encoded parameters according
     * to RFC 2231.
     * Note: "ep" means "encoded parameters".
     *
     * Example:
     *   [doc.html;title*=us-ascii'en-us'This%20is%20%2A%2A%2Afun%2A%2A%2A]
     *
     * The parameter [title] would be returned as:
     * - name is ["title"]
     * - value is ["This is ***fun***"]
     * - charset is ["US-ASCII"]
     * - language is ["en-us"]
     *
     * Raises [Failure] on syntax errors.
     *)

val scan_mime_type : string -> s_option list ->
                       (string * (string * string) list)
    (** [let name, params = scan_mime_type s options]:
     * Scans MIME types like
     *    [text/plain; charset=iso-8859-1]
     * The name of the type and the names of the parameters are converted
     * to lower case.
     *
     * Raises [Failure] on syntax errors.
     *)

val scan_mime_type_ep : string -> s_option list ->
                         (string * (string * s_param) list)
    (** [let name, params = scan_mime_type_ep s options]:
     * This version copes with RFC-2231-encoded parameters.
     *
     * Raises [Failure] on syntax errors.
     *)

val split_mime_type : string -> (string * string)
    (** [let (main_type, sub_type) = split_mime_type content_type]:
     * Splits the MIME type into main and sub type, for example
     * [ split_mime_type "text/plain" = ("text", "plain") ].
     * The returned strings are always lowercase.
     *
     * Raises [Failure] on syntax errors.
     *)

(* *********************************************************************)

(* Write s_token list *)

(** {1:printers_for_structured_values Printing Structured Values} *)

exception Line_too_long
  (** Raised when the hard limit of the line length is exceeded *)

val write_value : 
      ?maxlen1:int ->                   (* Default: no max length *)
      ?maxlen:int ->                    (* Default: no max length *)
      ?hardmaxlen1:int ->               (* Default: no hard max length *)
      ?hardmaxlen:int ->                (* Default: no hard max length *)
      ?fold_qstring:bool ->             (* Default: true *)
      ?fold_literal:bool ->             (* Default: true *)
      ?unused:int ref ->                (* Default: do not store this value *)
      ?hardunused:int ref ->            (* Default: do not store this value *)
      Netchannels.out_obj_channel ->
      s_token list ->
        unit
    (** Writes the list of [s_token] to the [out_obj_channel]. The value
     * is optionally folded into several lines while writing, but this
     * is off by default. To enable folding, pass {b both} [maxlen1] and
     * [maxlen]:
     * The [maxlen1] parameter specifies the length of the first line
     * to write, the [maxlen] parameter specifies the length of the
     * other lines.
     *
     * If enabled, folding tries to ensure that the value is written
     * in several lines that are not longer as specified by 
     * [maxlen1] and [maxlen]. The value is split into lines by inserting
     * "folding space" at certain locations (which is usually a linefeed
     * followed by a space character, see below). The following
     * table specifies between which tokens folding may happen:
     *
     * {[
     *               +=========================================================+
     * 1st   \   2nd | Atom | QString | DLiteral | EncWord | Special | Spec ' '|
     * ==============+======+=========+==========+=========+=========+=========+
     *          Atom | FS   |  FS     |   FS     |   FS    |    -    |    F    |
     *       QString | FS   |  FS     |   FS     |   FS    |    -    |    F    |
     * DomainLiteral | FS   |  FS     |   FS     |   FS    |    -    |    F    |
     *   EncodedWord | FS   |  FS     |   FS     |   FS    |    -    |    F    |
     *       Special | -    |  -      |   -      |   -     |    -    |    F    |
     *   Special ' ' | -    |  -      |   -      |   -     |    -    |    -    |
     * ==============+======+=========+==========+=========+=========+=========+
     *]}
     *
     * The table shows between which two types of tokens a space or a folding
     * space is inserted:
     * - [FS]: folding space
     * - [F]:  linefeed without extra space
     * - [-]:  nothing can be inserted here
     * 
     * Folding space is ["\n "], i.e. only LF, not CRLF is used as end-of-line
     * character. The function [write_header] will convert these LF to CRLF
     * if needed.
     *
     * [Special '\t'] is handled like [Special ' ']. Control characters are just
     * printed, without folding. Comments, however, are substituted by 
     * either space or folding space. The token [End] is ignored.
     *
     * Furthermore, folding may also happen within tokens:
     * - [Atom], [Control], and [Special] are never split up into parts.
     *   They are simply printed.
     * - [EncodedWord]s, however, are reformatted. This especially means:
     *   adjacent encoded words are first concatenated if possible
     *   (same character set, same encoding, same language), and then
     *   split up into several pieces with optimally chosen lengths.
     *   {b Note:} Because this function gets [s_token] as input and not
     *   [s_extended_token], it is not known whether [Special ' '] tokens
     *   (or other whitespace) between adjacent EncodedWords must be
     *   ignored. Because of this, [write_value] only reformats adjacent encoded 
     *   words when there is not any whitespace between them.
     * - [QString] may be split up in a special way unless [fold_qstring]
     *   is set to [false]. For example, ["One Two  Three"] may be split up into
     *   three lines ["One\n Two\n \ Three"]. Because some header fields
     *   explicitly forbid folding of quoted strings, it is possible to
     *   set [~fold_qstring:false] (it is [true] by default).
     *   {b Note:} Software should not rely on that the different types of
     *   whitespace (especially space and TAB) remain intact at the
     *   beginning of a line. Furthermore, it may also happen that 
     *   additional whitespace is added at the end of a line by the
     *   transport layer.
     * - [DomainLiteral]: These are handled like [QString]. The parameter
     *   [~fold_literal:false] turns folding off if it must be prevented,
     *   it is [true] by default.
     * - [Comment]: Comments are effectively omitted! Instead of [Comment],
     *   a space or folding space is printed. However, you can output comments
     *   by passing sequences like [ Special "("; ...; Special ")" ].
     *
     * It is possible to get the actual number of characters back that
     * can still be printed into the last line without making the line
     * too long. Pass an [int ref] as [unused] to get this value (it may
     * be negative!). Pass an
     * [int ref] as [hardunused] to get the number of characters that may
     * be printed until the hard limit is exceeded.
     *
     * The function normally does not fail when a line becomes too long,
     * i.e. it exceeds [maxlen1] or [maxlen].
     * However, it is possible to specify a hard maximum length
     * ([hardmaxlen1] and [hardmaxlen]). If these are exceeded, the function
     * will raise [Line_too_long].
     *
     * For electronic mail, a [maxlen] of 78 and a [hardmaxlen] of 998 is
     * recommended.
     *
     * {b Known Problems:} 
     * - The reformatter for EncodedWords takes into
     *   account that multi-byte characters must not be split up. However,
     *   this works only when the multi-byte character set is known
     *   to [Netconversion]. You can assume that UTF-8 and UTF-16 always
     *   work. If the character set is not known the reformatter may
     *   split the string at wrong positions.
     * - The reformatter for EncodedWords may parse the token, and if
     *   this fails, you will get the exception [Malformed_code].
     *   This is only done in some special cases, however.
     * - The function prints spaces between adjacent atoms. Although
     *   this is allowed in principal, other MIME implementations might fail when
     *   there are spaces at unexpected locations. Workaround: If
     *   no spaces are desired, concatenate adjacent atoms before
     *   passing them to this function.
     *
     * {b Further Tips:}
     * - Pass ~maxlen1:0 and ~maxlen:0 to get shortest lines
     * - Use the reformatter for encoded words! It works well. For
     *   example, to output a long sentence, just wrap it into
     *   {b one} [EncodedWord]. The reformatter takes care to
     *   fold the word into several lines.
     *)

val param_tokens : ?maxlen:int -> (string*s_param) list -> s_token list
    (** Formats a parameter list. For example, 
     * [[ "a", "b"; "c", "d" ]] is transformed to the token sequence
     * corresponding to [; a=b; c=d].
     * If [maxlen] is specified, it is ensured that the individual
     * parameter (e.g. ["a=b;"]) is not longer than [maxlen-1], such that
     * it will fit into a line with maximum length [maxlen].
     * By default, no maximum length is guaranteed.
     * If [maxlen] is passed, or if a parameter specifies a character
     * set or language, the encoding of RFC 2231 will be applied. If these
     * conditions are not met, the parameters will be encoded traditionally.
     *)

val split_uri : string -> s_token list
    (** Splits a long URI according to the algorithm of RFC 2017.
     * The input string must only contain 7 bit characters, and
     * must be, if necessary, already be URL-encoded.
     *)


(* *********************************************************************)

(* Scanners for MIME bodies *)

(** {1:scanning_mime Scanning MIME Messages} *)

val scan_multipart_body : string -> start_pos:int -> end_pos:int -> 
                            boundary:string ->
                            ((string * string) list * string) list
(** [let [params1, value1; params2, value2; ...]
 *   = scan_multipart_body s start_pos end_pos boundary]:
 *
 * Scans the string [s] that is the body of a multipart message.
 * The multipart message begins at position [start_pos] in [s], and 
 * [end_pos] is the position
 * of the character following the message. In [boundary] the boundary string
 * must be passed (this is the "boundary" parameter of the multipart
 * MIME type, e.g. [multipart/mixed;boundary="some string"] ).
 *
 *     The return value is the list of the parts, where each part
 * is returned as pair [(params, value)]. The left component [params]
 * is the list of name/value pairs of the header of the part. The
 * right component is the raw content of the part, i.e. if the part
 * is encoded ("content-transfer-encoding"), the content is returned
 * in the encoded representation. The caller is responsible for decoding
 * the content.
 *
 *     The material before the first boundary and after the last
 * boundary is not returned.
 *
 * {b Multipart Messages}
 *
 * The MIME standard defines a way to group several message parts to
 * a larger message (for E-Mails this technique is known as "attaching"
 * files to messages); these are the so-called multipart messages.
 * Such messages are recognized by the major type string "multipart",
 * e.g. [multipart/mixed] or [multipart/form-data]. Multipart types MUST
 * have a [boundary] parameter because boundaries are essential for the
 * representation.
 *
 *    Multipart messages have a format like (where "_" denotes empty lines):
 * {[
 * ...Header...
 * Content-type: multipart/xyz; boundary="abc"
 * ...Header...
 * _
 * Body begins here ("prologue")
 * --abc
 * ...Header part 1...
 * _
 * ...Body part 1...
 * --abc
 * ...Header part 2...
 * _
 * ...Body part 2
 * --abc
 * ...
 * --abc--
 * Epilogue ]}
 *
 * The parts are separated by boundary lines which begin with "--" and
 * the string passed as boundary parameter. (Note that there may follow
 * arbitrary text on boundary lines after "--abc".) The boundary is
 * chosen such that it does not occur as prefix of any line of the
 * inner parts of the message.
 *
 *     The parts are again MIME messages, with header and body. Note
 * that it is explicitely allowed that the parts are even multipart
 * messages.
 *
 *     The texts before the first boundary and after the last boundary
 * are ignored.
 *
 *     Note that multipart messages as a whole MUST NOT be encoded.
 * Only the PARTS of the messages may be encoded (if they are not
 * multipart messages themselves).
 *
 * Please read RFC 2046 if want to know the gory details of this
 * brain-dead format.
 *)

val scan_multipart_body_and_decode : string -> start_pos:int -> end_pos:int -> 
                                        boundary:string ->
                                        ((string * string) list * string) list
  (** Same as [scan_multipart_body], but decodes the bodies of the parts
   * if they are encoded using the methods "base64" or "quoted printable".
   * Fails, if an unknown encoding is used.
   *)

val scan_multipart_body_from_netstream
    : Netstream.in_obj_stream ->
      boundary:string ->
      create:((string * string) list -> 'a) ->
      add:('a -> Netstream.in_obj_stream -> int -> int -> unit) ->
      stop:('a -> unit) ->
      unit
    (** [scan_multipart_body_from_netstream s boundary create add stop]:
     *
     * Reads the MIME message from the netstream [s] block by block. The
     * parts are delimited by the [boundary].
     *
     * Once a new part is detected and begins, the function [create] is
     * called with the MIME header as argument. The result [p] of this function
     * may be of any type.
     *
     * For every chunk of the part that is being read, the function [add]
     * is invoked: [add p s k n].
     *
     * Here, [p] is the value returned by the [create] invocation for the
     * current part. [s] is the netstream. The current window of [s] contains
     * the read chunk completely; the chunk begins at position [k] of the
     * window (relative to the beginning of the window) and has a length
     * of [n] bytes.
     *
     * When the part has been fully read, the function [stop] is
     * called with [p] as argument.
     *
     * That means, for every part the following is executed:
     * - [let p = create h]
     * - [add p s k1 n1]
     * - [add p s k2 n2]
     * - ...
     * - [add p s kN nN]
     * - [stop p]
     *
     * {b Important Precondition:}
     * - The block size of the netstream [s] must be at least
     *   [String.length boundary + 4]
     *
     * {b Exceptions:}
     * - Exceptions can happen because of ill-formed input, and within
     *   the callbacks of the functions [create], [add], [stop].
     * - If the exception happens while part [p] is being read, and the
     *   [create] function has already been called (successfully), the
     *   [stop] function is also called (you have the chance to close files).
     *   The exception is re-raised after [stop] returns.
     *)

val read_multipart_body : 
      (Netstream.in_obj_stream -> 'a) -> string -> Netstream.in_obj_stream -> 
         'a list
    (** This is the "next generation" multipart message parser. It is 
     * called as follows:
     *
     *   [let parts = read_multipart_body f boundary s]
     *
     * As precondition, the current position of the stream [s] must be at
     * the beginning of the message body. The string [boundary] must
     * be the message boundary (without "--"). The function [f] is called
     * for every message part, and the resulting list [parts] is the
     * concatentation of the values returned by [f]. 
     *
     * The stream passed to [f] is a substream of [s] that begins at the
     * first byte of the header of the message part. The function [f]
     * can read data from the substream as necessary. The substream
     * terminates at the end of the message part. This means that [f] can simply
     * read the data of the substream from the beginning to the end. It is
     * not necessary that [f] reads the substream until EOF, however.
     *
     * After all parts have been read, the trailing material of stream [s] 
     * is skipped until EOF of [s] is reached.
     *)

(* *********************************************************************)

(* Tools to create multipart messages *)

(** {1:helpers_mime Helpers for MIME Messages} *)

val create_boundary : ?random:string list -> ?nr:int -> unit -> string
  (** Creates a boundary string that can be used to separate multipart
   * messages.
   * The string is 63 characters long and has the following "features":
   * - Most of the string consists of the minus character yielding
   *   a clear optical effect
   * - The string contains "=__". This sequence cannot be obtained
   *   by the quoted-printable encoding, so you need not to care whether
   *   strings encoded as quoted-printable contain the boundary.
   * - The string contains "<&>;" which is illegal in HTML, XML, and
   *   SGML.
   * - The string does not contain double quotes or backslashes,
   *   so you can safely put double quotes around it in the MIME header.
   * - The string contains [nr], so you can safely distinguish between
   *   several boundaries occurring in the same MIME body if you 
   *   assign different [nr].
   * - The string contains a hash value composed of the first
   *   256 bytes of all strings passed as [random], and influenced
   *   by the current GC state.
   *)


(* THREAD-SAFETY:
 * The functions are thread-safe as long as the threads do not share
 * values.
 *)
