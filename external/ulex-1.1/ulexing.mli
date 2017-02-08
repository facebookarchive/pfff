(** Runtime support for lexers generated by [ulex].
  This module is roughly equivalent to the module Lexing from 
  the OCaml standard library, except that its lexbuffers handles
  Unicode code points (OCaml type: [int] in the range
  [0..0x10ffff]) instead of bytes (OCaml type: [char]).

  It is possible to have ulex-generated lexers work on a custom
  implementation for lex buffers. To do this, define a module [L] which
  implements the [start], [next], [mark] and [backtrack] functions
  (See the Internal Interface section below for a specification),
  and the [Error] exception.  
  They need not work on a type named [lexbuf]: you can use the type
  name you want. Then, just do in your ulex-processed source, before
  the first lexer specification:

  [module Ulexing = L]

  Of course, you'll probably want to define functions like [lexeme]
  to be used in the lexers semantic actions.
*)

type lexbuf
  (** The type of lexer buffers. A lexer buffer is the argument passed
    to the scanning functions defined by the generated lexers.
    The lexer buffer holds the internal information for the
    scanners, including the code points of the token currently scanned,
    its position from the beginning of the input stream,
    and the current position of the lexer. *)

exception Error
  (** Raised by a lexer when it cannot parse a token from the lexbuf. 
    The functions [Ulexing.lexeme_start] (resp. [Ulexing.lexeme_end]) can be 
    used to find to positions of the first code point of the current
    matched substring (resp. the first code point that yield the error). *)

exception InvalidCodepoint of int
  (** Raised by some functions to signal that some code point is not
    compatible with a specified encoding. *)

(** {6 Clients interface} *)

val create: (int array -> int -> int -> int) -> lexbuf
  (** Create a generic lexer buffer.  When the lexer needs more
    characters, it will call the given function, giving it an array of
    integers [a], a position [pos] and a code point count [n].  The
    function should put [n] code points or less in [a], starting at
    position [pos], and return the number of characters provided. A
    return value of 0 means end of input. *)
  
val from_stream: int Stream.t -> lexbuf
  (** Create a lexbuf from a stream of Unicode code points. *)

val from_int_array: int array -> lexbuf
  (** Create a lexbuf from an array of Unicode code points. *)

val from_latin1_stream: char Stream.t -> lexbuf
  (** Create a lexbuf from a Latin1 encoded stream (ie a stream
    of Unicode code points in the range [0..255]) *)

val from_latin1_channel: in_channel -> lexbuf
  (** Create a lexbuf from a Latin1 encoded input channel.
    The client is responsible for closing the channel. *)

val from_latin1_string: string -> lexbuf
  (** Create a lexbuf from a Latin1 encoded string. *)

val from_utf8_stream: char Stream.t -> lexbuf
  (** Create a lexbuf from a UTF-8 encoded stream. *)

val from_utf8_channel: in_channel -> lexbuf
  (** Create a lexbuf from a UTF-8 encoded input channel. *)

val from_utf8_string: string -> lexbuf
  (** Create a lexbuf from a UTF-8 encoded string. *)

type enc = Ascii | Latin1 | Utf8
val from_var_enc_stream: enc ref -> char Stream.t -> lexbuf
  (** Create a lexbuf from a stream whose encoding is subject
    to change during lexing. The reference can be changed at any point.
    Note that bytes that have been consumed by the lexer buffer
    are not re-interpreted with the new encoding.

    In [Ascii] mode, non-ASCII bytes (ie [>127]) in the stream
    raise an [InvalidCodepoint] exception. *)

val from_var_enc_string: enc ref -> string -> lexbuf
  (** Same as [Ulexing.from_var_enc_stream] with a string as input. *)

val from_var_enc_channel: enc ref -> in_channel -> lexbuf
  (** Same as [Ulexing.from_var_enc_stream] with a channel as input. *)

(** {6 Interface for lexers semantic actions} *)

(** The following functions can be called from the semantic actions of
  lexer definitions.  They give access to the character string matched
  by the regular expression associated with the semantic action. These
  functions must be applied to the argument [lexbuf], which, in the
  code generated by [ulex], is bound to the lexer buffer passed to the
  parsing function.

  These functions can also be called when capturing a [Ulexing.Error] 
  exception to retrieve the problematic string. *)

val lexeme_start: lexbuf -> int
  (** [Ulexing.lexeme_start lexbuf] returns the offset in the
    input stream of the first code point of the matched string.
    The first code point of the stream has offset 0. *)

val lexeme_end: lexbuf -> int
(** [Ulexing.lexeme_end lexbuf] returns the offset in the input stream
   of the character following the last code point of the matched
   string. The first character of the stream has offset 0. *)

val loc: lexbuf -> int * int
(** [Ulexing.loc lexbuf] returns the pair 
  [(Ulexing.lexeme_start lexbuf,Ulexing.lexeme_end lexbuf)]. *)

val lexeme_length: lexbuf -> int
(** [Ulexing.loc lexbuf] returns the difference 
  [(Ulexing.lexeme_end lexbuf) - (Ulexing.lexeme_start lexbuf)],
  that is, the length (in code points) of the matched string. *)

val lexeme: lexbuf -> int array
(** [Ulexing.lexeme lexbuf] returns the string matched by
  the regular expression as an array of Unicode code point. *)

val get_buf: lexbuf -> int array
  (** Direct access to the internal buffer. *)
val get_start: lexbuf -> int
  (** Direct access to the starting position of the lexeme in the
      internal buffer. *)
val get_pos: lexbuf -> int
  (** Direct access to the current position (end of lexeme) in the
      internal buffer. *)

val lexeme_char: lexbuf -> int -> int
  (** [Ulexing.lexeme_char lexbuf pos] returns code point number [pos] in
      the matched string. *)

val sub_lexeme: lexbuf -> int -> int -> int array
(** [Ulexing.lexeme lexbuf pos len] returns a substring of the string
  matched by the regular expression as an array of Unicode code point. *)


val latin1_lexeme: lexbuf -> string
(** As [Ulexing.lexeme] with a result encoded in Latin1.
  This function throws an exception [InvalidCodepoint] if it is not possible
  to encode the result in Latin1. *)

val latin1_sub_lexeme: lexbuf -> int -> int -> string
(** As [Ulexing.sub_lexeme] with a result encoded in Latin1.
  This function throws an exception [InvalidCodepoint] if it is not possible
  to encode the result in Latin1. *)

val latin1_lexeme_char: lexbuf -> int -> char
(** As [Ulexing.lexeme_char] with a result encoded in Latin1.
  This function throws an exception [InvalidCodepoint] if it is not possible
  to encode the result in Latin1. *)

 
val utf8_lexeme: lexbuf -> string
(** As [Ulexing.lexeme] with a result encoded in UTF-8. *)

val utf8_sub_lexeme: lexbuf -> int -> int -> string
(** As [Ulexing.sub_lexeme] with a result encoded in UTF-8. *)


val rollback: lexbuf -> unit
(** [Ulexing.rollback lexbuf] puts [lexbuf] back in its configuration before
  the last lexeme was matched. It is then possible to use another
  lexer to parse the same characters again. The other functions
  above in this section should not be used in the semantic action
  after a call to [Ulexing.rollback]. *)

(** {6 Internal interface} *)

(** These functions are used internally by the lexers. They could be used
  to write lexers by hand, or with a lexer generator different from
  [ulex]. The lexer buffers have a unique internal slot that can store
  an integer. They also store a "backtrack" position.
*)

val start: lexbuf -> unit
(** [Ulexing.start lexbuf] informs the lexer buffer that any
  code points until the current position can be discarded.
  The current position become the "start" position as returned
  by [Ulexing.lexeme_start]. Moreover, the internal slot is set to
  [-1] and the backtrack position is set to the current position.
*)

val next: lexbuf -> int
(** [Ulexing.next lexbuf next] extracts the next code point from the
  lexer buffer and increments to current position. If the input stream
  is exhausted, the function returns [-1]. *)

val mark: lexbuf -> int -> unit
(** [Ulexing.mark lexbuf i] stores the integer [i] in the internal
  slot. The backtrack position is set to the current position. *)

val backtrack: lexbuf -> int
(** [Ulexing.backtrack lexbuf] returns the value stored in the
  internal slot of the buffer, and performs backtracking
  (the current position is set to the value of the backtrack position). *)
