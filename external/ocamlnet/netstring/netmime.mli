(* $Id: netmime.mli 1003 2006-09-24 15:17:15Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

(** Netmime contains high-level classes and functions to process
 * mail and MIME messages.
 *
 * {b Contents}
 *
 * - {!Netmime.types}
 * - {!Netmime.classes}
 * - {!Netmime.parsing}
 * - {!Netmime.printing}
 *
 * The tutorial has been moved to {!Netmime_tut}.
 *)

(* ***************************** Types ******************************** *)

(** {1:types Types} *)

open Netchannels

type store =
  [ `Memory
  | `File of string
      (* The string is the filename of a file containing the (decoded) value
       * of the body
       *)
  ]
  (** Specifies where to store the body of a mail message. [`Memory]
   * means in-memory, [`File name] means in the file [name]. The body
   * is stored in decoded form (i.e. without transfer encoding).
   *)

exception Immutable of string
  (** Raised if it is tried to modify a read-only value. The string denotes
   * the function or method where the incident happened.
   *)


(** MIME headers and bodies are defined in two steps. First the subtype
 * describing read access is defined ([mime_header_ro], and [mime_body_ro]),
 * and after that the full class type including write access is defined
 * ([mime_header], and [mime_body]).
 *
 * The idea is that you can write functions that take an ro value as
 * input to indicate that they do not modify the value. For example:
 * 
 * {[
 * let number_of_fields (h:#mime_header_ro) =
 *   List.length (h#fields) ]}
 *
 * This function accepts both [mime_header], and [mime_header_ro] values as
 * input, but the typing ensures that the function cannot mutate anything.
 *
 * There is another way to ensure that a header or body is not modified.
 * The read-only flag can be set when creating the object, and this flag
 * causes that all trials to modify the value will raise the exception
 * [Immutable]. Of course, such trials of mutation are only detected at
 * run-time.
 *
 * The advantage of the read-only flag is that it even works if 
 * mutation depends on a condition, but it can be ensured that this
 * condition is never true. Furthermore, typing is much simpler (getting
 * subtyping correct can be annoying).
 *)


(** This is the read-only version of a MIME header. There are only methods
 * to read the header fields.
 *)
class type mime_header_ro =             
object
  (* read-only view of a mime_header *)

  method fields : (string * string) list
  method field  : string -> string
  method multiple_field : string -> string list
    (** The current fields of the header. [fields] returns the complete
     * header. [field name] returns the value of the field, or raises
     * [Not_found]. [multiple_field name] returns all fields with the same
     * name.
     *
     * Note that field names are case-insensitive; [field "content-length"],
     * and [field "CONTENT-LENGTH"] will return the same field. However,
     * the method [fields] returns the original field names, without
     * adjustment of the case.
     *
     * The order of the fields is preserved.
     *)

  (* --------------------- Standard fields ----------------------- *)

  (** Access methods for frequent standard fields.
   *
   * These methods will raise [Not_found] if the fields are not
   * present.
   *)

  method content_length : unit -> int
    (** Returns the Content-length field as integer *)

  method content_type : 
           unit -> (string * (string * Mimestring.s_param)list)
    (** Returns the Content-type as parsed value. The left value of the
     * pair is the main type, and the right value is the list of 
     * parameters. For example, for the field value
     * ["text/plain; charset=utf-8"] this method returns
     * [("text/plain", ["charset", p])] where [p] is an opaque value
     * with [Mimestring.param_value p = "utf-8"]. 
     *)

  method content_disposition : 
           unit -> (string * (string * Mimestring.s_param)list)
    (** Returns the Content-disposition field as parsed value. The
     * left value is the main disposition, and the right value is the
     * list of parameters. For example, for the field value
     * ["attachment; filename=xy.dat"] this method returns
     * [("attachment", ["filename", p])] where [p] is an opaque value
     * with [Mimestring.param_value p = "xy.dat"].
     *)

  method content_transfer_encoding : unit -> string
    (** Returns the Content-transfer-encoding as string *)
end


(** A MIME header with both read and write method. It is still possible,
 * however, to set the read-only flag to make this kind of header
 * immutable, too.
 *)
class type mime_header = 
object
  (* A mutable or immutable mime_header *)

  inherit mime_header_ro
    (** Supports all these read access method, too *)

  method ro : bool
    (** whether the header is read-only or not *)

  method set_fields : (string * string) list -> unit
  method update_field : string -> string -> unit
  method update_multiple_field : string -> string list -> unit
  method delete_field : string -> unit
    (** These methods modify the fields of the header. If the header is
     * read-only, the exception [Immutable] will be raised.
     *
     * [set_fields] replaces the current fields with a new list of
     * (name,value) pairs. [update_field name value] replaces all fields
     * of the passed name with the single setting (name,value), or
     * adds this setting to the list. [update_multiple_field name values]
     * replaces all fields of the passed name with the list of values,
     * or adds this list. Finally, [delete_field name] deletes all
     * fields of the passed name. Nothing happens if there is no such
     * field.
     *
     * Both [update_field] and [update_multiple_field] first replace
     * existing values by the new ones without changing the order
     * of the fields in the header. Additional values are inserted
     * after the last existing value, or at the end of the header.
     *)

end


(** This is the read-only version of a MIME body. There are only methods
 * to read the body contents.
 *
 * The value of the body can be returned either as [string], or as
 * object channel. Both ways are possible independently of where
 * the value is stored, in-memory, or as external file.
 *)
class type mime_body_ro =
object
  (* a read-only view of a mime_body *)

  method value : string
    (** The [value] method returns the _decoded_ body,
     * i.e. transfer encodings are removed before the value is passed
     * back.
     *
     * When the body is stored in an external file, this method
     * reads the complete file into memory.
     *)

  method store : store
    (** Where the body is actually stored. *)

  method open_value_rd : unit -> in_obj_channel
    (** Opens the value for reading. This works independently of where
     * the body is stored. For example, to read the body line by line:
     * {[
     * let ch = body # open_value_rd () in
     * try
     *   while true do
     *     let line = ch # input_line() in
     *     ... (* do something *)
     *   done;
     *   assert false; (* never reached *)
     * with
     *   End_of_file ->
     *     ch # close_in()
     * ]}
     *
     * As [value], this method returns the value in decoded form.
     * This method is quite economical with the resources, and takes
     * only as much memory as needed for the channel operations.
     *)

  method finalize : unit -> unit
    (** After the body has been finalized, it cannot be accessed any longer.
     * External resources (files) are deallocated, if they are seen as
     * temporary.
     *)
end


(** A MIME body with both read and write method. It is still possible,
 * however, to set the read-only flag to make this kind of body
 * immutable, too.
 *
 * The value of the body can be set either by a [string], or by writing
 * to an object channel. Both ways are possible independently of where
 * the value is stored, in-memory, or as external file.
 *)
class type mime_body =
object
  (* A mutable or immutable mime_body *)

  inherit mime_body_ro
    (** Supports all these read access method, too *)

  method ro : bool
    (** whether this body is read-only or not *)

  method set_value : string -> unit
    (** Sets the value. If the value is immutable, the exception
     * [Immutable] will be raised.
     *
     * The passed string must be in decoded form. When the body is
     * stored in an external file, the file is overwritten.
     *)

  method open_value_wr : unit -> out_obj_channel
    (** Opens the value for writing. The current value is overwritten. 
     * If the value is immutable, the exception [Immutable] will be raised.
     *
     * For example, to copy the file [f] into the value:
     * {[ 
     * let ch = body # open_value_wr() in
     * let f_ch = new Netchannels.input_file f in
     * ch # output_channel f_ch;
     * f_ch # close_in();
     * ch # close_out();
     * ]}
     * 
     *)

end


(** One can consider the pair [(mime_header, mime_body)] as simple MIME
 * message with one header and one body. Of course, this simple representation
 * does not support multi-part messages (attachments). For that reason,
 * the [complex_mime_message] was invented: The body can be further
 * structured as a sequence of parts that are complex messages themselves.
 *
 * For example, a mail message with an attachment is usually
 * represented as
 * {[
 * (mail_header, `Parts [ (main_header, `Body main_body);
 *                        (att_header, `Body att_body) ] ) ]}
 *
 * Here, [mail_header] is the real header of the mail message.
 * [main_header] is the header of the main message, usually
 * only containing the content type of [main_body], the body
 * of the main message. The attachment has also its own 
 * [att_header], again usually only containing the content type,
 * and the data of the attachment can be found in [att_body].
 *
 * Nowadays, mails have often even a more complicated structure
 * with [`Parts] containing nested [`Parts]. As [complex_mime_message]
 * is recursive, any kind of nesting can be easily represented.
 *)

type complex_mime_message = mime_header * complex_mime_body
and complex_mime_body =
  [ `Body of mime_body
  | `Parts of complex_mime_message list
  ]
  (* A complex_mime_message can have (nested) multipart structure. *)

type complex_mime_message_ro = mime_header_ro * complex_mime_body_ro
and complex_mime_body_ro =
  [ `Body of mime_body_ro
  | `Parts of complex_mime_message_ro list
  ]
  (** The read-only view of a complex_mime_message *)


(** Note: [`Parts []], i.e. [`Parts] together with an empty list, is 
 * considered as illegal. Such a value cannot be transformed into
 * printable text.
 *)


type mime_message = mime_header * [ `Body of mime_body ]
  (** Simple MIME message, in a form that is compatible with complex
   * ones.
   *)

type mime_message_ro = mime_header_ro * [ `Body of mime_body_ro ]
  (** Read-only variant of simple messages *)


(* ************************* Representations ************************** *)

(** {1:classes Classes} *)

class basic_mime_header : ?ro:bool -> (string * string) list -> mime_header
  (** An implementation of [mime_header].
   *
   * The argument is the list of (name,value) pairs of the header. 
   *
   * Example: Create a MIME header with only the field "Content-type":
   * {[ let h = new basic_mime_header ["content-type", "text/plain"] ]}
   *
   * Example: Set the field "Subject":
   * {[ h # update_field "subject" "The value of this field" ]}
   *
   * This [mime_header] implementation bases on a mixture of a [Map] data
   * structure and a doubly linked list. The efficiency of the operations
   * (n=number of fields; m=average number of values per field; 
   * n*m=total number of values):
   * - [new], [set_fields]: O(m * n * log n), but the construction of the dictionary
   *   is deferred until the first real access
   * - [field]: O(log n)
   * - [multiple_field]: O(log n + m)
   * - [fields]: O(n * m)
   * - [update_field], [update_multiple_field]: O(log n + m)
   * - [delete_field]: O(n + m)
   *
   * @param ro whether the header is read-only (default: false)
   *)

class memory_mime_body : ?ro:bool -> string -> mime_body
  (** An implementation of [mime_body] where the value is stored
   * in-memory.
   *
   * The argument is the initial (decoded) value of the body.
   * The method [store] returns [`Memory].
   *
   * Example: To create a body from a string, call
   * {[ new memory_mime_body "The value as string" ]}
   *
   * @param ro whether the body is read-only (default: false)
   *)


class file_mime_body : ?ro:bool -> ?fin:bool -> string -> mime_body
  (** An implementation of [mime_body] where the value is stored
   * in an external file.
   *
   * The argument is the name of the file containing the (decoded) value. 
   * The method [store] returns [`File filename].
   * The method [value] loads the contents of the file and returns them
   * as string.
   *
   * Example: To create a body from the file "f", call
   * {[ new file_mime_body "f" ]}
   *
   * @param ro whether the body is read-only (default: false)
   * @param fin whether to delete the file when the [finalize] method is called
   *   (default: false)
   *)

(* ******************************************************************** *)

(** {1:parsing Parsing MIME messages} *)

val read_mime_header :
      ?unfold:bool ->                        (* default: false *)
      ?strip:bool ->                         (* default: true *)
      ?ro:bool ->                            (* default: false *)
      Netstream.in_obj_stream -> 
	mime_header
  (** Decodes the MIME header that begins at the current position of the
   * netstream, and returns the header as class [basic_mime_header].
   * After returning, the stream is advanced to the byte following the 
   * empty line terminating the header.
   *
   * Example: To read the header at the beginning of the file "f", use:
   * {[ 
   * let ch = new Netchannels.input_channel (open_in "f") in
   * let stream = new Netstream.input_stream ch in
   * let h = read_mime_header stream in
   * ...
   * stream#close_in();    (* no need to close ch *)
   * ]}
   *
   * Note that although the [stream] position after parsing is exactly 
   * known, the position of [ch] cannot be predicted.
   *
   * @param unfold whether linefeeds are replaced by spaces in the values of the
   *   header fields (Note: defaults to [false] here in contrast to
   *   [Mimestring.scan_header]!)
   * @param strip whether whitespace at the beginning and at the end of the 
   *   header fields is stripped
   * @param ro whether the returned header is read-only (default: false)
   *)

(** Hint: To write the header [h] into the channel [ch], use
 * {[ Mimestring.write_header ch h#fields ]}
 *
 * Link: {!Mimestring.write_header}
 *)

type multipart_style = [ `None | `Flat | `Deep ]
  (** How to parse multipart messages:
   * - [`None]: Do not handle multipart messages specially. Multipart bodies
   *    are not further decoded, and returned as [`Body b] where [b] is
   *    the transfer-encoded text representation.
   * - [`Flat]: If the top-level message is a multipart message, the parts
   *    are separated and returned as list. If the parts are again multipart
   *    messages, these inner multipart messages are not furher decoded 
   *    and returned as [`Body b].
   * - [`Deep]: Multipart messages are recursively decoded and returned as
   *    tree structure.
   *
   * This value determines how far the [complex_mime_message] structure
   * is created for a parsed MIME message. [`None] means that no parts
   * are decoded, and messages have always only a simple [`Body b],
   * even if [b] is in reality a multi-part body. With [`Flat], the
   * top-level multi-part bodies are decoded (if found), and messages
   * can have a structured [`Parts [_, `Body b1; _, `Body b1; ...]]
   * body. Finally, [`Deep] allows that inner multi-part bodies are
   * recursively decoded, and messages can have an arbitrarily complex
   * form.
   *)

val decode_mime_body : #mime_header_ro -> out_obj_channel -> out_obj_channel
  (** [let ch' = decode_mime_body hdr ch]:
   * According to the value of the Content-transfer-encoding header field
   * in [hdr] the encoded MIME body written to [ch'] is decoded and transferred
   * to [ch].
   * 
   * Handles 7bit, 8bit, binary, quoted-printable, base64.
   *
   * Example: The file "f" contains base64-encoded data, and is to be decoded 
   * and to be stored in "g":
   *
   * {[ 
   * let ch_f = new Netchannels.input_channel (open_in "f") in
   * let ch_g = new Netchannels.output_channel (open_out "g") in
   * let hdr = new basic_mime_header ["content-transfer-encoding", "base64" ] in
   * let ch = decode_mime_body hdr ch_g in
   * ch # output_channel ch_f;
   * ch # close_out();
   * ch_g # close_out();
   * ch_f # close_in();
   * ]}
   *
   * Note: This function is internally used by [read_mime_message] to
   * decode bodies. There is usually no need to call it directly.
   *)


val storage : ?ro:bool -> ?fin:bool -> store -> (mime_body * out_obj_channel)
  (** Creates a new storage facility for a mime body according to [store].
   * This function can be used to build the [storage_style] argument 
   * of the class [read_mime_message] (below). For example, this is
   * useful to store large attachments in external files, as in:
   *
   * {[ 
   * let storage_style hdr = 
   *   let filename = hdr ... (* extract from hdr *) in
   *   storage (`File filename)
   * ]}
   *
   * @param ro whether the returned mime_bodies are read-only or not. Note that
   *   it is always possible to write into the body using the returned
   *   out_obj_channel regardless of the value of ~ro.
   *   Default: false
   * @param fin whether to finalize bodies stored in files.
   *   Default: false
   *)

val read_mime_message : 
      ?unfold:bool ->                                     (* Default: false *)
      ?strip:bool ->                                      (* default: true *)
      ?ro:bool ->                                         (* Default: false *)
      ?multipart_style:multipart_style ->                 (* Default: `Deep *)
      ?storage_style:(mime_header -> (mime_body * out_obj_channel)) ->
      Netstream.in_obj_stream -> 
        complex_mime_message
  (** Decodes the MIME message that begins at the current position of the
   * passed netstream. It is expected that the message continues until
   * EOF of the netstream.
   *
   * Multipart messages are decoded as specified by [multipart_style] (see
   * above).
   *
   * Message bodies with content-transfer-encodings of 7bit, 8bit, binary,
   * base64, and quoted-printable can be processed. The bodies are stored
   * without content-transfer-encoding (i.e. in decoded form), but the
   * content-transfer-encoding header field is not removed from the header.
   *
   * The [storage_style] function determines where every message body is
   * stored. The corresponding header of the body is passed to the function
   * as argument; the result of the function is a pair of a new [mime_body]
   * and an [out_obj_channel] writing into this body. You can create such a
   * pair by calling [storage] (above).
   *
   * By default, the [storage_style] is [storage ?ro `Memory] for every header. 
   * Here, the designator [`Memory] means that the body will be stored in an
   * O'Caml string. The designator [`File fn] would mean that the body will be stored in the
   * file [fn]. The file would be created if it did not yet exist, and
   * it would be overwritten if it did already exist.
   *
   * Note that the [storage_style] function is called for every non-multipart
   * body part.
   *
   * Large message bodies (> maximum string length) are supported if the
   * bodies are stored in files. The memory consumption is optimized for
   * this case, and usually only a small constant amount of memory is needed.
   *
   * Example:
   *
   * Parse the MIME message stored in the file f:
   *
   * {[
   * let m = read_mime_message 
   *           (new input_stream (new input_channel (open_in f)))
   * ]}
   *
   * @param unfold whether linefeeds are replaced by spaces in the values of the
   *   header fields (Note: defaults to [false] here in contrast to
   *   {!Mimestring.scan_header}!)
   * @param strip whether whitespace at the beginning and at the end of the 
   *   header fields is stripped
   * @param ro Whether the created MIME headers are read-only or not. Furthermore,
   *   the default [storage_style] uses this parameter for the MIME bodies, too.
   *   However, the MIME bodies may have a different read-only flag in general.
   *
   *)

  (* TODO: what about messages with type "message/*"? It may be possible that
   * they can be recursively decoded, but it is also legal for some media
   * types that they are "partial".
   * Currently the type "message/*" is NOT decoded.
   *)

(** {1:printing Printing MIME Messages} *)

val encode_mime_body : ?crlf:bool -> #mime_header_ro -> out_obj_channel -> out_obj_channel
  (** [let ch' = encode_mime_body hdr ch]:
   * According to the value of the Content-transfer-encoding header field
   * in [hdr] the unencoded MIME body written to ch' is encoded and transferred
   * to ch.
   *
   * Handles 7bit, 8bit, binary, quoted-printable, base64.
   *
   * For an example, see [decode_mime_body] which works in a similar way
   * but performs decoding instead of encoding.
   *
   * @param crlf if set (this is by default the case) CR/LF will be used for
   *   end-of-line (eol) termination, if not set LF will be used. For 7bit, 8bit and
   *   binary encoding the existing eol delimiters are not rewritten, so this option
   *   has only an effect for quoted-printable and base64.
   *)


val write_mime_message :
      ?wr_header:bool ->                       (* default: true *)
      ?wr_body:bool ->                         (* default: true *)
      ?nr:int ->                               (* default: 0 *)
      ?ret_boundary:string ref ->              (* default: do not return it *)
      ?crlf:bool ->                            (* default: true *)
      Netchannels.out_obj_channel ->
      complex_mime_message ->
        unit
  (** Writes the MIME message to the output channel. The content-transfer-
   * encoding of the leaves is respected, and their bodies are encoded
   * accordingly. The content-transfer-encoding of multipart messages is
   * always "fixed", i.e. set to "7bit", "8bit", or "binary" depending
   * on the contents.
   *
   * The function fails if multipart messages do not have a multipart
   * content type field (i.e. the content type does not begin with "multipart").
   * If only the boundary parameter is missing, a good boundary parameter is
   * added to the content type. "Good" means here that it is impossible
   * that the boundary string occurs in the message body if the
   * content-transfer-encoding is quoted-printable or base64, and that
   * such an occurrence is very unlikely if the body is not encoded.
   * If the whole content type field is missing, a "multipart/mixed" type
   * with a boundary parameter is added to the printed header.
   *
   * Note that already existing boundaries are used, no matter whether
   * they are of good quality or not.
   *
   * No other header fields are added, deleted or modified. The mentioned
   * modifications are _not_ written back to the passed MIME message but
   * only added to the generated message text.
   *
   * It is possible in some cases that the boundary does not work (both
   * the existing boundary, and the added boundary). This causes that a wrong
   * and unparseable MIME message is written. In order to ensure a correct
   * MIME message, it is recommended to parse the written text, and to compare
   * the structure of the message trees. It is, however, very unlikely that
   * a problem arises.
   *
   * Note that if the passed message is a simple message like (_,`Body _),
   * and if no content-transfer-encoding is set, the written message might
   * not end with a linefeed character.
   *
   * @param wr_header If true, the outermost header is written. Inner headers
   *   of the message parts are written unless ~wr_body=false.
   * @param wr_body If true, the body of the whole message is written; if false,
   *   no body is written at all.
   * @param nr This argument sets the counter that is included in generated
   *   boundaries to a certain minimum value.
   * @param ret_boundary if passed, the boundary of the outermost multipart
   *   message is written to this reference. (Internally used.)
   * @param crlf if set (this is by default the case) CR/LF will be used for
   *   end-of-line (eol) termination, if not set LF will be used. The eol 
   *   separator is used for the header, the multipart framing, and for
   *   bodies encoded as quoted-printable or base64. Other eol separators are
   *   left untouched.
   *)
