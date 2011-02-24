(* $Id: netsendmail.mli 1206 2008-10-23 13:51:12Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

(** Functions to compose and send electronic mails 
 *
 * {b Contents}
 *
 * - {!Netsendmail.composing} 
 * - {!Netsendmail.sending}
 *
 * The tutorial has been moved to {!Netsendmail_tut}.
 *)


(** {1:composing Composing Messages} 
 *
 * The core function is {!Netsendmail.compose} generating a MIME mail.
 * The mail can be sent with {!Netsendmail.sendmail}, written to an
 * object channel with {!Netmime.write_mime_message}, or postprocessed
 * by a user function.
 *
 * The call to [compose] can be as easy as
 *
 * {[ compose ~from_addr:("me", "me\@domain.net") 
 *            ~to_addrs:["you", "you\@domain.com"]
 *            ~subject:"I have a message for you"
 *            "Hello, this is my message!\n"
 * ]}
 *
 * This call generates the message as {!Netmime.complex_mime_message},
 * and can be directly sent with {!Netsendmail.sendmail}.
 *
 * The [compose] function is the simplified interface; alternatively one
 * can also generate the mail by calling {!Netsendmail.wrap_mail},
 * {!Netsendmail.wrap_parts}, and {!Netsendmail.wrap_attachment}, getting
 * more fine-grained control of certain options.
 *)

val compose :
      ?in_charset:Netconversion.encoding ->
      ?out_charset:Netconversion.encoding ->
      ?from_addr:(string * string) ->
      ?cc_addrs:(string * string) list ->
      ?bcc_addrs:(string * string) list ->
      ?content_type:(string * (string * Mimestring.s_param) list) ->
      ?container_type:(string * (string * Mimestring.s_param) list) ->
      ?attachments:Netmime.complex_mime_message list ->
      to_addrs:(string * string) list ->
      subject:string ->
      (* text:*) string ->
	Netmime.complex_mime_message
  (** Composes a mail message with a main text, and optionally
   * a number of attachments.
   *
   * The addresses [from_addr], [to_addrs], [cc_addrs], and [bcc_addrs] are
   * passed as pairs [(human_readable,formal)] where
   * [human_readable] is an arbitrary printable string identifying the
   * sender/receiver, and where [formal] is the RFC-822 mailbox specification.
   * An example is [("Stolpmann, Gerd", "gerd\@gerd-stolpmann.de")].
   *
   * The [subject] can be any text.
   *
   * The anonymous [string] argument is the main text of the mail.
   * 
   * The resulting message is always a correct MIME message.
   *
   * @param in_charset All passed texts (except the formal addresses) must
   *    be encoded in [in_charset]. Default: [`Enc_iso88591].
   *    As another exception, setting [content_type] explicitly prevents
   *    the main text from being converted, and [in_charset] does not
   *    have a meaning for the main text.
   * @param out_charset The encoded words in the generated header fields,
   *    if necessary, and the main text are encoded in [out_charset]. 
   *    Default: [`Enc_iso88591].
   *    It is required that [out_charset] is ASCII-compatible.
   *    As a special rule, setting [content_type] explicitly prevents
   *    the main text from being converted to [out_charset].
   * @param content_type The content type of the main text. The list is
   *    the list of parameters attached
   *    to the type, e.g. [("text/plain", ["charset", mk_param "ISO-8859-1"])]
   *    (see {!Mimestring.mk_param}). When this argument is set,
   *    the main text is no longer converted to [out_charset].
   *    By default, when this argument is missing, the main text is
   *    converted from [in_charset] to [out_charset], and the 
   *    content type becomes ["text/plain; charset=<out_charset>"].
   * @param container_type The content type of the container wrapping the
   *    main text and the attachment into one entity
   *    (only used if [attachments] are present). This
   *    defaults to [("multipart/mixed", [])]. This must be either a
   *    "multipart" or "message" type.
   * @param attachments An optional list of attachments. Should be generated
   *    with [wrap_attachment].
   *)

(** {b Character Set Conversion}
 *
 * The impact of [in_charset] and [out_charset] on the generated mail
 * is not very obvious. The charset arguments may have an effect on
 * the mail header and the mail body.
 *
 * The mail header can only be composed of ASCII characters (7 bit).
 * To circumvent this restriction the MIME standard specifies a special
 * format, the so-called encoded words. These may only be used in some
 * places, and [compose] knows where: In the subject, and the non-formal 
 * part of mail addresses. The [out_charset] is the character set
 * used in the generated mail. The [in_charset] is the character set
 * the strings are encoded you pass to [compose]. It is a good idea
 * to have [in_charset = out_charset], or at least choose [out_charset] 
 * as a superset of [in_charset], because this ensures that the character
 * set conversion succeeds.
 *
 * If the mail header does not make use of the additional non-ASCII
 * characters, the encoded words will be avoided.
 *
 * The mail body is only subject of character set conversion if 
 * the [content_type] is {b not} passed to [compose]. In this case,
 * the function sets it to [text/plain], and converts the message
 * from [in_charset] to [out_charset].
 *
 * {b Adding Attachments}
 *
 * To generate the attachments, call {!Netsendmail.wrap_attachment}, e.g.
 *
 * {[ compose ...
 *      ~attachments:[ wrap_attachment  
 *                       ~content_type:("application/octet-stream", [])
 *                       (new Netmime.file_mime_body "file.tar.gz") ]
 * ]}
 *
 * There
 * are a number of kinds of attaching files, identified by [container_type].
 * The default is [multipart/mixed], meaning that the parts of the mail are
 * mixed messages and files. One can give a hint whether to display
 * the parts directly in the mailer program (so-called inline attachments),
 * or whether to suggest that the file is saved to disk ("real"
 * attachments). This hint is contained in the [Content-disposition]
 * header, see [wrap_attachment] how to set it.
 *
 * For a discusion of the other [container_type]s see the
 * {!Netsendmail.tutorial} at the end of this document.
 *)


val wrap_attachment : 
      ?in_charset:Netconversion.encoding -> 
      ?out_charset:Netconversion.encoding ->
      ?content_id:string ->
      ?content_description:string ->
      ?content_location:string ->
      ?content_disposition:(string * (string * Mimestring.s_param) list) ->
      content_type:(string * (string * Mimestring.s_param) list) ->
      Netmime.mime_body ->
	Netmime.complex_mime_message
  (** Generates a header for the [mime_body]. The returned value
   * is intended to be used as input for the [attachments] argument
   * of the [compose] function:
   *
   * {[
   * compose ...
   *    ~attachments:[ wrap_attachment
   *                      ~content_type:("audio/wav", [])
   *                      (new file_mime_body "music.wav") ]
   * ]}
   *
   * The header contains at least the [Content-type] and the
   * [Content-transfer-encoding] fields. The latter is currently
   * always ["base64"], but it is possible that the function is
   * changed in the future to also generate ["quoted-printable"]
   * when applicable.
   *
   * @param in_charset The encoding of the [content_description] argument.
   *   Default: [`Enc_iso88591].
   * @param out_charset The encoding of the generated [Content-Description]
   *    header. Default: [`Enc_iso88591].
   * @param content_type Specifies the content type with main
   *   type and list of parameters. Example:
   *   [ ("text/plain", ["charset", Mimestring.mk_param "ISO-8859-1" ]) ]
   *   (see {!Mimestring.mk_param})
   * @param content_disposition Optionally sets the [Content-disposition]
   *   header. Frequent values are
   *   - [ ("inline", []) ]: Indicates that the attachment is displayed
   *     together with the main text
   *   - [ ("attachment", ["filename", Mimestring.mk_param fn]) ]: Indicates
   *     that the attachment should be stored onto the disk. The
   *     parameter [fn] is the suggested file name. Note that [fn]
   *     should only consist of ASCII characters unless the [charset]
   *     argument of [mk_param] is set to a different character encoding.
   * @param content_id Optionally sets the [Content-ID] header field.
   *   The passed string is the ID value without the embracing angle
   *   brackets. The [Content-ID] can be used to refer to the attachment
   *   from other parts of the mail, e.g. in [multipart/related] mails
   *   HTML documents can include hyperlinks to attachments using the
   *   URL syntax [cid:ID] where [ID] is the ID value.
   * @param content_description The [Content-Description] header
   * @param content_location The [Content-Location] header. This must be
   *   a valid URL, only composed of 7 bit characters, and with escaped
   *   unsafe characters
   *)

val wrap_mail :
      ?in_charset:Netconversion.encoding ->
      ?out_charset:Netconversion.encoding ->
      ?from_addr:(string * string) ->
      ?cc_addrs:(string * string) list ->
      ?bcc_addrs:(string * string) list ->
      to_addrs:(string * string) list ->
      subject:string ->
      Netmime.complex_mime_message ->
	Netmime.complex_mime_message
  (** Sets the mail-related header fields in the input message, and
   * returns a message ready for delivery. Transfer- and delivery-related 
   * header fields are removed from the message first, and the new fields
   * are set to the values passed to this function.
   *
   * The arguments are like in {!Netsendmail.compose}.
   *
   * The input message should have at least a [Content-type] header,
   * but this is not enforced.
   *
   * Use this function as an alternative to {!Netsendmail.compose},
   * if the message is already available as [complex_mime_message],
   * e.g. to re-send a parsed mail message to a new destination.
   *)

(** {b Note: Resending Messages}
 *
 * Note that mails generated by [wrap_mail] always appear as new mails,
 * not as forwarded or replied mails. In order to do the latter a different
 * way of processing the message is needed.
 *)


val wrap_parts :
      ?in_charset:Netconversion.encoding -> 
      ?out_charset:Netconversion.encoding ->
      ?content_type:(string * (string * Mimestring.s_param) list) ->
      ?content_id:string ->
      ?content_description:string ->
      ?content_location:string ->
      ?content_disposition:(string * (string * Mimestring.s_param) list) ->
      Netmime.complex_mime_message list ->
	Netmime.complex_mime_message
  (** Generates an intermediate container for multipart attachments.
   * Use this if you want to bundle a set of attachments as a single
   * attachment.
   *
   * @param in_charset The encoding of the [content_description] argument.
   *   Default: [`Enc_iso88591].
   * @param out_charset The encoding of the generated [Content-Description]
   *    header. Default: [`Enc_iso88591].
   * @param content_type The [Content-Type] header. Default: multipart/mixed
   * @param content_id The [Content-ID] header, without the angle brackets
   * @param content_description The [Content-Description] header
   * @param content_location The [Content-Location] header. This must be
   *   a valid URL, only composed of 7 bit characters, and with escaped
   *   unsafe characters
   * @param content_disposition The [Content-Disposition] header
   *)

(** {b Low-level} *)

val create_address_list_tokens :
      ?in_charset:Netconversion.encoding -> 
      ?out_charset:Netconversion.encoding ->
      (string * string) list ->
	Mimestring.s_token list
  (** Returns the list of [s_token]s representing email addresses as
   * structured value. The addresses are passed as list of pairs
   * [(human_readable, formal)] as in the [compose] function above.
   * The returned structured field value can be formatted and filled
   * into a mail header. For example, to set the "To" header to
   * ["Stolpmann, Gerd" <gerd\@gerd-stolpmann.de>] use
   * {[
   * let sval = create_address_list_tokens ["Stolpmann, Gerd",
   *                                        "gerd\@gerd-stolpmann.de"] in
   * header # update_field "to" (format_field_value "to" sval)
   * ]}
   * This ensures that the field is correctly quoted, that appropriate
   * encodings are applied and that long values are folded into several
   * lines.
   *
   * @param in_charset The character encoding used for [human_readable].
   *   Defaults to [`Enc_iso88591].
   * @param out_charset The character encoding used in the generated
   *   encoded word. This encoding must be ASCII-compatible. Defaults to
   *   [`Enc_iso88591].
   *)


val create_text_tokens :
      ?in_charset:Netconversion.encoding ->
      ?out_charset:Netconversion.encoding ->
      string ->
	Mimestring.s_token list
  (** Returns the list of [s_token]s representing an informal text
   * as structured value. The text is passed as simple string.
   * The returned structured field value can be formatted and filled
   * into a mail header. For example, to set the "Subject" header to
   * ["I have to say something"], use
   * {[
   * let sval = create_text_tokens "I have to say something" in
   * header # update_field "subject" (format_field_value "subject" sval)
   * ]}
   * This ensures that the field is correctly quoted, that appropriate
   * encodings are applied and that long values are folded into several
   * lines.
   *
   * @param in_charset The character encoding used for the input string.
   *   Defaults to [`Enc_iso88591].
   * @param out_charset The character encoding used in the generated
   *   encoded words. This encoding must be ASCII-compatible. Defaults to
   *   [`Enc_iso88591].
   *)

val format_field_value : string -> Mimestring.s_token list -> string
  (** To put [sval], an [s_token list], into the header field [name],
   * call
   *
   * [ header # update_field name (format_field_value name sval) ]
   *
   * The field value is folded into several lines, if necessary.
   *) 


(** {1:sending Sending Messages} *)

val sendmail : ?mailer:string -> ?crlf:bool -> Netmime.complex_mime_message -> unit
  (** Sends the passed message. The mailer program must be sendmail-compatible
   * (this can be assumed on all Unix systems, even if a non-sendmail
   * mailer is installed).
   *
   * The mailer program is the command passed as [mailer], which is by
   * default a reasonable compile-time setting.
   *
   * With [crlf] one can determine the EOL convention for the message piped to
   * the mailer program: If [crlf], CR/LF is used, if [not crlf], only LF is
   * used. The default is [false] for Unix systems.
   *)
