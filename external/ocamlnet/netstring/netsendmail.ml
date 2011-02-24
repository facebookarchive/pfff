(* $Id: netsendmail.ml 1003 2006-09-24 15:17:15Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

open Netchannels
open Netmime
open Mimestring

let sendmail_program = "/usr/lib/sendmail" ;;


let only_usascii_re = Pcre.regexp "^[\001-\127]*$";;

let specials_re = 
  Pcre.regexp "[\\<\\>\\\"\\\\\\,\\(\\)\\@\\;\\:\\.\\[\\]\\/\\=\\?]"

let exists rex s =
  try 
    ignore(Pcre.exec ~rex s); true
  with
      Not_found -> false
;;

let create_address_tokens
    ?(in_charset = `Enc_iso88591) ?(out_charset = `Enc_iso88591) 
    (hr_addr, formal_addr)=
  (* Generates addresses like "Gerd Stolpmann <gerd@gerd-stolpmann.de>".
   * hr_addr is the "human readable" part, and formal_addr is the formal
   * address. hr_addr must be encoded by [charset].
   *)
  let hr_addr = 
    Netconversion.recode_string ~in_enc:in_charset ~out_enc:out_charset hr_addr in
  let hr_words =
    if Pcre.pmatch ~rex:only_usascii_re hr_addr then begin
      (* Use double quotes to protect meta characters *)
      if exists specials_re hr_addr then
	[Mimestring.QString hr_addr]
      else 
	List.map (fun s -> Mimestring.Atom s) (Pcre.split hr_addr)
    end
    else
      [Mimestring.EncodedWord((Netconversion.string_of_encoding out_charset,""),
			      "Q", 
			      hr_addr)]
  in
  (* TODO: Check syntax of formal_addr *)
  let formal_words =
    [ Mimestring.Special '<'; 
      Mimestring.Atom formal_addr; 
      Mimestring.Special '>'
    ] in
  (hr_words @ [ Mimestring.Special ' ' ] @ formal_words)
;;


let create_address_list_tokens ?in_charset ?out_charset addrs =
  let rec map addrs =
    match addrs with
	[] -> []
      | addr :: (addr' :: _ as addrs') ->
	  create_address_tokens ?in_charset ?out_charset addr @ 
	  [ Mimestring.Special ','; Mimestring.Special ' ' ] @
	  map addrs'
      | [ addr ] ->
	  create_address_tokens ?in_charset ?out_charset addr
  in
  map addrs
;;


let format_field_value fieldname tokens =
  let val_buf = Buffer.create 80 in
  let val_ch = new output_buffer val_buf in
  let maxlen = 78 in
  let hardmaxlen = 998 in
  let initlen = String.length fieldname + 2 in  (* String.length ": " = 2 *)
  Mimestring.write_value 
    ~maxlen1:(maxlen - initlen)
    ~maxlen
    ~hardmaxlen1:(hardmaxlen - initlen)
    ~hardmaxlen
    val_ch
    tokens;
  Buffer.contents val_buf
;;


let create_text_tokens
      ?(in_charset = `Enc_iso88591) ?(out_charset = `Enc_iso88591) value =
  let value =
    Netconversion.recode_string ~in_enc:in_charset ~out_enc:out_charset value in
  let words =
    if Pcre.pmatch ~rex:only_usascii_re value then
      List.map (fun s -> Atom s) (Pcre.split value)
    else
      [ Mimestring.EncodedWord((Netconversion.string_of_encoding out_charset,""), 
			       "Q", 
			       value) ]
  in
  words
;;



(* Criterions for fields in [fields_to_remove]:
 *
 * We want that the message appears as a new message. Because of this
 * all delivery and transport-related fields are removed. This also
 * includes delivery and error notifications, transfer priorities,
 * mailing list information, and local status fields.
 *
 * We keep all content-related fields. Sometimes it is difficult to
 * distinguish between content and transfer fields, especially for
 * the non-standard fields.
 *
 * Sources:
 * - RFC 2822
 * - http://www.dsv.su.se/~jpalme/ietf/mail-headers/mail-headers.html
 *   (former IETF draft, now expired)
 * - http://www.cs.tut.fi/~jkorpela/headers.html
 *)
let fields_to_remove =
  [ "date";
    "from";
    "sender";
    "reply-to";
    "to";
    "cc";
    "bcc";
    "message-id";
    "in-reply-to";
    "references";
    (* but not subject, comments, keywords *)
    "resent-date";
    "resent-from";
    "resent-sender";
    "resent-to";
    "resent-cc";
    "resent-bcc";
    "resent-message-id";
    "return-path";
    "received";
    (* non-standard, or other RFCs but frequently used: *)
    "alternate-recipient";
    "x-rcpt-to";
    "x-sender";
    "x-x-sender";
    "envelope-to";
    "x-envelope-to";
    "envelope-from";
    "x-envelope-from";
    "errors-to";
    "return-receipt-to";
    "read-receipt-to";
    "x-confirm-reading-to";
    "return-receipt-requested";
    "registered-mail-reply-requested-by";
    "delivery-date";
    "delivered-to";
    "x-loop";
    "precedence";
    "priority";   (* but not "importance" *)
    "x-msmail-priority";
    "x-priority";
    "apparently-to";
    "posted-to";
    "content-return";
    "x400-content-return";
    "disposition-notification-options";
    "disposition-notification-to";
    "generate-delivery-report";
    "original-recipient";
    "prevent-nondelivery-report";
    "mail-reply-to";
    "x-uidl";
    "x-imap";
    "x-mime-autoconverted";
    "list-archive";
    "list-digest";
    "list-help";
    "list-id";
    "mailing-list";
    "x-mailing-list";
    "list-owner";
    "list-post";
    "list-software";
    "list-subscribe";
    "list-unsubscribe";
    "list-url";
    "x-listserver";
    "x-list-host";
    "status";
  ]
;;


let wrap_mail
      ?(in_charset = `Enc_iso88591)
      ?(out_charset = `Enc_iso88591)
      ?from_addr ?(cc_addrs = []) ?(bcc_addrs = []) 
      ~to_addrs ~subject (msg : complex_mime_message) : complex_mime_message =

  let (main_hdr, main_body) = msg in
  let main_hdr' =
    new basic_mime_header main_hdr#fields in
  List.iter main_hdr'#delete_field fields_to_remove;
  let set_field_toks f toks =
    main_hdr' # update_field f (format_field_value f toks)
  in
  ( match from_addr with
	None -> ()
      | Some a -> 
	  set_field_toks "From" 
	    (create_address_list_tokens ~in_charset ~out_charset [ a ])
  );
  set_field_toks "To" 
    (create_address_list_tokens ~in_charset ~out_charset to_addrs);
  if cc_addrs <> [] then
    set_field_toks "Cc" 
      (create_address_list_tokens ~in_charset ~out_charset cc_addrs);
  if bcc_addrs <> [] then
    set_field_toks "Bcc" 
      (create_address_list_tokens ~in_charset ~out_charset bcc_addrs);
  set_field_toks "Subject" 
    (create_text_tokens ~in_charset ~out_charset subject);
  main_hdr' # update_field "MIME-Version" "1.0";
  main_hdr' # update_field "X-Mailer" "OcamlNet (ocamlnet.sourceforge.net)";
  main_hdr' # update_field "Date" 
    (Netdate.mk_mail_date ~zone:Netdate.localzone (Unix.time()));
  (main_hdr', main_body)
;;


let create_header 
      ?(in_charset = `Enc_iso88591)
      ?(out_charset = `Enc_iso88591)
      ?content_id
      ?content_description
      ?content_location
      ?content_disposition
      (ct_type, params) =
  let hdr = new basic_mime_header [] in
  let set_field_toks f toks =
    hdr # update_field f (format_field_value f toks)
  in
  
  let toks = Atom ct_type :: param_tokens ~maxlen:60 params in
  set_field_toks "Content-type" toks;
  
  (* Set Content-ID: *)
  ( match content_id with
	None -> ()
      | Some cid ->
	  set_field_toks "Content-ID" [Atom ("<" ^ cid ^ ">")]
  );
  
  (* Set Content-Description: *)
  ( match content_description with
	None -> ()
      | Some d ->
	  set_field_toks "Content-Description" 
	    (create_text_tokens ~in_charset ~out_charset d);
  );

  (* Set Content-Location: *)
  ( match content_location with
	None -> ()
      | Some loc ->
	  set_field_toks "Content-Location" 
	    (split_uri loc)
  );

  (* Set Content-Disposition: *)
  ( match content_disposition with
	None -> ()
      | Some (d_main, d_params) ->
	  set_field_toks "Content-Disposition"
	    (Atom d_main :: param_tokens ~maxlen:60 d_params)
  );

  hdr
;;


let wrap_parts
      ?(in_charset = `Enc_iso88591)
      ?(out_charset = `Enc_iso88591)
      ?(content_type = ("multipart/mixed", []))
      ?content_id
      ?content_description
      ?content_location
      ?content_disposition
      elements =

  if elements = [] then
    failwith "Netsendmail.wrap_parts";

  (* Check Content-type: *)
  let (ct_type, params) = content_type in
  let (main_type, sub_type) = split_mime_type ct_type in
  if main_type <> "multipart" && main_type <> "message" then
    failwith "Netsendmail.wrap_parts";

  let hdr =
    create_header
      ~in_charset ~out_charset ?content_id ?content_description
      ?content_location ?content_disposition (ct_type, params) in

  (hdr, `Parts elements)
;;


let wrap_attachment 
      ?(in_charset = `Enc_iso88591)
      ?(out_charset = `Enc_iso88591)
      ?content_id
      ?content_description
      ?content_location
      ?content_disposition
      ~content_type
      body =

  let hdr =
    create_header
      ~in_charset ~out_charset ?content_id ?content_description
      ?content_location ?content_disposition content_type in

  (* Set Content-transfer-encoding: *)
  let (ct_type, params) = content_type in
  let (main_type, sub_type) = split_mime_type ct_type in
  let cte =
    match main_type with
	"text" -> "quoted-printable"
      | "multipart"
      | "message" -> "binary"  (* Don't know better *)
      | _ -> "base64"
  in
  hdr # update_field "Content-transfer-encoding" cte;

  (hdr, `Body body)
;;


let compose
      ?(in_charset = `Enc_iso88591)
      ?(out_charset = `Enc_iso88591)
      ?from_addr ?(cc_addrs = []) ?(bcc_addrs = []) 
      ?content_type
      ?(container_type = ("multipart/mixed" , []))
      ?(attachments = ([] : complex_mime_message list))
      ~to_addrs ~subject body : complex_mime_message =


  (* First generate/cleanup (hdr,body) for the main text of the message: *)
  let body = 
    if content_type = None then
      Netconversion.recode_string 
	~in_enc:in_charset ~out_enc:out_charset body
    else
      body 
  in

  (* Set Content-type: *)
  let (ct_type, params) =
    ( match content_type with
	  None ->
	    ("text/plain", [ "charset",
			     mk_param (Netconversion.string_of_encoding 
					 out_charset) ])
	| Some (ct_type, params) ->
	    (ct_type, params)
    ) in

  let (main_hdr, main_body) =
    wrap_attachment 
      ~in_charset ~out_charset ~content_type:(ct_type, params)
      (new memory_mime_body body)
  in

  (* Generate the container for attachments: *)
  let mail_hdr, mail_body =
    if attachments = [] then 
      (main_hdr, main_body)
    else (
      wrap_parts
	~in_charset ~out_charset ~content_type:container_type 
	( (main_hdr, main_body) :: attachments )
    )
  in	  
  wrap_mail ~in_charset ~out_charset ?from_addr ~to_addrs ~cc_addrs ~bcc_addrs
            ~subject (mail_hdr, mail_body)
;;


let sendmail ?(mailer = sendmail_program) ?(crlf = false) message =
  let cmd = mailer ^ " -B8BITMIME -t -i" in
  with_out_obj_channel
    (new output_command cmd)
    (fun ch ->
       write_mime_message ~crlf ch message;
    )
;;
