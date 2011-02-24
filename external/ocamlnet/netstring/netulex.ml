(* $Id: netulex.ml 799 2004-07-08 23:04:25Z stolpmann $
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)


module ULB = struct
  open Netaux.ArrayAux
  open Netconversion

  type unicode_lexbuf =
	{ mutable ulb_encoding : encoding;
	  mutable ulb_encoding_start : int;
	  mutable ulb_rawbuf : string;
	  mutable ulb_rawbuf_len : int;
	  mutable ulb_rawbuf_end : int;
	  mutable ulb_rawbuf_const : bool;
	  mutable ulb_chars : int array;
	  mutable ulb_chars_pos : int array;
	  mutable ulb_chars_len : int;
	  mutable ulb_eof : bool;
	  mutable ulb_refill : string -> int -> int -> int;
	  mutable ulb_enc_change_hook : unicode_lexbuf -> unit;
	  mutable ulb_cursor : cursor
	}

  let from_function ?(raw_size = 512) ?(char_size = 250) 
                    ?(enc_change_hook = fun _ -> ())
                    ~refill enc =
    { ulb_encoding = enc;
      ulb_encoding_start = 0;
      ulb_rawbuf = String.create raw_size;
      ulb_rawbuf_len = 0;
      ulb_rawbuf_end = 0;
      ulb_rawbuf_const = false;
      ulb_chars = Array.make char_size (-1);
      ulb_chars_pos = ( let cp = Array.make (char_size+1) (-1) in
			cp.(0) <- 0;
			cp );
      ulb_chars_len = 0;
      ulb_eof = false;
      ulb_refill = refill;
      ulb_enc_change_hook = enc_change_hook;
      ulb_cursor = create_cursor enc "";
    }

  let from_in_obj_channel ?raw_size ?char_size ?enc_change_hook enc inch =
    let refill s k l =
      try
	let n = inch # input s k l in
	if n=0 then
	  failwith "Netulex.ULB.from_in_obj_channel: non-blocking channel";
	n
      with
	  End_of_file -> 0
    in
    from_function ?raw_size ?char_size ?enc_change_hook ~refill enc

  let from_string ?(enc_change_hook = fun _ -> ()) enc s =
    let char_size = 250 in
    { ulb_encoding = enc;
      ulb_encoding_start = 0;
      ulb_rawbuf = String.copy s;
      ulb_rawbuf_len = String.length s;
      ulb_rawbuf_end = 0;
      ulb_rawbuf_const = true;
      ulb_chars = Array.make char_size (-1);
      ulb_chars_pos = ( let cp = Array.make (char_size+1) (-1) in
			cp.(0) <- 0;
			cp );
      ulb_chars_len = 0;
      ulb_eof = true;
      ulb_refill = (fun _ _ _ -> assert false);
      ulb_enc_change_hook = enc_change_hook;
      ulb_cursor = create_cursor enc "";
    }

  let from_string_inplace ?(enc_change_hook = fun _ -> ()) enc s =
    let char_size = 250 in
    { ulb_encoding = enc;
      ulb_encoding_start = 0;
      ulb_rawbuf = s;
      ulb_rawbuf_len = String.length s;
      ulb_rawbuf_end = 0;
      ulb_rawbuf_const = true;
      ulb_chars = Array.make char_size (-1);
      ulb_chars_pos = ( let cp = Array.make (char_size+1) (-1) in
			cp.(0) <- 0;
			cp );
      ulb_chars_len = 0;
      ulb_eof = true;
      ulb_refill = (fun _ _ _ -> assert false);
      ulb_enc_change_hook = enc_change_hook;
      ulb_cursor = create_cursor enc "";
    }

  let delete n ulb =
    if n < 0 || n > ulb.ulb_chars_len then
      invalid_arg "Netulex.ULB.delete";
    let m = ulb.ulb_chars_len - n in
    int_blit ulb.ulb_chars n ulb.ulb_chars 0 m;
    int_blit ulb.ulb_chars_pos n ulb.ulb_chars_pos 0 (m+1);

    if not ulb.ulb_rawbuf_const then (
      let k = ulb.ulb_chars_pos.(0) in
      assert (ulb.ulb_rawbuf_end >= k);
      let m' = ulb.ulb_rawbuf_len - k in
      String.blit ulb.ulb_rawbuf k ulb.ulb_rawbuf 0 m';
      let cp = ulb.ulb_chars_pos in
      for i = 0 to m do
	cp.(i) <- cp.(i) - k
      done;

      ulb.ulb_rawbuf_len <- m';
      ulb.ulb_rawbuf_end <- ulb.ulb_rawbuf_end - k;
    );

    ulb.ulb_chars_len  <- m;
    ulb.ulb_encoding_start <- max 0 (ulb.ulb_encoding_start - n)
      
  let set_encoding enc ulb =
    if enc <> ulb.ulb_encoding then (
      ulb.ulb_encoding <- enc;
      ulb.ulb_encoding_start <- ulb.ulb_chars_len;
      ulb.ulb_enc_change_hook ulb
    )

  let close ulb =
    ulb.ulb_eof <- true

  let utf8_sub_string k n ulb =
    if k < 0 || k > ulb.ulb_chars_len || n < 0 || k+n > ulb.ulb_chars_len then
      invalid_arg "Netulex.ULB.utf8_sub_string";

    if ulb.ulb_encoding = `Enc_utf8 && k >= ulb.ulb_encoding_start then (
      (* Extract the substring from [ulb_rawbuf] ! *)
      let k' = ulb.ulb_chars_pos.(k) in
      let n' = ulb.ulb_chars_pos.(k+n) - k' in
      String.sub ulb.ulb_rawbuf k' n'
    )
    else (
      (* Create the UTF-8 string from [ulb_chars] *)
      ustring_of_uarray `Enc_utf8 ~pos:k ~len:n ulb.ulb_chars
    )

  let utf8_sub_string_length k n ulb =
    if k < 0 || k > ulb.ulb_chars_len || n < 0 || k+n > ulb.ulb_chars_len then
      invalid_arg "Netulex.ULB.utf8_sub_string_length";

    if ulb.ulb_encoding = `Enc_utf8 && k >= ulb.ulb_encoding_start then (
      (* Extract the substring from [ulb_rawbuf] ! *)
      let k' = ulb.ulb_chars_pos.(k) in
      let n' = ulb.ulb_chars_pos.(k+n) - k' in
      n'
    )
    else (
      (* Count the UTF-8 string from [ulb_chars] *)
      (* Maybe better algorithm: divide into several slices, and call
       * ustring_of_uarray for them. Goal: Reduction of memory allocation
       *)
      let conv = ustring_of_uchar `Enc_utf8 in
      let n' = ref 0 in
      for i = k to k+n-1 do
	n' := !n' + String.length (conv ulb.ulb_chars.(i))
      done;
      !n'
    )


  let rec refill_aux ulb = 
    (* Check whether we cannot add at least one byte to [ulb_chars] because
     * of EOF:
     *)
    if ulb.ulb_eof && ulb.ulb_rawbuf_len = ulb.ulb_rawbuf_end then
      0
    else (

      (* Enlarge [ulb_chars] if necessary (need at least space for one character)
       *)
      if ulb.ulb_chars_len >= Array.length ulb.ulb_chars then (
	let n = min (Sys.max_array_length-1) (2 * (Array.length ulb.ulb_chars)) in
	if n = Array.length ulb.ulb_chars then
	  failwith "Netulex.ULB.refill: array too large";
	
	let c = Array.make n (-1) in
	let cp = Array.make (n+1) (-1) in
	int_blit ulb.ulb_chars 0 c 0 ulb.ulb_chars_len;
	int_blit ulb.ulb_chars_pos 0 cp 0 (ulb.ulb_chars_len+1);
	
	ulb.ulb_chars <- c;
	ulb.ulb_chars_pos <- cp;
      );
      
      (* If there is unanalysed material in [ulb_rawbuf], try to convert it.
       * It may happen, however, that there is only the beginning of a 
       * multi-byte character, so this may not add any new character.
       *)
      let new_chars =
	if ulb.ulb_rawbuf_end < ulb.ulb_rawbuf_len then (
	  let cs = ulb.ulb_cursor in
	  reinit_cursor
	    ~range_pos:ulb.ulb_rawbuf_end
	    ~range_len:(ulb.ulb_rawbuf_len - ulb.ulb_rawbuf_end)
	    ~enc:ulb.ulb_encoding
	    ulb.ulb_rawbuf
	    cs;
	  let counter = ref 0 in
	  ( try
	      while ulb.ulb_chars_len < Array.length ulb.ulb_chars do
		let space = Array.length ulb.ulb_chars - ulb.ulb_chars_len in
		(* cursor_blit may raise End_of_string, too *)
		let n = cursor_blit 
			  cs ulb.ulb_chars ulb.ulb_chars_len space in
		let n' = cursor_blit_positions
			   cs ulb.ulb_chars_pos ulb.ulb_chars_len space in
		assert(n=n');
		if n>0 then (
		  ulb.ulb_chars_len <- ulb.ulb_chars_len+n;
		  counter := !counter + n;
		  move ~num:n cs; (* may raise Malformed_code *)
		) 
		else (
		  (* We are at a special position in the string! *)
		  try ignore(uchar_at cs); assert false
		  with
		      Byte_order_mark ->
			(* Skip the BOM: *)
			move cs   (* may raise Malformed_code *)
			(* Note: this [move] does not count *)
		    | Partial_character ->
			(* Stop here *)
			raise Exit
		    (* End_of_string: already handled *)
		)
	      done
	    with
		Exit ->
		  ()
	      | End_of_string ->
		  ()
	  );
	  
	  let e = cursor_pos cs; in
	  ulb.ulb_chars_pos.(ulb.ulb_chars_len) <- e;
	  ulb.ulb_rawbuf_end <- e;

	  (* Encoding might have changed: *)
	  set_encoding (cursor_encoding cs) ulb;
	
	  !counter
	)
	else
	  0
      in
      
      (* In the case we still did not add any char: Check if we are near
       * EOF (the last multi-byte character is not complete).
       *)
      if new_chars = 0 then (
	if ulb.ulb_eof then raise Malformed_code;

	assert(not ulb.ulb_rawbuf_const);

	(* Now try to get new data into [ulb_rawbuf]. First, we check whether
	 * we have enough free space in this buffer. We insist on at least
	 * 50 bytes (quite arbitrary...). Then call the [ulb_refill] function
	 * to get the data.
	 *)
	if ulb.ulb_rawbuf_len + 50 >= String.length ulb.ulb_rawbuf then (
	  let n = min Sys.max_string_length (2 * (String.length ulb.ulb_rawbuf)) in
	  if n = String.length ulb.ulb_rawbuf then
	    failwith "Netulex.ULB.refill: string too large";
	  
	  let s = String.create n in
	  String.blit ulb.ulb_rawbuf 0 s 0 ulb.ulb_rawbuf_len;
	  ulb.ulb_rawbuf <- s;
	);
	
	(* Call now [ulb_refill]. If we detect EOF, record this. Anyway,
	 * start over.
	 *)
	let space = (String.length ulb.ulb_rawbuf) - ulb.ulb_rawbuf_len in
	let n = ulb.ulb_refill ulb.ulb_rawbuf ulb.ulb_rawbuf_len space in
	assert(n>=0);
	if n=0 then (
	  (* EOF *)
	  ulb.ulb_eof <- true;
	)
	else (
	  ulb.ulb_rawbuf_len <- ulb.ulb_rawbuf_len + n
	);
	
	refill_aux ulb
      )
      else
	new_chars
    )

  let refill ulb =
    let n = refill_aux ulb in
    assert(n>=0);
    if n=0 then (
      assert(ulb.ulb_eof);
      assert(ulb.ulb_rawbuf_len = ulb.ulb_rawbuf_end);
      raise End_of_file
    )

end


module Ulexing = struct
  type lexbuf =
      { ulb : ULB.unicode_lexbuf;
	mutable offset : int;
	mutable pos : int;
	mutable start : int;
	mutable marked_pos : int;
	mutable marked_val : int;
      }

  exception Error

  let from_ulb_lexbuf ulb =
    { ulb = ulb;
      offset = 0;
      pos = 0;
      start = 0;
      marked_pos = 0;
      marked_val = 0;
    }

  let lexeme_start lexbuf = lexbuf.start + lexbuf.offset
  let lexeme_end lexbuf = lexbuf.pos + lexbuf.offset
  let lexeme_length lexbuf = lexbuf.pos - lexbuf.start

  let lexeme lexbuf =
    let buf = lexbuf.ulb.ULB.ulb_chars in
    Array.sub buf lexbuf.start (lexbuf.pos - lexbuf.start)

  let sub_lexeme lexbuf pos len =
    let buf = lexbuf.ulb.ULB.ulb_chars in
    Array.sub buf (lexbuf.start + pos) len

  let lexeme_char lexbuf pos =
    let buf = lexbuf.ulb.ULB.ulb_chars in
    buf.(lexbuf.start + pos)

  let utf8_lexeme lexbuf =
    ULB.utf8_sub_string lexbuf.start (lexbuf.pos - lexbuf.start) lexbuf.ulb

  let utf8_sub_lexeme lexbuf pos len =
    ULB.utf8_sub_string (lexbuf.start + pos) len lexbuf.ulb

  let utf8_sub_lexeme_length lexbuf pos len =
    ULB.utf8_sub_string_length (lexbuf.start + pos) len lexbuf.ulb
    
  (* "Internal" interface *)
  let start lexbuf =
    lexbuf.start <- lexbuf.pos;
    lexbuf.marked_pos <- lexbuf.pos;
    lexbuf.marked_val <- (-1)

  let mark  lexbuf i =
    lexbuf.marked_pos <- lexbuf.pos;
    lexbuf.marked_val <- i
      
  let backtrack lexbuf =
    lexbuf.pos <- lexbuf.marked_pos;
    lexbuf.marked_val

  let rollback lexbuf =
    lexbuf.pos <- lexbuf.start

  let eof = (-1)

  let refill lexbuf = 
    try
      (* Delete all characters in ulexbuf before the current lexeme: *)
      if lexbuf.start > 0 then (
	let n = lexbuf.start in
	ULB.delete n lexbuf.ulb;
	lexbuf.offset <- lexbuf.offset + n;
	lexbuf.pos <- lexbuf.pos - n;
	lexbuf.marked_pos <- lexbuf.marked_pos - n;
	lexbuf.start <- 0;
      );
      ULB.refill lexbuf.ulb;
      (* raises either End_of_file, or ensures there is one char in ulb *)
      lexbuf.ulb.ULB.ulb_chars.(lexbuf.pos)
    with
	End_of_file ->
	  (* We cannot modify the buffer as the original Ulexing implementation
	   *)
	  eof
	  

  let next lexbuf =
    let ulb = lexbuf.ulb in
    let i =
      if lexbuf.pos = ulb.ULB.ulb_chars_len then
	refill lexbuf
      else 
	ulb.ULB.ulb_chars.(lexbuf.pos)
    in
    if i <> eof then lexbuf.pos <- lexbuf.pos + 1;
    i

end
