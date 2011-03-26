(* $Id: netchannels.ml 1519 2010-12-20 04:19:42Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

exception Closed_channel
exception Buffer_underrun
exception Command_failure of Unix.process_status

let () =
  Netexn.register_printer
    (Command_failure(Unix.WEXITED 0))
    (fun e ->
       match e with
	 | Command_failure ps ->
	     let ps_str =
	       match ps with
		 | Unix.WEXITED n -> "WEXITED " ^ string_of_int n
		 | Unix.WSIGNALED n -> "WSIGNALED " ^ string_of_int n
		 | Unix.WSTOPPED n -> "WSTOPPED " ^ string_of_int n
	     in
	     "Netchannels.Command_failure(" ^ ps_str ^ ")"
	 | _ ->
	     assert false
    )

let () =
  Netsys_signal.init()


class type rec_in_channel = object
  method input : string -> int -> int -> int
  method close_in : unit -> unit
end

class type raw_in_channel = object
  inherit rec_in_channel
  method pos_in : int             (* number of read characters *)
end

type input_result =
    [ `Data of int
    | `Separator of string
    ]

class type enhanced_raw_in_channel =
object 
  inherit raw_in_channel
  method private enhanced_input_line : unit -> string
  method private enhanced_input : string -> int -> int -> input_result
end

class type rec_out_channel = object
  method output : string -> int -> int -> int
  method close_out : unit -> unit
  method flush : unit -> unit
end

class type raw_out_channel = object
  inherit rec_out_channel
  method pos_out : int             (* number of written characters *)
end

class type raw_io_channel = object
  inherit raw_in_channel
  inherit raw_out_channel
end

class type compl_in_channel = object
  (* Classic operations: *)
  method really_input : string -> int -> int -> unit
  method input_char : unit -> char
  method input_line : unit -> string
  method input_byte : unit -> int
end

class type in_obj_channel = object
  inherit raw_in_channel
  inherit compl_in_channel
end


class type compl_out_channel = object
  (* Classic operations: *)
  method really_output : string -> int -> int -> unit
  method output_char : char -> unit
  method output_string : string -> unit
  method output_byte : int -> unit
  method output_buffer : Buffer.t -> unit
  method output_channel : ?len:int -> in_obj_channel -> unit
      (* ~len: optionally limit the number of bytes *)
end


class type out_obj_channel = object
  inherit raw_out_channel
  inherit compl_out_channel
end

class type io_obj_channel = object
  inherit in_obj_channel
  inherit out_obj_channel
end

class type trans_out_obj_channel = object
  inherit out_obj_channel

  method commit_work : unit -> unit
  method rollback_work : unit -> unit
end
;;


(* error_behavior: currently not used. This was a proposal to control
 * error handling, but it is not clear whether it is really 
 * useful or not.
 * I do not delete these types because they remind us of this
 * possibility. Maybe we find an outstanding example for them, and
 * want to have them back.
 *)

type error_behavior = 
    [ `Close | `Fun of (unit -> unit) | `None ]

type extended_error_behavior =
    [ `Close | `Rollback | `Fun of (unit -> unit) | `None ]


type close_mode = [ `Commit | `Rollback ];;


(* Delegation *)

class rec_in_channel_delegation ?(close=true) (ch:rec_in_channel) =
object(self)
  method input = ch#input
  method close_in() = if close then ch#close_in()
end


class raw_in_channel_delegation ?(close=true) (ch:raw_in_channel) =
object(self)
  method input = ch#input
  method close_in() = if close then ch#close_in()
  method pos_in = ch#pos_in
end


class in_obj_channel_delegation ?(close=true) (ch:in_obj_channel) =
object(self)
  method input = ch#input
  method close_in() = if close then ch#close_in()
  method pos_in = ch#pos_in
  method really_input = ch#really_input
  method input_char = ch#input_char
  method input_line = ch#input_line
  method input_byte = ch#input_byte
end


class rec_out_channel_delegation ?(close=true) (ch:rec_out_channel) =
object(self)
  method output = ch#output
  method close_out() = if close then ch#close_out()
  method flush = ch#flush
end


class raw_out_channel_delegation ?(close=true) (ch:raw_out_channel) =
object(self)
  method output = ch#output
  method close_out() = if close then ch#close_out()
  method flush = ch#flush
  method pos_out = ch#pos_out
end


class out_obj_channel_delegation ?(close=true) (ch:out_obj_channel) =
object(self)
  method output = ch#output
  method close_out() = if close then ch#close_out()
  method flush = ch#flush
  method pos_out = ch#pos_out
  method really_output = ch#really_output
  method output_char = ch#output_char
  method output_string = ch#output_string
  method output_byte = ch#output_byte
  method output_buffer = ch#output_buffer
  method output_channel = ch#output_channel
end


(****************************** input ******************************)

class input_channel ch (* : in_obj_channel *) =
object (self)
  val ch = ch
  val mutable closed = false

  method private complain_closed() =
    raise Closed_channel

  method input buf pos len = 
    if closed then self # complain_closed();
    try
      if len=0 then raise Sys_blocked_io;
      let n = Pervasives.input ch buf pos len in
      if n=0 then raise End_of_file else n
    with
	Sys_blocked_io -> 0

  method really_input buf pos len = 
    if closed then self # complain_closed();
    Pervasives.really_input ch buf pos len 

  method input_char () =
    if closed then self # complain_closed();
    Pervasives.input_char ch 

  method input_line () =
    if closed then self # complain_closed();
    Pervasives.input_line ch 

  method input_byte () =
    if closed then self # complain_closed();
    Pervasives.input_byte ch 

  method close_in () =
    if not closed then (
      Pervasives.close_in ch; closed <- true
    )

  method pos_in =
    if closed then self # complain_closed();
    Pervasives.pos_in ch 
end
;;


class input_command cmd =
let ch = Unix.open_process_in cmd in
object (self)
  inherit input_channel ch as super

  method close_in() =
    if not closed then (
      let p = Unix.close_process_in ch in
      closed <- true;
      if p <> Unix.WEXITED 0 then
	raise (Command_failure p);
    )
end
;;


class input_string ?(pos = 0) ?len s : in_obj_channel =
object (self)
  val mutable str = s
  val mutable str_len = 
    match len with 
	None   -> String.length s 
      | Some l -> pos + l

  val mutable str_pos = pos
  val mutable closed = false

  initializer
    if str_pos < 0 || str_pos > String.length str || 
       str_len < 0 || str_len > String.length s 
    then
      invalid_arg "new Netchannels.input_string";
	  

  method private complain_closed() =
    raise Closed_channel

  method input buf pos len =
    if closed then self # complain_closed();
    if pos < 0 || len < 0 || pos+len > String.length buf then
      invalid_arg "input";

    let n = min len (str_len - str_pos) in
    String.blit str str_pos buf pos n;
    
    str_pos <- str_pos + n;

    if n=0 && len>0 then raise End_of_file else n


  method really_input buf pos len =
    if closed then self # complain_closed();
    if pos < 0 || len < 0 || pos+len > String.length buf then
      invalid_arg "really_input";

    let n = self # input buf pos len in
    if n <> len then raise End_of_file;
    ()


  method input_char() =
    if closed then self # complain_closed();
    if str_pos >= str_len then raise End_of_file;
    let c = str.[ str_pos ] in
    str_pos <- str_pos + 1;
    c


  method input_line() =
    if closed then self # complain_closed();
    try
      let k = String.index_from str str_pos '\n' in
      (* CHECK: Are the different end of line conventions important here? *)
      let line = String.sub str str_pos (k - str_pos) in
      str_pos <- k+1;
      line
    with
	Not_found ->
	  if str_pos >= str_len then raise End_of_file;
	  (* Implicitly add linefeed at the end of the file: *)
	  let line = String.sub str str_pos (str_len - str_pos) in
	  str_pos <- str_len;
	  line


  method input_byte() =
    Char.code (self # input_char())


  method close_in() =
    str <- "";
    closed <- true;


  method pos_in = 
    if closed then self # complain_closed();
    str_pos

end
;;


class type nb_in_obj_channel =
object
  inherit in_obj_channel
  method shutdown : unit -> unit
end


class input_netbuffer b : nb_in_obj_channel =
object (self)
  val mutable b = b
  val mutable eof = false
  val mutable closed = false
  val mutable ch_pos = 0

  method private complain_closed() =
    raise Closed_channel

  method input buf pos len =
    if closed then self # complain_closed();
    if pos < 0 || len < 0 || pos+len > String.length buf then
      invalid_arg "input";

    let n = min len (Netbuffer.length b) in
    if n = 0 && len>0 then begin
      if eof then raise End_of_file else raise Buffer_underrun
    end
    else begin
      Netbuffer.blit b 0 buf pos n;
      Netbuffer.delete b 0 n;
      ch_pos <- ch_pos + n;
      n
    end


  method really_input buf pos len =
    if closed then self # complain_closed();
    if pos < 0 || len < 0 || pos+len > String.length buf then
      invalid_arg "really_input";

    let n = self # input buf pos len in
    if n <> len then raise End_of_file;
    ()


  method input_char() =
    if closed then self # complain_closed();
    let s = String.create 1 in
    match self # input s 0 1 with
      | 1 -> s.[0]
      | _ -> assert false


  method input_line() =
    if closed then self # complain_closed();
    try
      let k = Netbuffer.index_from b 0 '\n' in
      (* CHECK: Are the different end of line conventions important here? *)
      let line = Netbuffer.sub b 0 k in
      Netbuffer.delete b 0 (k+1);
      ch_pos <- ch_pos + k + 1;
      line
    with
	Not_found ->
	  if eof then begin
	    if Netbuffer.length b = 0 then raise End_of_file;
	    (* Implicitly add linefeed at the end of the file: *)
	    let line = Netbuffer.contents b in
	    Netbuffer.clear b;
	    ch_pos <- ch_pos + (Netbuffer.length b);
	    line
	  end
	  else raise Buffer_underrun


  method input_byte() =
    Char.code (self # input_char())


  method close_in() =
    closed <- true;


  method pos_in = 
    if closed then self # complain_closed();
    ch_pos


  method shutdown() = eof <- true
end
;;


let create_input_netbuffer b =
  let ch = new input_netbuffer b in
  (ch :> in_obj_channel), (ch # shutdown)
;;


let lexbuf_of_in_obj_channel (objch : in_obj_channel) : Lexing.lexbuf =
  let fill_buffer buf len =
    try
      let n = objch # input buf 0 len in
      if n=0 then failwith "Netchannels.lexbuf_of_in_obj_channel: No data (non-blocking I/O?)";
      n
    with
	End_of_file -> 0
  in
  Lexing.from_function fill_buffer
;;


let string_of_in_obj_channel (objch : in_obj_channel) : string =
  (* There are similarities to copy_channel below. *)
  (* The following algorithm uses only up to 2 * N memory, not 3 * N
   * as with the Buffer module.
   *)
  let slen = 1024 in
  let l = ref [] in
  let k = ref 0 in
  try
    while true do
      let s = String.create slen in
      let n = objch # input s 0 slen in
      if n = 0 then 
	failwith "Netchannels.string_of_in_obj_channel: No data (non-blocking I/O?)";
      k := !k + n;
      if n < slen then
	l := (String.sub s 0 n) :: !l
      else
	l := s :: !l;
    done;
    assert false
  with
      End_of_file -> 
	let s = String.create !k in
	while !l <> [] do
	  match !l with
	      u :: l' ->
		let n = String.length u in
		k := !k - n;
		String.blit u 0 s !k n;
		l := l'
	    | [] -> assert false
	done;
	assert (!k = 0);
	s
;;


let lines_of_in_obj_channel ch =
  let acc = ref [] in
  try
    while true do
      acc := ch#input_line() :: !acc
    done;
    assert false
  with
    | End_of_file -> List.rev !acc
;;


let with_in_obj_channel ch f =
  try
    let result = f ch in
    ( try ch # close_in() with Closed_channel -> ());
    result
  with
      e ->
	( try ch # close_in() with Closed_channel -> ());
	raise e
;;


class virtual augment_raw_in_channel =
object (self)
  method virtual input : string -> int -> int -> int
  method virtual close_in : unit -> unit
  method virtual pos_in : int

  method really_input s pos len =
    let rec read_rest n =
      if n < len then
	let m = self # input s (pos+n) (len-n) in
	if m = 0 then raise Sys_blocked_io;
	read_rest (n+m)
      else
	()
    in
    read_rest 0

  method input_char () =
    let s = String.create 1 in
    self # really_input s 0 1;
    s.[0]

  method input_byte () =
    let s = String.create 1 in
    self # really_input s 0 1;
    Char.code s.[0]

  method input_line () =
    let s = String.create 1 in
    let b = Buffer.create 80 in
    let m = self # input s 0 1 in
    if m = 0 then raise Sys_blocked_io;
    while s.[0] <> '\n' do
      Buffer.add_char b s.[0];
      try
	let m = self # input s 0 1 in
	if m = 0 then raise Sys_blocked_io;
      with
	  End_of_file ->
	    s.[0] <- '\n'
    done;
    Buffer.contents b

end
;;


class lift_raw_in_channel r =
object(self)
  inherit augment_raw_in_channel

  method input s p l =
    r # input s p l

  method close_in () =
    r # close_in()

  method pos_in =
    r # pos_in

end;;


class lift_rec_in_channel ?(start_pos_in = 0) (r : rec_in_channel) =
object(self)
  inherit augment_raw_in_channel

  val mutable closed = false
  val mutable pos_in = start_pos_in

  method input s p l =
    if closed then raise Closed_channel;
    let n = r # input s p l in
    pos_in <- pos_in + n;
    n

  method close_in () =
    if not closed then (
      closed <- true;
      r # close_in()
    )

  method pos_in =
    if closed then raise Closed_channel;
    pos_in

end;;


type eol_status =
    EOL_not_found
  | EOL_partially_found of int (* Position *)
  | EOL_found of int * int     (* Position, length *)


class buffered_raw_in_channel
      ?(eol = [ "\n" ])
      ?(buffer_size = 4096)
      (ch : raw_in_channel) : enhanced_raw_in_channel =
object (self)
  val out = ch
  val bufsize = buffer_size
  val buf = String.create buffer_size
  val mutable bufpos = 0
  val mutable buflen = 0
  val mutable eof = false
  val mutable closed = false

  initializer
    if List.exists(fun s -> s = "") eol then
      invalid_arg "Netchannels.buffered_raw_in_channel";
    if List.exists(fun s -> String.length s > buffer_size) eol then
      invalid_arg "Netchannels.buffered_raw_in_channel";

  method input s pos len =
    if closed then raise Closed_channel;
    if len > 0 then (
      if bufpos = buflen then (
	self # refill();
      );
      let n = min len (buflen - bufpos) in
      String.blit buf bufpos s pos n;
      bufpos <- bufpos + n;
      n
    )
    else 0

  method private refill() =
    let d = bufpos in
    if d > 0 && d < buflen then (
      String.blit buf d buf 0 (buflen-d)
    );
    bufpos <- 0;
    buflen <- buflen - d;
    try
      assert(bufsize > buflen);  (* otherwise problems... *)
      let n = ch # input buf buflen (bufsize-buflen) in  (* or End_of_file *)
      if n = 0 then raise Sys_blocked_io;
      buflen <- buflen+n;
    with
	End_of_file as exn ->
	  eof <- true;
	  raise exn


  method close_in () =
    if not closed then (
      ch # close_in();
      closed <- true
    )

  method pos_in =
    (ch # pos_in) - (buflen - bufpos)


  method private find_eol() =
    (* Try all strings from [eol] in turn. For every string we may
     * have three results:
     * - Not found
     * - Partially found
     * - Found
     * The eol delimiter is only found if there are no partial
     * results, and at least one positive result. The longest
     * string is taken.
     *)

    let find_this_eol eol =
      (* Try to find the eol string [eol] in [buf] starting at
       * [bufpos] up to [buflen]. Return [eol_status].
       *)
      let eol0 = eol.[0] in
      try
	let k = String.index_from buf bufpos eol0 in (* or Not_found *)
	if k>=buflen then raise Not_found;
	let k' = min buflen (k+String.length eol) in
	let s = String.sub buf k (k' - k) in
	if s = eol then
	  EOL_found(k, String.length eol)
	else
	  if not eof && String.sub eol 0 (String.length s) = s then
	    EOL_partially_found k
	  else
	    EOL_not_found
      with
	    Not_found -> EOL_not_found
    in  

    let rec find_best_eol best eol_result =
      match eol_result with
	  EOL_not_found :: eol_result' ->
	    find_best_eol best eol_result'
	| EOL_partially_found pos as r :: eol_result' ->
	    ( match best with
		  EOL_partially_found pos' ->
		    if pos < pos' then
		      find_best_eol r eol_result'
		    else
		      find_best_eol best eol_result'
		| _ ->
		    find_best_eol r eol_result'
	    )
	| EOL_found(pos,len) as r :: eol_result' ->
	    ( match best with
		  EOL_found(pos',len') ->
		    if pos < pos' || (pos = pos' && len > len') then
		      find_best_eol r eol_result'
		    else
		      find_best_eol best eol_result'
		| EOL_partially_found _ ->
		    find_best_eol best eol_result'
		| EOL_not_found ->
		    find_best_eol r eol_result'
	    )
	| [] ->
	    best
    in
    let eol_results = List.map find_this_eol eol in
    find_best_eol EOL_not_found eol_results

  method private enhanced_input s pos len : input_result =
    if closed then raise Closed_channel;
    if len > 0 then (
      if bufpos = buflen then (
	self # refill(); (* may raise End_of_file *)
      );
      let result = ref None in
      while !result = None do
	let best = self # find_eol() in
	match best with
	    EOL_not_found -> 
	      let n = min len (buflen - bufpos) in
	      String.blit buf bufpos s pos n;
	      bufpos <- bufpos + n;
	      result := Some(`Data n)
	  | EOL_found(p,l) ->
	      if p = bufpos then (
		bufpos <- bufpos + l;
		result := Some(`Separator(String.sub buf p l))
	      )
	      else (
		let n = min len (p - bufpos) in
		String.blit buf bufpos s pos n;
		bufpos <- bufpos + n;
		result := Some(`Data n)
	      )
	  | EOL_partially_found p ->
	      if p = bufpos then (
		try self # refill()
		with End_of_file -> ()
		(* ... and continue! *)
	      )
	      else (
		let n = min len (p - bufpos) in
		String.blit buf bufpos s pos n;
		bufpos <- bufpos + n;
		result := Some(`Data n)
	      )
      done;
      match !result with
	  None -> assert false
	| Some r -> r
    )
    else `Data 0

  method private enhanced_input_line() =
    if closed then raise Closed_channel;
    let b = Buffer.create 80 in
    let eol_found = ref false in
    if bufpos = buflen then (
      self # refill();  (* may raise End_of_file *)
    );
    while not !eol_found do
      let best = self # find_eol() in
      try
	match best with
	    EOL_not_found ->
	      Buffer.add_substring b buf bufpos (buflen-bufpos);
	      bufpos <- buflen;
	      self # refill();     (* may raise End_of_file *)
	  | EOL_partially_found pos ->
	      Buffer.add_substring b buf bufpos (pos-bufpos);
	      bufpos <- pos;
	      self # refill();     (* may raise End_of_file *)
	  | EOL_found(pos,len) ->
	      Buffer.add_substring b buf bufpos (pos-bufpos);
	      bufpos <- pos+len;
	      eol_found := true
	with
	    End_of_file -> 
	      bufpos <- 0;
	      buflen <- 0;
	      eof <- true;
	      eol_found := true
    done;
    Buffer.contents b
end
;;


class lift_raw_in_channel_buf ?eol ?buffer_size r =
object(self)
  inherit buffered_raw_in_channel ?eol ?buffer_size r
  inherit augment_raw_in_channel

  method input_line () =
    self # enhanced_input_line()
end;;


type lift_in_arg = [ `Rec of rec_in_channel | `Raw of raw_in_channel ]

let lift_in ?(eol = ["\n"]) ?(buffered=true) ?buffer_size (x : lift_in_arg) =
  match x with
      `Rec r when not buffered ->
	if eol <> ["\n"] then invalid_arg "Netchannels.lift_in";
	new lift_rec_in_channel r
    | `Rec r when buffered ->
	let r' = new lift_rec_in_channel r in
	new lift_raw_in_channel_buf ~eol ?buffer_size (r' :> raw_in_channel)
    | `Raw r when not buffered ->
	if eol <> ["\n"] then invalid_arg "Netchannels.lift_in";
	new lift_raw_in_channel r
    | `Raw r when buffered ->
	new lift_raw_in_channel_buf ~eol ?buffer_size r
    | _ -> failwith "Impossible"
;;


(****************************** output ******************************)

exception No_end_of_file

let copy_channel 
      ?(buf = String.create 1024)
      ?len (src_ch : in_obj_channel) (dest_ch : out_obj_channel) =
  (* Copies contents from src_ch to dest_ch. Returns [true] if at EOF.
   *)
  let slen = String.length buf in
  let k = ref 0 in
  try
    while true do
      let m = min slen (match len with Some x -> x - !k | None -> max_int) in
      if m <= 0 then raise No_end_of_file;
      let n = src_ch # input buf 0 m in
      if n = 0 then raise Sys_blocked_io;
      dest_ch # really_output buf 0 n;
      k := !k + n
    done;
    assert false
  with
      End_of_file ->
	true
    | No_end_of_file ->
	false
;;


class output_channel ?(onclose = fun () -> ()) ch 
		     (* : out_obj_channel *) =
  let errflag = ref false in
  let monitored f arg =
    try 
      let r = f arg in
      errflag := false;
      r
    with
      | error ->
	  errflag := true;
	  raise error in

object (self)
  val ch = ch
  val onclose = onclose
  val mutable closed = false

  method private complain_closed() =
    raise Closed_channel

  method output buf pos len =
    if closed then self # complain_closed();
    (* Pervasives.output does not support non-blocking I/O directly.
     * Work around it:
     *)
    let p0 = Pervasives.pos_out ch in
    try 
      Pervasives.output ch buf pos len;
      errflag := false;
      len
    with
      | Sys_blocked_io ->
	  let p1 = Pervasives.pos_out ch in
	  errflag := false;
	  p1 - p0
      | error ->
	  errflag := true;
	  raise error

  method really_output buf pos len =
    if closed then self # complain_closed();
    monitored (Pervasives.output ch buf pos) len

  method output_char c =
    if closed then self # complain_closed();
    monitored (Pervasives.output_char ch) c

  method output_string s =
    if closed then self # complain_closed();
    monitored (Pervasives.output_string ch) s

  method output_byte b =
    if closed then self # complain_closed();
    monitored (Pervasives.output_byte ch) b

  method output_buffer b =
    if closed then self # complain_closed();
    monitored(Buffer.output_buffer ch) b

  method output_channel ?len ch =
    if closed then self # complain_closed();
    ignore
      (monitored
	 (copy_channel ?len ch)
	 (self : #out_obj_channel :> out_obj_channel))

  method flush() =
    if closed then self # complain_closed();
    monitored Pervasives.flush ch

  method close_out() =
    if not closed then (
      try
	(* if !errflag is set, we know that the immediately preceding
	   operation raised an exception, and we are now likely in the
	   exception handler
	 *)
	if !errflag then
	  Pervasives.close_out_noerr ch
	else
	  Pervasives.close_out ch; 
	closed <- true; 
	onclose()
      with
	| error ->
	    Netlog.logf `Err
	      "Netchannels: Suppressed error in close_out: %s"
	      (Netexn.to_string error);
	    Pervasives.close_out_noerr ch;
	    closed <- true; 
	    onclose()
    )

  method pos_out = 
    if closed then self # complain_closed();
    Pervasives.pos_out ch

end
;;


class output_command ?onclose cmd =
  let ch = Unix.open_process_out cmd in
object (self)
  inherit output_channel ?onclose ch as super

  method close_out() =
    if not closed then (
      let p = Unix.close_process_out ch in
      closed <- true;
      onclose();
      if p <> Unix.WEXITED 0 then
	raise (Command_failure p);  (* Keep this *)
    )
end
;;


class output_buffer ?(onclose = fun () -> ()) buffer : out_obj_channel =
object(self)
  val buffer = buffer
  val onclose = onclose
  val mutable closed = false

  method private complain_closed() =
    raise Closed_channel

  method output buf pos len =
    if closed then self # complain_closed();
    Buffer.add_substring buffer buf pos len;
    len

  method really_output buf pos len =
    if closed then self # complain_closed();
    Buffer.add_substring buffer buf pos len

  method output_char c =
    if closed then self # complain_closed();
    Buffer.add_char buffer c

  method output_string s =
    if closed then self # complain_closed();
    Buffer.add_string buffer s

  method output_byte b =
    if closed then self # complain_closed();
    Buffer.add_char buffer (Char.chr b)

  method output_buffer b =
    if closed then self # complain_closed();
    Buffer.add_buffer buffer b

  method output_channel ?len ch =
    if closed then self # complain_closed();
    ignore(copy_channel ?len ch (self : #out_obj_channel :> out_obj_channel))

  method flush() = 
    if closed then self # complain_closed();
    ()

  method close_out() =
    if not closed then (
      closed <- true;
      onclose()
    )

  method pos_out = 
    if closed then self # complain_closed();
    Buffer.length buffer

end
;;


class output_netbuffer ?(onclose = fun () -> ()) buffer : out_obj_channel =
object(self)
  val buffer = buffer
  val onclose = onclose
  val mutable closed = false
  val mutable ch_pos = 0

  method private complain_closed() =
    raise Closed_channel

  method output buf pos len =
    if closed then self # complain_closed();
    Netbuffer.add_sub_string buffer buf pos len;
    ch_pos <- ch_pos + len;
    len

  method really_output buf pos len =
    if closed then self # complain_closed();
    Netbuffer.add_sub_string buffer buf pos len;
    ch_pos <- ch_pos + len;

  method output_char c =
    if closed then self # complain_closed();
    Netbuffer.add_string buffer (String.make 1 c);
    ch_pos <- ch_pos + 1;

  method output_string s =
    if closed then self # complain_closed();
    Netbuffer.add_string buffer s;
    ch_pos <- ch_pos + String.length s

  method output_byte b =
    if closed then self # complain_closed();
    Netbuffer.add_string buffer (String.make 1 (Char.chr b));
    ch_pos <- ch_pos + 1;

  method output_buffer b =
    if closed then self # complain_closed();
    Netbuffer.add_string buffer (Buffer.contents b);
    ch_pos <- ch_pos + Buffer.length b;

  method output_channel ?len ch =
    if closed then self # complain_closed();
    ignore(copy_channel ?len ch (self : #out_obj_channel :> out_obj_channel))

  method flush() = 
    if closed then self # complain_closed();
    ()

  method close_out() =
    if not closed then (
      closed <- true;
      onclose()
    )

  method pos_out = 
    if closed then self # complain_closed();
    ch_pos
    (* We cannot return Netbuffer.length b as [pos_out] (like in the class
     * [output_buffer]) because the user of this class is allowed to delete
     * data from the netbuffer. So we manually count how many bytes are
     * ever appended to the netbuffer.
     * This behavior is especially needed by [pipe_channel] below.
     *)
end
;;


class output_null ?(onclose = fun () -> ()) () : out_obj_channel =
object(self)
  val mutable closed = false
  val mutable pos = 0
  method private complain_closed() =
    raise Closed_channel
  method output s start len =
    if closed then self # complain_closed();
    pos <- pos + len; len
  method really_output s start len =
    if closed then self # complain_closed();
    pos <- pos + len
  method output_char _ =
    if closed then self # complain_closed();
    pos <- pos + 1
  method output_string s =
    if closed then self # complain_closed();
    pos <- pos + String.length s
  method output_byte _ =
    if closed then self # complain_closed();
    pos <- pos + 1
  method output_buffer b =
    if closed then self # complain_closed();
    pos <- pos + Buffer.length b
  method output_channel ?len ch =
    if closed then self # complain_closed();
    ignore(copy_channel ?len ch (self : #out_obj_channel :> out_obj_channel))
  method flush() = 
    if closed then self # complain_closed();
  method close_out() =
    closed <- true
  method pos_out =
    if closed then self # complain_closed();
    pos
end ;;


let with_out_obj_channel ch f =
  try
    let result = f ch in
    (* we _have_ to flush here because close_out often does no longer
       report exceptions
     *)
    ( try ch # flush() with Closed_channel -> ());
    ( try ch # close_out() with Closed_channel -> ());
    result
  with
      e ->
	( try ch # close_out() with Closed_channel -> ());
	raise e
;;


class virtual augment_raw_out_channel =
object (self)
  method virtual output : string -> int -> int -> int
  method virtual close_out : unit -> unit
  method virtual flush : unit -> unit
  method virtual pos_out : int

  method really_output s pos len =
    let rec print_rest n =
      if n < len then
	let m = self # output s (pos+n) (len-n) in
	if m=0 then raise Sys_blocked_io;
	print_rest (n+m)
      else
	()
    in
    print_rest 0

  method output_char c =
    ignore(self # output (String.make 1 c) 0 1)

  method output_byte n =
    ignore(self # output (String.make 1 (Char.chr n)) 0 1)

  method output_string s =
    self # really_output s 0 (String.length s)

  method output_buffer b =
    self # output_string (Buffer.contents b)

  method output_channel ?len ch =
    ignore(copy_channel ?len ch (self : #out_obj_channel :> out_obj_channel))

end
;;


class lift_raw_out_channel (r : raw_out_channel) =
object(self)
  inherit augment_raw_out_channel

  method output s p l =
    r # output s p l

  method flush () =
    r # flush()

  method close_out () =
    r # close_out()

  method pos_out =
    r # pos_out

end;;


class lift_rec_out_channel ?(start_pos_out = 0) (r : rec_out_channel) =
object(self)
  inherit augment_raw_out_channel

  val mutable closed = false
  val mutable pos_out = start_pos_out

  method output s p l =
    if closed then raise Closed_channel;
    let n = r # output s p l in
    pos_out <- pos_out + n;
    n

  method flush() =
    if closed then raise Closed_channel;
    r # flush();

  method close_out () =
    if not closed then (
      closed <- true;
      r # close_out()
    )

  method pos_out =
    if closed then raise Closed_channel;
    pos_out

end;;


class buffered_raw_out_channel 
      ?(buffer_size = 4096)
      (ch : raw_out_channel) : raw_out_channel =
object (self)
  val out = ch
  val bufsize = buffer_size
  val buf = String.create buffer_size
  val mutable bufpos = 0
  val mutable closed = false

  method output s pos len =
    if closed then raise Closed_channel;
    let n = min len (bufsize - bufpos) in
    String.blit s pos buf bufpos n;
    bufpos <- bufpos + n;
    if bufpos = bufsize then
      self # flush();
    n

  method flush() =
    let k = ref 0 in
    while !k < bufpos do
      k := !k + (ch # output buf !k (bufpos - !k))
    done;
    bufpos <- 0;
    ch # flush()

  method close_out() = 
    ( try
	self # flush()
      with
	| error ->
	    Netlog.logf `Err
	      "Netchannels: Suppressed error in close_out: %s"
	      (Netexn.to_string error);
    );
    ch # close_out();
    closed <- true

  method pos_out =
    (ch # pos_out) + bufpos
end
;;


type lift_out_arg = [ `Rec of rec_out_channel | `Raw of raw_out_channel ]

let lift_out ?(buffered=true) ?buffer_size (x : lift_out_arg) =
  match x with
      `Rec r when not buffered ->
	new lift_rec_out_channel r
    | `Rec r when buffered ->
	let r' = new lift_rec_out_channel r in
	let r'' = new buffered_raw_out_channel ?buffer_size (r' :> raw_out_channel) in
	new lift_raw_out_channel r''
    | `Raw r when not buffered ->
	new lift_raw_out_channel r
    | `Raw r when buffered ->
	let r' = new buffered_raw_out_channel ?buffer_size r in
	new lift_raw_out_channel r'
    | _ -> failwith "Impossible"
;;


(************************* raw channels *******************************)

class input_descr_prelim ?(blocking=true) ?(start_pos_in = 0) fd =
  let fd_style = Netsys.get_fd_style fd in
object (self)
  val fd_in = fd
  val mutable pos_in = start_pos_in
  val mutable closed_in = false

  method private complain_closed() =
    raise Closed_channel

  method input buf pos len =
    if closed_in then self # complain_closed();
    try
      let n = Netsys.gread fd_style fd_in buf pos len in
      pos_in <- pos_in + n;
      if n=0 && len>0 then raise End_of_file;
      n
    with
	Unix.Unix_error(Unix.EINTR,_,_) ->
	  self # input buf pos len
      | Unix.Unix_error(Unix.EAGAIN,_,_)
      | Unix.Unix_error(Unix.EWOULDBLOCK,_,_) ->
	  if blocking then (
	    let _  = Netsys.restart 
	      (Netsys.wait_until_readable fd_style fd) (-1.0) in
	    self # input buf pos len
	  )
	  else 0
	  
  
  method close_in () =
    if not closed_in then (
      Netsys.gclose fd_style fd_in; 
      closed_in <- true
    )

  method pos_in =
    if closed_in then self # complain_closed();
    pos_in
end
;;


class input_descr ?blocking ?start_pos_in fd : raw_in_channel = 
  input_descr_prelim ?blocking ?start_pos_in fd
;;


class output_descr_prelim ?(blocking=true) ?(start_pos_out = 0) fd =
  let fd_style = Netsys.get_fd_style fd in
object (self)
  val fd_out = fd
  val mutable pos_out = start_pos_out
  val mutable closed_out = false

  method private complain_closed() =
    raise Closed_channel

  method output buf pos len =
    if closed_out then self # complain_closed();
    try
      let n = Netsys.gwrite fd_style fd_out buf pos len in
      pos_out <- pos_out + n;
      n
    with
	Unix.Unix_error(Unix.EINTR,_,_) ->
	  self # output buf pos len
      | Unix.Unix_error(Unix.EAGAIN,_,_)
      | Unix.Unix_error(Unix.EWOULDBLOCK,_,_) ->
	  if blocking then (
	    let _  = Netsys.restart 
	      (Netsys.wait_until_writable fd_style fd) (-1.0) in
	    self # output buf pos len
	  )
	  else
	    0
  
  method close_out () =
    if not closed_out then (
      ( try
	  Netsys.gshutdown fd_style fd Unix.SHUTDOWN_SEND
	with
	  | Netsys.Shutdown_not_supported -> ()
	  | Unix.Unix_error(Unix.EAGAIN, _, _) ->
	      (* FIXME. We block here even when non-blocking semantics
               is requested. We do this because most programmers would
               be surprised to get EAGAIN when closing a channel.
               Actually, this only affects Win32 output threads.
	       *)
	      let _  = Netsys.restart 
		(Netsys.wait_until_writable fd_style fd) (-1.0) in
	      Netsys.gshutdown fd_style fd Unix.SHUTDOWN_SEND
	  | Unix.Unix_error(Unix.EPERM, _, _) ->
	      ()
      );
      Netsys.gclose fd_style fd_out; 
      closed_out <- true
    )

  method pos_out =
    if closed_out then self # complain_closed();
    pos_out

  method flush () =
    if closed_out then self # complain_closed()
end
;;


class output_descr ?blocking ?start_pos_out fd : raw_out_channel =
  output_descr_prelim ?blocking ?start_pos_out fd 
;;


class socket_descr ?blocking ?(start_pos_in = 0) ?(start_pos_out = 0) fd 
      : raw_io_channel =
  let fd_style = Netsys.get_fd_style fd in
  let () =
    match fd_style with
      | `Recv_send _
      | `Recv_send_implied
      | `W32_pipe -> ()
      | _ ->
	  failwith "Netchannels.socket_descr: This type of descriptor is \
                    unsupported"
  in
object (self)
  inherit input_descr_prelim ?blocking ~start_pos_in fd
  inherit output_descr_prelim ?blocking ~start_pos_out fd 
  
  method private gen_close cmd =
    ( try
	Netsys.gshutdown fd_style fd cmd
      with
	| Netsys.Shutdown_not_supported -> ()
	| Unix.Unix_error(Unix.EAGAIN, _, _) -> assert false
	| Unix.Unix_error(Unix.EPERM, _, _) -> ()
    );
    if cmd = Unix.SHUTDOWN_ALL then
      Netsys.gclose fd_style fd


  method close_in () =
    if not closed_in then (
      closed_in <- true;
      if closed_out then
	self # gen_close Unix.SHUTDOWN_ALL
      else
	self # gen_close Unix.SHUTDOWN_RECEIVE
    )

  method close_out () =
    if not closed_out then (
      closed_out <- true;
      if closed_in then
	self # gen_close Unix.SHUTDOWN_ALL
      else
	self # gen_close Unix.SHUTDOWN_SEND
    )
end
;;


(************************** transactional *****************************)

class buffered_trans_channel ?(close_mode = (`Commit : close_mode)) 
			      (ch : out_obj_channel)
                              : trans_out_obj_channel =
  let _transbuf = Buffer.create 50 in
object (self)
  val transbuf   = _transbuf
  val trans      = new output_buffer _transbuf
  val out        = ch
  val close_mode = close_mode

  method output         = trans # output
  method really_output  = trans # really_output
  method output_char    = trans # output_char
  method output_string  = trans # output_string
  method output_byte    = trans # output_byte
  method output_buffer  = trans # output_buffer
  method output_channel = trans # output_channel
  method flush          = trans # flush

  method close_out() =
    ( try
	( match close_mode with
	      `Commit   -> self # commit_work()
	    | `Rollback -> self # rollback_work()
	)
      with
	| error ->
	    Netlog.logf `Err
	      "Netchannels: Suppressed error in close_out: %s"
	      (Netexn.to_string error);
    );
    trans # close_out();
    out # close_out()


  method pos_out =
    out # pos_out + trans # pos_out

  method commit_work() =
    try
      out # output_buffer transbuf;
      out # flush();
      Buffer.reset transbuf
    with
	err ->
	  self # rollback_work();   (* reset anyway *)
	  raise err

  method rollback_work() =
    Buffer.reset transbuf

end
;;


let make_temporary_file 
  ?(mode = 0o600) 
  ?(limit = 1000) 
  ?(tmp_directory = Netsys_tmp.tmp_directory() ) 
  ?(tmp_prefix = "netstring")
  () =
  (* Returns (filename, in_channel, out_channel). *)
  let rec try_creation n =
    try
      let fn =
        Filename.concat
          tmp_directory
          (Netsys_tmp.tmp_prefix tmp_prefix ^ "-" ^ (string_of_int n))
      in
      let fd_in =
	Unix.openfile fn [ Unix.O_RDWR; Unix.O_CREAT; Unix.O_EXCL ] mode in
      let fd_out =
	Unix.openfile fn [ Unix.O_RDWR ] mode in
      (* For security reasons check that fd_in and fd_out are the same file: *)
      let stat_in = Unix.fstat fd_in in
      let stat_out = Unix.fstat fd_out in
      if stat_in.Unix.st_dev <> stat_out.Unix.st_dev ||
	 stat_in.Unix.st_rdev <> stat_out.Unix.st_rdev ||
	 stat_in.Unix.st_ino <> stat_out.Unix.st_ino 
      then
	raise(Sys_error("File has been replaced (security alert)"));
      let ch_in  = Unix.in_channel_of_descr fd_in in
      let ch_out = Unix.out_channel_of_descr fd_out in
      fn, ch_in, ch_out
    with
        Unix.Unix_error(Unix.EEXIST,_,_) ->
          (* This does not look very intelligent, but it is the only chance
           * to limit the number of trials.
	   * Note that we get EACCES if the directory is not writeable.
           *)
          if n > limit then
            failwith ("Netchannels: Cannot create temporary file - too many files in this temp directory: " ^ tmp_directory);
          try_creation (n+1)
      | Unix.Unix_error(e,_,_) ->
	  raise (Sys_error("Cannot create a temporary file in the directory " ^
			   tmp_directory ^ ": " ^ Unix.error_message e))
	  
  in
  try_creation 0
;;


class tempfile_trans_channel ?(close_mode = (`Commit : close_mode)) 
                              ?tmp_directory
			      ?tmp_prefix
			      (ch : out_obj_channel)
			      : trans_out_obj_channel =
  let _transname, _transch_in, _transch_out = 
    make_temporary_file ?tmp_directory ?tmp_prefix () in
object (self)
  val transch_out        = _transch_out
  val mutable transch_in = _transch_in
  val trans              = new output_channel _transch_out
  val mutable out        = ch
  val close_mode         = close_mode

  initializer
    try
      Sys.remove _transname;
      (* Remove the file immediately. This requires "Unix semantics" of the
       * underlying file system, because we don't remove the file but only
       * the entry in the directory. So we can read and write the file and
       * allocate disk space, but the file is private from now on. (It's
       * not fully private, because another process can obtain a descriptor
       * between creation of the file and removal of the entry. We should
       * keep that in mind if privacy really matters.)
       * The disk space will be freed when the descriptor is closed.
       *)
    with
	err -> 
	  close_in  _transch_in;
	  close_out _transch_out;
	  raise err

  method output         = trans # output
  method really_output  = trans # really_output
  method output_char    = trans # output_char
  method output_string  = trans # output_string
  method output_byte    = trans # output_byte
  method output_buffer  = trans # output_buffer
  method output_channel = trans # output_channel
  method flush          = trans # flush

  method close_out() =
    ( try
	( match close_mode with
	      `Commit   -> self # commit_work()
	    | `Rollback -> self # rollback_work()
	)
      with
	| error ->
	    Netlog.logf `Err
	      "Netchannels: Suppressed error in close_out: %s"
	      (Netexn.to_string error);
    );
    Pervasives.close_in transch_in;
    trans # close_out();      (* closes transch_out *)
    out # close_out()


  method pos_out =
    out # pos_out + trans # pos_out

  method commit_work() =
    let len = trans # pos_out in
    trans # flush();
    Pervasives.seek_in transch_in 0;
    let trans' = new input_channel transch_in in
    ( try 
        out # output_channel ~len trans';
        out # flush();
      with
	  err -> 
	    self # rollback_work();
	    raise err
    );
    self # clear()

  method rollback_work() = self # clear()

  method private clear() =
    (* delete the contents of the file *)
    (* First empty the file and reset the output channel: *)
    Pervasives.seek_out transch_out 0;
    Unix.ftruncate (Unix.descr_of_out_channel transch_out) 0;
    (* Renew the input channel. We create a new channel to avoid problems
     * with the internal buffer of the channel.
     * (Problem: transch_in has an internal buffer, and the buffer contains
     * old data now. So we drop the channel and create a new channel for the
     * same file descriptor. Note that we cannot set the file offset with
     * seek_in because neither the old nor the new channel is properly 
     * synchronized with the file. So we fall back to lseek.)
     *) 
    let fd = Unix.descr_of_in_channel transch_in in
    ignore(Unix.lseek fd 0 Unix.SEEK_END);                  (* set the offset *)
    transch_in <- Unix.in_channel_of_descr fd;               (* renew channel *)
    (* Now check that everything worked: *)
    assert(pos_in transch_in = 0);
    assert(in_channel_length transch_in = 0);
    (* Note: the old transch_in will be automatically finalized, but the
     * underlying file descriptor will not be closed in this case
     *)

end
;;


let id_conv incoming incoming_eof outgoing =
  (* Copies everything from [incoming] to [outgoing] *)
  let len = Netbuffer.length incoming in
  ignore
    (Netbuffer.add_inplace ~len outgoing 
       (fun s_outgoing pos len' ->
	  assert (len = len');
	  Netbuffer.blit incoming 0 s_outgoing pos len';
	  Netbuffer.clear incoming;
	  len'
       ))
;;


let call_input refill f arg =
  (* Try to satisfy the request: *)
  try f arg
  with
      Buffer_underrun ->
	(* Not enough data in the outgoing buffer. *)
	refill();
	f arg
;;


class pipe ?(conv = id_conv) ?(buffer_size = 1024) () : io_obj_channel =
  let _incoming = Netbuffer.create buffer_size in
  let _outgoing = Netbuffer.create buffer_size in
object(self)
  (* The properties as "incoming buffer" [output_super] are simply inherited
   * from [output_netbuffer]. The "outgoing buffer" [input_super] invocations
   * are delegated to [input_netbuffer]. Inheritance does not work because
   * there is no way to make the public method [shutdown] private again.
   *)

  inherit output_netbuffer _incoming as output_super
    
  val conv = conv
  val incoming = _incoming
  val outgoing = _outgoing
  val input_super = new input_netbuffer _outgoing

  val mutable incoming_eof = false
  val mutable pos_in = 0
    (* We must count positions ourselves. Can't use input_super#pos_in
     * because conv may manipulate the buffer.
     *)

  val mutable output_closed = false

  (* Input methods: *)

  method private refill() =
    conv incoming incoming_eof outgoing;
    if incoming_eof then input_super # shutdown()


  method input str pos len =
    let n = call_input self#refill (input_super#input str pos) len in
    pos_in <- pos_in + n;
    n

  method input_line() =
    let p = input_super # pos_in in
    let line = call_input self#refill (input_super#input_line) () in
    let p' = input_super # pos_in in
    pos_in <- pos_in + (p' - p);
    line

  method really_input str pos len =
    call_input self#refill (input_super#really_input str pos) len;
    pos_in <- pos_in + len

  method input_char() =
    let c = call_input self#refill (input_super#input_char) () in
    pos_in <- pos_in + 1;
    c

  method input_byte() =
    let b = call_input self#refill (input_super#input_byte) () in
    pos_in <- pos_in + 1;
    b

  method close_in() =
    (* [close_in] implies [close_out]: *)
    if not output_closed then (
      output_super # close_out();
      output_closed <- true;
    );
    input_super # close_in()

  method pos_in = pos_in

  (* [close_out] also shuts down the input side of the pipe. *)

  method close_out () =
    if not output_closed then (
      output_super # close_out();
      output_closed <- true;
    );
    incoming_eof <- true

end


class output_filter 
      (p : io_obj_channel) (out : out_obj_channel) : out_obj_channel =
object(self)
  val p = p
  val mutable p_closed = false  (* output side of p is closed *)
  val out = out
  val buf = String.create 1024  (* for copy_channel *)

  method output s pos len =
    if p_closed then raise Closed_channel;
    let n = p # output s pos len in
    self # transfer();
    n

  method really_output s pos len =
    if p_closed then raise Closed_channel;
    p # really_output s pos len;
    self # transfer();

  method output_char c =
    if p_closed then raise Closed_channel;
    p # output_char c;
    self # transfer();

  method output_string s =
    if p_closed then raise Closed_channel;
    p # output_string s;
    self # transfer();

  method output_byte b =
    if p_closed then raise Closed_channel;
    p # output_byte b;
    self # transfer();

  method output_buffer b =
    if p_closed then raise Closed_channel;
    p # output_buffer b;
    self # transfer();

  method output_channel ?len ch =
    (* To avoid large intermediate buffers, the channel is copied
     * chunk by chunk 
     *)
    if p_closed then raise Closed_channel;
    let len_to_do = ref (match len with None -> -1 | Some l -> max 0 l) in
    let buf = buf in
    while !len_to_do <> 0 do
      let n = if !len_to_do < 0 then 1024 else min !len_to_do 1024 in
      if copy_channel ~buf ~len:n ch (p :> out_obj_channel) then
	(* EOF *)
	len_to_do := 0
      else
	if !len_to_do >= 0 then
	  (len_to_do := !len_to_do - n; assert(!len_to_do >= 0));
      self # transfer();
    done

  method flush() =
    p # flush();
    self # transfer();
    out # flush()

  method close_out() =
    if not p_closed then (
      p # close_out();
      p_closed <- true;
      ( try
	  self # transfer()
	with
	  | error ->
	      (* We report the error. However, we prevent that another,
		 immediately following [close_out] reports the same
		 error again. This is done by setting p_closed.
	       *)
	      raise error
      )
    )

  method pos_out = p # pos_out

  method private transfer() =
    (* Copy as much as possible from [p] to [out] *)
    try
      (* Call [copy_channel] directly (and not the method [output_channel])
       * because we can pass the copy buffer ~buf
       *)
      ignore(copy_channel ~buf (p :> in_obj_channel) out);
      out # flush();
    with
	Buffer_underrun -> ()

end


let rec filter_input refill f arg =
  (* Try to satisfy the request: *)
  try f arg
  with
      Buffer_underrun ->
	(* Not enough data in the outgoing buffer. *)
	refill();
	filter_input refill f arg
;;


class input_filter 
      (inp : in_obj_channel) (p : io_obj_channel) : in_obj_channel =
object(self)
  val inp = inp
  val p = p
  val buf = String.create 1024  (* for copy_channel *)

  method private refill() =
    (* Copy some data from [inp] to [p] *)
    (* Call [copy_channel] directly (and not the method [output_channel])
     * because we can pass the copy buffer ~buf
     *)
    let eof = 
      copy_channel ~len:(String.length buf) ~buf inp (p :> out_obj_channel) in
    if eof then p # close_out();

  method input str pos =
    filter_input self#refill (p#input str pos)

  method input_line =
    filter_input self#refill (p#input_line)

  method really_input str pos =
    filter_input self#refill (p#really_input str pos)

  method input_char =
    filter_input self#refill (p#input_char)

  method input_byte =
    filter_input self#refill (p#input_byte)

  method close_in() =
    p#close_in();

  method pos_in = p#pos_in

end
