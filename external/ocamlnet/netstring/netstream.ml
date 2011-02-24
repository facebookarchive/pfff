(* $Id: netstream.ml 1412 2010-02-15 16:20:27Z gerd $
 * ----------------------------------------------------------------------
 *
 *)


open Netchannels;;


class type in_obj_stream =
object
  inherit Netchannels.in_obj_channel
  method block_size : int
  method window : Netbuffer.t
  method want : int -> unit
  method want_another_block : unit -> unit
  method window_length : int
  method window_at_eof : bool
  method skip : int -> unit
end


class virtual input_methods init_s_netbuf =
object(self)
  val mutable s_pos = 0
  val mutable s_at_eof = false
  val s_netbuf = init_s_netbuf
  val mutable s_closed = false


  method virtual want : int -> unit

  method virtual want_another_block : unit -> unit

  method virtual window_length : int

  method virtual input : string -> int -> int -> int


  (* The following input methods base all on [input] *)

  method really_input buf pos len =
    if s_closed then raise Netchannels.Closed_channel;
    let rec read p =
      let l = self # input buf (pos+p) (len-p) in
      let p' = p + l in
      if p' = len then
	()
      else (
	if l=0 then raise Sys_blocked_io;
	read p'
      )
    in
    self # want len;  (* may raise Buffer_underrun *)
    read 0


  method input_char () =
    let s = String.create 1 in
    self # really_input s 0 1;
    s.[0]


  method input_byte () =
    let s = String.create 1 in
    self # really_input s 0 1;
    int_of_char s.[0]


  method input_line () =
    (* CHECK: Are the different end of line conventions important here? *)
    let rec find_eol() =
      try
	Netbuffer.index_from s_netbuf 0 '\n'    (* or Not_found *)
      with
	  Not_found ->
	    if not s_at_eof then begin
	      self # want_another_block();  (* may raise Buffer_underrun *)
	      find_eol()
	    end
	    else self#window_length
    in
    if s_closed then raise Netchannels.Closed_channel;
    let n = find_eol() in
    if n >= self#window_length then begin
      if n = 0 then raise End_of_file;
      let s = String.create n in
      self#really_input s 0 n;
      s
    end
    else begin
      let s = String.create n in
      self#really_input s 0 n;
      ignore(self#input_char());    (* '\n' *)
      s
    end
    

  method pos_in =
    if s_closed then raise Netchannels.Closed_channel;
    s_pos


end


class input_stream ?len ?(block_size = 4096) in_ch : in_obj_stream =
object (self)
  val s_channel = (in_ch : in_obj_channel)
  val s_maxlength = len
  val s_blocksize = block_size
  val mutable s_underrun = false

  inherit input_methods (Netbuffer.create block_size)

  (* Note: This implementation must even work if [in_ch] is a pipe,
   * and raises Buffer_underrun from time to time. This may happen
   * at inconvenient situations. In this case the flag s_underrun stores 
   * whether an underrun happened, and should be reported later.
   *)

  initializer 
    try
      self # want_minimum()   (* may raise Buffer_underrun *)
    with
	Buffer_underrun ->
	  s_underrun <- true

  method private debug msg =
    prerr_endline (msg ^ ": s_pos=" ^ string_of_int s_pos ^ 
		   " s_at_eof=" ^ string_of_bool s_at_eof ^ 
		   " buflen=" ^ string_of_int (Netbuffer.length s_netbuf) ^
		   " s_closed=" ^ string_of_bool s_closed);

  method block_size = s_blocksize
    (* The block size is a static property, so never raise Closed_channel *)

  method window = 
    if s_closed then raise Netchannels.Closed_channel;
    s_netbuf

  method window_length = 
    if s_closed then raise Netchannels.Closed_channel;
    Netbuffer.length s_netbuf

  method window_at_eof = 
    if s_closed then raise Netchannels.Closed_channel;
    s_at_eof

  method want_another_block() =
    if s_closed then raise Netchannels.Closed_channel;
    if not s_at_eof then begin
      (* How much are we allowed to read? *)
      let m =
	match s_maxlength with
	    None   -> s_blocksize
	  | Some l -> min (l - s_pos - Netbuffer.length s_netbuf) s_blocksize
      in
      assert(m >= 0);
      (* Try to read m bytes: *)
      let rec read_block k =
	if k < m then
	  let n = 
	    Netbuffer.add_inplace ~len:(m-k) 
	      s_netbuf 
	      (s_channel # input)    
	        (* may raise End_of_file, Buffer_underrun *)
	  in
	  ( if n > 0 then
	      read_block (k+n)
	    else
	      raise Sys_blocked_io
	  )
	else
	  ()
      in
      try
	if m=0 then
	  (* Artificial EOF because len is reached *)
	  s_at_eof <- true
	else
	  read_block 0
      with
	  End_of_file ->
	    s_at_eof <- true
    end;
    (* self # debug "after stream#want_another_block"; *)
    (* Unix.sleep 1; *)


  method want n =
    if s_closed then raise Netchannels.Closed_channel;
    while not s_at_eof && Netbuffer.length s_netbuf < n do
      self # want_another_block()
    done


  method private want_minimum() =
    self # want s_blocksize


  method skip len =
    if s_closed then raise Netchannels.Closed_channel;
    let rec read len =
      if len > 0 then begin
	let k = min (Netbuffer.length s_netbuf) len in
	Netbuffer.delete s_netbuf 0 k;
	s_pos <- s_pos + k;
	self # want_minimum();    (* may raise Buffer_underrun *)
	if k > 0 then read (len - k)
      end
    in
    read len


  method input buf pos len =
    if s_closed then raise Netchannels.Closed_channel;
    if s_underrun then (
      self # want_minimum();  (* may raise Buffer_underrun *)
      s_underrun <- false;
    );
    (* Assertion: Either window length >= minimum, or eof *)
    let len' = min len (Netbuffer.length s_netbuf) in
    Netbuffer.blit s_netbuf 0 buf pos len';
    Netbuffer.delete s_netbuf 0 len';
    s_pos <- s_pos + len';
    ( try 
	self # want_minimum();  (* may raise Buffer_underrun *)
      with
	  Buffer_underrun ->
	    s_underrun <- true
    );
    if len'=0 && len>0 then raise End_of_file;
    len'


  method close_in () =
    if not s_closed then (
      s_channel # close_in();
      s_closed <- true;
    )


end


(*
let find_prefix s1 pos len s2 =
  (* Checks where a non-empty prefix of [s2] occurs at the end of the substring
   * of [s1] beginning at [pos] with length [len]. The function returns 
   * the position [p] of the prefix in [s1]. 
   * The function raises Not_found if it does not find a prefix.
   * POSTCONDITION:
   * - s1[p..p+n-1] = s2[0..n-1] for some biggest n, n <= String.length s2
   *   "The string s1 contains the prefix of s2 at position p, and the
   *   prefix has the maximum length n."
   * - n < String.length s2 ==> p+n = String.length s1
   *   "If the prefix is a proper prefix, it occurs at the end of s1"
   *)
  assert(String.length s2 > 0);
  let l1 = min (String.length s1) (pos+len) in
  let l2 = String.length s2 in
  let s2c0 = s2.[0] in
  let rec check_rec p k =
    k >= l2 || p+k >= l1 || (s1.[p+k] = s2.[k] && check_rec p (k+1)) in
  let rec search_rec p =
    if p >= l1 then raise Not_found;
    let p' = String.index_from s1 p s2c0 in  (* or Not_found *)
    if p' >= l1 then raise Not_found;
    if check_rec p' 0 then
      p'
    else
      search_rec (p'+1)
  in
  search_rec pos
;;
*)


class sub_stream ?len ?delimiter in_stream : in_obj_stream =
object(self)
  val s = (in_stream : in_obj_stream)
  val mutable s_winlen = 0
  val mutable s_del = None    (* initialized below *)
  val s_len = len
  val mutable s_underrun = false

  inherit input_methods (in_stream # window)

  initializer
    (match delimiter with
	 Some "" -> invalid_arg "new Netstream.sub_stream";
       | Some d  -> s_del <- Some(d, Netaux.KMP.make_pattern d)
       | None    -> s_del <- None
    );
    (match s_len with 
	 Some l -> if l<0 then invalid_arg "new Netstream.sub_stream";
       | None   -> ()
    );
    try
      self # want_minimum()
    with
	Buffer_underrun ->
	  s_underrun <- true

  method block_size = s # block_size

  method window = 
    if s_closed then raise Netchannels.Closed_channel;
    s_netbuf

  method window_length = 
    if s_closed then raise Netchannels.Closed_channel;
    s_winlen

  method window_at_eof = 
    if s_closed then raise Netchannels.Closed_channel;
    s_at_eof


  method private compute_winlen() =
    (* sets [s_winlen], [s_at_eof], and returns whether the current window
     * is "ambigous" (it is not clear if the stream does end or does not
     * end)
     *)
    let ambigous = ref false in
    let w = s#window in
    let wlen = s#window_length in
    let weof = s#window_at_eof in
    begin match s_del with
	None -> 
	  s_winlen <- wlen;
	  s_at_eof <- weof;
      | Some(d,pat) ->
	  let p = Netaux.KMP.find_pattern
		    pat ~len:wlen (Netbuffer.unsafe_buffer w) in
	  if p >= wlen then begin
	    (* Delimiter d does not occur in the buffer *)
	    s_winlen <- wlen;
	    s_at_eof <- weof;
	  end
	  else 
	    if (p + String.length d) > wlen then begin
	      (* Case: prefix is a proper prefix *)
	      ambigous := not weof;
	      s_winlen <- wlen;
	      s_at_eof <- weof;
	    end
	    else begin
	      (* Case: [d] occurs in the window *)
	      s_winlen <- p;
	      s_at_eof <- true;
	    end
    end;
    begin match s_len with
	None ->
	  ()
      | Some l ->
	  if l - s_pos < s_winlen then begin
	    ambigous := false;
	    s_winlen <- l - s_pos;
	    s_at_eof <- true;
	  end
    end;
    !ambigous


  method want_another_block() =
    if s_closed then raise Netchannels.Closed_channel;
    s # want_another_block();   (* may raise Buffer_underrun *)
    while self # compute_winlen() do
      s # want_another_block(); (* may raise Buffer_underrun *)
    done

  method want n =
    if s_closed then raise Netchannels.Closed_channel;
    while not s_at_eof && s_winlen < n do
      self # want_another_block()   (* may raise Buffer_underrun *)
    done

  method private want_minimum() =
    if self # compute_winlen() then
      self # want_another_block();   (* may raise Buffer_underrun *)
    self # want s#block_size


  method skip len =
    if s_closed then raise Netchannels.Closed_channel;
    let rec read len =
      if len > 0 then begin
	let k = min s_winlen len in
	s # skip k;              (* may raise Buffer_underrun *)
	s_pos <- s_pos + k;
	self # want_minimum();   (* may raise Buffer_underrun *)
	if k > 0 then read (len - k)
      end
    in
    read len


  method input buf pos len =
    if s_closed then raise Netchannels.Closed_channel;
    if s_underrun then (
      self # want_minimum();  (* may raise Buffer_underrun *)
      s_underrun <- false;
    );
    (* Assertion: Either window length >= minimum, or eof *)
    let len' = min len s_winlen in
    Netbuffer.blit s_netbuf 0 buf pos len';
    s # skip len';              (* never raises Buffer_underrun *)
    s_pos <- s_pos + len';
    ( try
	self # want_minimum();
      with
	  Buffer_underrun ->
	    s_underrun <- true
    );
    if len'=0 && len>0 then raise End_of_file;
    len'


  method close_in () =
    if not s_closed then (
      s # close_in();
      s_closed <- true;
    )

end


let print_in_obj_stream fmt s =
  Format.fprintf fmt
    "<NETSTREAM pos_in:%d window_length:%d eof=%b>"
    s#pos_in
    s#window_length
    s#window_at_eof
;;
