(* $Id: netmime.ml 1201 2008-08-31 23:41:22Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

open Netchannels

type store =
  [ `Memory
  | `File of string
  ]

exception Immutable of string

class type mime_header_ro = 
object
  method fields : (string * string) list
  method field  : string -> string
  method multiple_field : string -> string list
  method content_length : unit -> int
  method content_type : 
           unit -> (string * (string * Mimestring.s_param)list)
  method content_disposition : 
           unit -> (string * (string * Mimestring.s_param)list)
  method content_transfer_encoding : unit -> string
end

class type mime_header = 
object
  inherit mime_header_ro
  method ro : bool
  method set_fields : (string * string) list -> unit
  method update_field : string -> string -> unit
  method update_multiple_field : string -> string list -> unit
  method delete_field : string -> unit
end

class type mime_body_ro =
object
  method value : string
  method store : store
  method open_value_rd : unit -> in_obj_channel
  method finalize : unit -> unit
end

class type mime_body =
object
  inherit mime_body_ro
  method ro : bool
  method set_value : string -> unit
  method open_value_wr : unit -> out_obj_channel
end

type complex_mime_message = mime_header * complex_mime_body
and complex_mime_body =
  [ `Body of mime_body
  | `Parts of complex_mime_message list
  ]

type complex_mime_message_ro = mime_header_ro * complex_mime_body_ro
and complex_mime_body_ro =
  [ `Body of mime_body_ro
  | `Parts of complex_mime_message_ro list
  ]

(* Check that coercion is possible: *)
let _ = fun x -> (x : complex_mime_message :> complex_mime_message_ro)

type mime_message = mime_header * [ `Body of mime_body ]

type mime_message_ro = mime_header_ro * [ `Body of mime_body_ro ]

module CI : sig  (* case-insensitive strings *)
  type t
  val compare : t -> t -> int
  val make : string -> t
end = struct
  type t = string
  let compare (a_ci:t) (b_ci:t) =
    Pervasives.compare a_ci b_ci
  let make s = String.lowercase s
end

module CIMap = Map.Make(CI)
  (* Maps from case-insensitive strings to any type *)

module DL : sig  (* doubly-linked lists *)
  type 'a t
  type 'a cell
  val create : unit -> 'a t
  val is_empty : 'a t -> bool
  val cell : 'a -> 'a cell
  val contents : 'a cell -> 'a
  val first : 'a t -> 'a cell (* or Not_found *)
  val last : 'a t -> 'a cell (* or Not_found *)
  val prev : 'a cell -> 'a cell (* or Not_found *)
  val next : 'a cell -> 'a cell (* or Not_found *)
  val iter : ('a cell -> unit) -> 'a t -> unit
  val delete : 'a cell -> unit
  val insert_after : neo:'a cell -> 'a cell -> unit
  val add_at_end : neo:'a cell -> 'a t -> unit
  val replace : neo:'a cell -> 'a cell -> unit
  val of_list : 'a list -> 'a t
  val to_list : 'a t -> 'a list
end = struct
  type 'a t =
      { mutable first : 'a cell option;
	mutable last : 'a cell option;
      }
  and 'a cell =
      { mutable prev : 'a cell option;
	mutable next : 'a cell option;
	mutable list : 'a t option;
	contents : 'a;
      }
  let create() = 
    { first = None; last = None }
  let is_empty l =
    l.first = None
  let cell x =
    { prev = None; next = None; list = None; contents = x }
  let contents c =
    c.contents
  let first l =
    match l.first with Some c -> c | None -> raise Not_found
  let last l =
    match l.last with Some c -> c | None -> raise Not_found
  let prev c =
    match c.prev with Some c' -> c' | None -> raise Not_found
  let next c =
    match c.next with Some c' -> c' | None -> raise Not_found
  let iter f l =
    match l.first with
	Some c ->
	  f c;
	  let current = ref c in
	  while (let c0 = ! current in c0.next) <> None do  (* Error in camlp4 *)
	    current := next !current;
	    f !current
	  done; ()
      | None ->
	  ()
  let delete c =
    match c.list with
	Some l ->
	  ( match c.prev with
		Some p ->
		  p.next <- c.next
	      | None ->
		  l.first <- c.next
	  );
	  ( match c.next with
		Some n ->
		  n.prev <- c.prev
	      | None ->
		  l.last <- c.prev
	  );
	  c.prev <- None;
	  c.next <- None;
	  c.list <- None
      | None -> 
	  failwith "DL.delete: cannot delete free cell"
  let insert_after ~neo c =
    if neo.list <> None then
      failwith "DL.insert_after: new cell must be free";
    match c.list with
	Some l ->
	  let nx = c.next in
	  c.next <- Some neo;
	  neo.prev <- Some c;
	  ( match nx with
		Some n ->
		  n.prev <- Some neo;
		  neo.next <- Some n;
	      | None ->
		  l.last <- Some neo;
		  neo.next <- None
	  );
	  neo.list <- Some l
      | None -> 
	  failwith "DL.insert_after: cannot insert after free cell"
  let add_at_end ~neo l =
    if neo.list <> None then
      failwith "DL.insert_after: new cell must be free";
    match l.last with
	Some n ->
	  n.next <- Some neo;
	  neo.prev <- Some n;
	  neo.next <- None;
	  neo.list <- Some l;
	  l.last <- Some neo
      | None ->
	  l.last <- Some neo;
	  l.first <- Some neo;
	  neo.prev <- None;
	  neo.next <- None;
	  neo.list <- Some l
  let replace ~neo c =
    if neo.list <> None then
      failwith "DL.replace: new cell must be free";
    match c.list with
	Some l ->
	  ( match c.prev with
		Some p ->
		  p.next <- Some neo
	      | None ->
		  l.first <- Some neo
	  );
	  neo.prev <- c.prev;
	  ( match c.next with
		Some n ->
		  n.prev <- Some neo
	      | None ->
		  l.last <- Some neo
	  );
	  neo.next <- c.next;
	  neo.list <- Some l;
	  c.prev <- None;
	  c.next <- None;
	  c.list <- None
      | None -> 
	  failwith "DL.replace: cannot replace free cell"
  let of_list l =
    let dl = create() in
    List.iter
      (fun x -> 
	 add_at_end ~neo:(cell x) dl
      )
      l;
    dl
  let rec to_list dl =
    chain_to_list dl.first
  and chain_to_list chain =
    match chain with
	None -> []
      | Some c -> c.contents :: chain_to_list c.next

end


let drop_ws_re = Netstring_pcre.regexp "^[ \t\r\n]*(.*[^ \t\r\n])[ \t\r\n]*$";;

let drop_ws s =
  (* Deletes whitespace at the beginning and at the end of s, and returns
   * the new string
   *)
  match Netstring_pcre.string_match drop_ws_re s 0 with
      None -> ""
    | Some r -> Netstring_pcre.matched_group r 1 s
;;
  

class basic_mime_header ?(ro=false) h : mime_header =
object (self)
  val ro = ro
  val mutable hdr_map = lazy (assert false)
  val mutable hdr_dl  = lazy (assert false)

  initializer
    self # do_set_fields h

  method ro = ro

  method fields = 
    DL.to_list (Lazy.force hdr_dl)

  method field n = 
    let m = Lazy.force hdr_map in
    match CIMap.find (CI.make n) m with
	[] -> raise Not_found
      | cell :: _ -> snd (DL.contents cell)

  method multiple_field n =
    let m = Lazy.force hdr_map in
    try 
      List.map 
        (fun cell -> snd (DL.contents cell)) 
	(CIMap.find (CI.make n) m) 
    with Not_found -> []

  method private immutable() =
    raise (Immutable "Netmime.basic_mime_header");

  method set_fields h =
    if ro then self#immutable();
    self # do_set_fields h

  method private do_set_fields h =
    hdr_dl <- lazy (DL.of_list h);
    hdr_map <- lazy begin
      (* This seems to be expensive (O(n log n)). Because of this we do it only
       * on demand; maybe nobody accesses the header at all
       *)
      let m = ref CIMap.empty in
      DL.iter
	(fun cell ->
	   let (n,v) = DL.contents cell in
	   let n_ci = CI.make n in
	   let current = 
	     try CIMap.find n_ci !m 
	     with Not_found -> [] 
	   in
	   m := CIMap.add n_ci (cell :: current) !m;
	)
	(Lazy.force hdr_dl);
      CIMap.map List.rev !m
    end

  method update_field n v =
    self # update_multiple_field n [v]

  method update_multiple_field n vl =
    if ro then self#immutable();
    let n_ci = CI.make n in
    let m = Lazy.force hdr_map in
    let dl = Lazy.force hdr_dl in
    (* Algorithm: First try to replace existing values.
     * If there are more new values than old values,
     * at the excess values after the last old value,
     * or if not possible, at the end.
     *)
    let insert_point =
      ref None in
    let old_cells =
      ref(try CIMap.find n_ci m with Not_found -> []) in
    let new_vals  = ref vl in
    let new_cells = ref [] in
    while !old_cells <> [] || !new_vals <> [] do
      match !old_cells, !new_vals with
	  (old_cell :: old_cells'), (new_val :: new_vals') ->
	    (* Only update if the value has changed: *)
	    let (old_n, old_val) = DL.contents old_cell in
	    if old_val = new_val then (
	      new_cells := old_cell :: !new_cells;
	      insert_point := Some old_cell;
	    )
	    else (
	      let new_cell = DL.cell (n, new_val) in
	      DL.replace ~neo:new_cell old_cell;
	      insert_point := Some new_cell;
	      new_cells := new_cell :: !new_cells
	    );
	    old_cells := old_cells';
	    new_vals  := new_vals';
	| [], (new_val :: new_vals') ->
	    let new_cell = DL.cell (n, new_val) in
	    ( match !insert_point with
		  Some p ->
		    DL.insert_after ~neo:new_cell p;
		| None ->
		    DL.add_at_end ~neo:new_cell dl
	    );
	    new_vals := new_vals';
	    insert_point := Some new_cell;
	    new_cells := new_cell :: !new_cells
	| (old_cell :: old_cells'), [] ->
	    DL.delete old_cell;
	    old_cells := old_cells'
	| [], [] ->
	    assert false
    done;
    let m' = CIMap.add n_ci (List.rev !new_cells) m in
    hdr_map <- lazy m'

  method delete_field n =
    if ro then self#immutable();
    let n_ci = CI.make n in
    let m = Lazy.force hdr_map in
    let old_cells =
      try CIMap.find n_ci m with Not_found -> [] in
    List.iter DL.delete old_cells;
    let m' = CIMap.remove n_ci m in
    hdr_map <- lazy m';

  method content_length() = 
    int_of_string (drop_ws(self # field "content-length"))
  method content_type() =
    Mimestring.scan_mime_type_ep (self#field "content-type") []
  method content_disposition() =
    Mimestring.scan_mime_type_ep (self#field "content-disposition") []
  method content_transfer_encoding() = 
    String.lowercase (self # field "content-transfer-encoding")
end ;;


class complement_mime_header_ro h : mime_header =
  basic_mime_header ~ro:true h#fields ;;

let complement_mime_header_ro h =
  new complement_mime_header_ro h ;;


class memory_mime_body_int ?(ro_first=false) ?(ro=false) v : mime_body =
object (self)
  (* ro_first (not exported): whether ro for the first write access *)

  val ro' = ro
  val mutable ro = ro_first
  val mutable value = v
  val mutable finalized = false

  method value = 
    if finalized then self # finalized();
    value
  method store = 
    `Memory
  method open_value_rd() = 
    if finalized then self # finalized();
    new input_string value
  method finalize() = 
    finalized <- true

  method ro = 
    ro
  method set_value s = 
    if finalized then self # finalized();
    if ro then self#immutable() else value <- s;
    ro <- ro'

  method open_value_wr() =
    if finalized then self # finalized();
    if ro then self#immutable();
    ro <- ro';
    let b = Netbuffer.create 60 in
    new output_netbuffer ~onclose:(fun () -> value <- Netbuffer.contents b) b;

  method private immutable() =
    raise (Immutable "Netmime.memory_mime_body");

  method private finalized() =
    failwith "Netmime.memory_mime_body: object is finalized";
end ;;


class memory_mime_body ?ro =
  memory_mime_body_int ?ro_first:ro ?ro ;;

class file_mime_body_int ?(ro_first=false) ?(ro=false) ?(fin=false) f : mime_body =
object (self)
  (* ro_first (not exported): whether ro for the first write access *)

  val ro' = ro
  val mutable ro = ro_first
  val mutable finalized = false
  val fin = fin
  val filename = f
  val cached_value = Weak.create 1

  method ro = 
    ro
  method store = 
    `File filename

  method value =
    if finalized then self # finalized();
    match Weak.get cached_value 0 with
	None ->
	  with_in_obj_channel
	    (new input_channel (open_in_bin filename))
	    (fun objch ->
	       let v = string_of_in_obj_channel objch in
	       Weak.set cached_value 0 (Some v);
	       v
	    )
      | Some v -> v

  method open_value_rd() =
    if finalized then self # finalized();
    new input_channel (open_in_bin filename)

  method set_value s =
    if finalized then self # finalized();
    if ro then self#immutable();
    ro <- ro';
    with_out_obj_channel 
      (new output_channel (open_out_bin filename))
      (fun ch -> ch # output_string s);

  method open_value_wr() =
    if finalized then self # finalized();
    if ro then self#immutable();
    ro <- ro';
    new output_channel (open_out_bin filename)

  method private immutable() =
    raise (Immutable "Netmime.file_mime_body");

  method finalize () =
    if fin && not finalized then begin
      try Sys.remove filename with _ -> ()
    end;
    finalized <- true

  method private finalized() =
    failwith "Netmime.file_mime_body: object is finalized";
end ;;

class file_mime_body ?ro =
  file_mime_body_int ?ro_first:ro ?ro ;;

class complement_mime_body_ro body : mime_body =
object(self)
  val body = body
  method value = body#value
  method store = body#store
  method open_value_rd = body#open_value_rd
  method finalize = body#finalize

  method ro = true
  method set_value _ = raise (Immutable "Netmime.complement_mime_body_ro");
  method open_value_wr _ = raise (Immutable "Netmime.complement_mime_body_ro");
end ;;
  (* FIXME: complement_mime_body_ro should make a copy of the value *)



let complement_mime_body_ro body =
  new complement_mime_body_ro body;;

let rec complement_complex_mime_message_ro (h,cb) =
  (complement_mime_header_ro h,
   match cb with
       `Body b  -> `Body(complement_mime_body_ro b)
     | `Parts p -> `Parts(List.map complement_complex_mime_message_ro p)
  )
;;


let read_mime_header ?(unfold=false) ?(strip=true) ?ro stream =
  let h = Mimestring.read_header ~downcase:false ~unfold ~strip stream in
  new basic_mime_header ?ro h
;;


type multipart_style = [ `None | `Flat | `Deep ] ;;


let decode_mime_body hdr =
  let encoding =
    try hdr # content_transfer_encoding()
    with Not_found -> "7bit"
  in
  match encoding with
      ("7bit"|"8bit"|"binary") ->
	(fun s -> s)
    | "base64" ->
	(fun s -> 
	   new output_filter 
	     (new Netencoding.Base64.decoding_pipe 
	      ~url_variant:false ~accept_spaces:true ()) s)
    | "quoted-printable" ->
	(fun s ->
	   new output_filter
	     (new Netencoding.QuotedPrintable.decoding_pipe()) s)
    | _ ->
	failwith "Netmime.decode_mime_body: Unknown Content-transfer-encoding"
;;


let encode_mime_body ?(crlf = true) hdr =
  let encoding =
    try hdr # content_transfer_encoding()
    with Not_found -> "7bit"
  in
  match encoding with
      ("7bit"|"8bit"|"binary") ->
	(fun s -> s)
    | "base64" ->
	(fun s -> 
	   new output_filter 
	     (new Netencoding.Base64.encoding_pipe 
	       ~linelength:76 ~crlf ()) s)
    | "quoted-printable" ->
	(fun s ->
	   new output_filter
	     (new Netencoding.QuotedPrintable.encoding_pipe ~crlf ()) s)
    | _ ->
	failwith "Netmime.encode_mime_body: Unknown Content-transfer-encoding"
;;


let storage ?ro ?fin : store -> (mime_body * out_obj_channel) = function
    `Memory ->
      let body = new memory_mime_body_int ~ro_first:false ?ro "" in
      let body_ch = body#open_value_wr() in
      body, body_ch
  | `File filename ->
      let body = new file_mime_body_int ~ro_first:false ?ro ?fin filename in
      let body_ch = body#open_value_wr() in
      body, body_ch
;;


let rec read_mime_message
      ?unfold ?strip ?ro
      ?(multipart_style = (`Deep : multipart_style))
      ?(storage_style = fun _ -> storage ?ro `Memory)
      stream =
  
  (* First read the header: *)
  let h_obj = read_mime_header ?ro ?unfold ?strip stream in

  let mime_type, mime_type_params = 
    try h_obj#content_type() with Not_found -> "", [] in

  let multipart = "multipart/" in
  let is_multipart_type = 
    (String.length mime_type >= String.length multipart) &&
    (String.sub mime_type 0 (String.length multipart) = multipart) in

  (* Now parse the body, (with multiparts or without) *)
  let body =
    if is_multipart_type && multipart_style <> `None then begin
      (* --- Divide the message into parts: --- *)
      let boundary = 
	try List.assoc "boundary" mime_type_params 
	with Not_found -> failwith "Netmime.read_mime_message: missing boundary parameter"
      in
      let multipart_style =  (* of the sub parser *)
	if multipart_style = `Flat then `None else multipart_style in
      `Parts
	(Mimestring.read_multipart_body
	   (read_mime_message ?ro ~multipart_style ~storage_style)
	   (Mimestring.param_value boundary)
	   stream
	)
    end
    else begin
      (* --- Read the body and optionally decode it: --- *)
      (* Where to store the body: *)
      let decoder = decode_mime_body h_obj in
      let body, body_ch = storage_style h_obj in
      if 
	with_out_obj_channel 
	  (decoder body_ch)
	  (fun body_ch' ->
	     body_ch' # output_channel (stream :> in_obj_channel);
	     body_ch' <> body_ch
	  )
      then
	body_ch # close_out();
      `Body body
    end
  in
	
  (h_obj, body)
;;


let rec augment_message (hdr,cbody) =
  (* Computes the content-transfer-encoding field for multipart messages.
   * The resulting message uses `Parts_ag(cte,parts) instead of `Parts(parts)
   * where cte is the content-transfer-encoding field.
   *)
  match cbody with
      `Body _ as b -> (hdr,b)
    | `Parts p ->
	let p' = List.map augment_message p in
	let mp_cte_id =
	  List.fold_left
	    (fun x (hdr,body) ->
	       let cte = 
		 match body with
		     `Body _ -> 
		       (try hdr#content_transfer_encoding() 
			with Not_found -> "7bit")
		   | `Parts_ag(cte,_) -> cte
	       in
	       let cte_id =
		 match cte with
		     "7bit" | "quoted-printable" | "base64" -> 0
		   | "8bit" -> 1
		   | _ -> 2
	       in
	       max x cte_id
	    )
	    0
	    p' in
	let mp_cte = match mp_cte_id with
	    0 -> "7bit"
	  | 1 -> "8bit"
	  | 2 -> "binary"
	  | _ -> assert false
	in
	(hdr, `Parts_ag(mp_cte,p'))
;;


let rec write_mime_message_int ?(wr_header = true) ?(wr_body = true) ?(nr = 0) 
                               ?ret_boundary ?(crlf = true)
                               outch (hdr,cbody) =

  let eol = if crlf then "\r\n" else "\n" in

  let mk_boundary parts =
    (* For performance reasons, gather random data only from the first
     * `Body
     *)
    let rec gather_data parts =
      match parts with
	  (_,`Body body) :: parts' ->
	    let s = String.make 240 ' ' in  (* So it is in the minor heap *)
	    with_in_obj_channel
	      (body # open_value_rd())
	      (fun ch ->
		 try 
		   ignore(ch # input s 0 240)
		 with
		     End_of_file -> ()  (* When body is empty *)
	      );
	    [s]
	| (_,`Parts_ag(_, parts'')) :: parts' ->
	    (try gather_data parts'' with Not_found -> gather_data parts')
	| [] ->
	    raise Not_found
    in
    let data = try gather_data parts with Not_found -> [] in
    Mimestring.create_boundary ~random:data ~nr ()
  in

  match cbody with
      `Body body ->
	(* Write the header as it is, and append the body *)
	if wr_header then
	  Mimestring.write_header ~eol ~soft_eol:eol outch hdr#fields;
	if wr_body then begin
	  let outch' = encode_mime_body ~crlf hdr outch in
	  with_in_obj_channel
	    (body # open_value_rd())
	    (fun bodych -> outch' # output_channel bodych);
	  if outch' <> outch then outch' # close_out();
	end

    | `Parts_ag(cte,parts) ->
	if parts = [] then
	  failwith "Netmime.write_mime_message: Cannot write multipart message with empty list of parts";
	(* If the header does not include a proper content-type field, repair
	 * this now.
	 *)
	let hdr' = new basic_mime_header hdr#fields in
	    (* hdr' will contain the repaired header as side effect *)
	let boundary =
	  try
	    let ctype,params = 
	      try
		hdr # content_type()   (* or Not_found *)
	      with
		  Not_found as ex -> raise ex  (* falls through to next [try] *)
		| ex ->
		    failwith ("Netmime.write_mime_message: Cannot parse content-type field: " ^ Netexn.to_string ex)
	    in  
	    if String.length ctype < 10 || String.sub ctype 0 10 <> "multipart/"
	    then 
	      failwith "Netmime.write_mime_message: The content type of a multipart message must be 'multipart/*'";
	    try
	      let b = List.assoc "boundary" params in   (* or Not_found *)
	      Mimestring.param_value b
	    with
		Not_found ->
		  (* Add the missing boundary parameter: *)
		  let b = mk_boundary parts in
		  let ctype_field = 
		    hdr # field "content-type" ^ 
		    ";" ^ eol ^ " boundary=\"" ^ b ^ "\""  in
		  hdr' # update_field "content-type" ctype_field;
		  b
	  with
	      Not_found ->
		(* Add the missing content-type header: *)
		let b = mk_boundary parts in
		let ctype_field = 
		  "multipart/mixed;" ^ eol ^ " boundary=\"" ^ b ^ "\"" in
		hdr' # update_field "content-type" ctype_field;
		b
	in
	(* Now fix the content-transfer-encoding field *)
	hdr' # update_field "content-transfer-encoding" cte;
	(* Write now the header fields *)
	if wr_header then
	  Mimestring.write_header ~eol ~soft_eol:eol outch hdr'#fields;
	(* Write the parts: *)
	if wr_body then begin
	  let boundary_string = "--" ^ boundary ^ eol in
	  List.iter
	    (fun part ->
	       outch # output_string boundary_string;
	       write_mime_message_int 
	         ~wr_header:true ~wr_body:true ~nr:(nr + 1) ~crlf outch part;
	       outch # output_string eol;
	    )
	    parts;
	  outch # output_string ("--" ^ boundary ^ "--" ^ eol);
	end;
	( match ret_boundary with
	      None -> ()
	    | Some r -> r := boundary
	)
;;


let write_mime_message ?wr_header ?wr_body ?nr ?ret_boundary ?crlf ch msg =
  write_mime_message_int 
     ?wr_header ?wr_body ?nr ?ret_boundary ?crlf
     ch (augment_message msg)
;;
