(* $Id: netsys_mem.ml 1491 2010-11-21 23:17:19Z gerd $ *)

open Printf

type memory = 
    (char,Bigarray.int8_unsigned_elt,Bigarray.c_layout) Bigarray.Array1.t

external netsys_blit_memory_to_string :
           memory -> int -> string -> int -> int -> unit
  = "netsys_blit_memory_to_string"

external netsys_blit_string_to_memory : 
           string -> int -> memory ->  int -> int -> unit
  = "netsys_blit_string_to_memory"

let blit_memory_to_string mem memoff s soff len =
  let memlen = Bigarray.Array1.dim mem in
  let slen = String.length s in
  if len < 0 || memoff < 0 || memoff > memlen - len || 
     soff < 0 || soff > slen - len 
  then
    invalid_arg "Netsys_mem.blit_memory_to_string";
  netsys_blit_memory_to_string mem memoff s soff len

let blit_string_to_memory s soff mem memoff len =
  let memlen = Bigarray.Array1.dim mem in
  let slen = String.length s in
  if len < 0 || memoff < 0 || memoff > memlen - len || 
     soff < 0 || soff > slen - len 
  then
    invalid_arg "Netsys_mem.blit_string_to_memory";
  netsys_blit_string_to_memory s soff mem memoff len

external memory_address : memory -> nativeint
  = "netsys_memory_address"

external getpagesize : unit -> int
  = "netsys_getpagesize"

external netsys_alloc_memory_pages : nativeint -> int -> memory
  = "netsys_alloc_memory_pages"

let alloc_memory_pages ?(addr=0n) len =
  netsys_alloc_memory_pages addr len

external alloc_aligned_memory : int -> int -> memory
  = "netsys_alloc_aligned_memory"

external netsys_map_file : 
           Unix.file_descr -> int64 -> nativeint -> bool -> int -> memory
  = "netsys_map_file"

let memory_map_file fd ?(pos=0L) ?(addr=0n) shared size =
  netsys_map_file fd pos addr shared size

external memory_unmap_file : memory -> unit
  = "netsys_memory_unmap_file"

external netsys_zero_pages : memory -> int -> int -> unit
  = "netsys_zero_pages"

let zero_pages mem pos len =
  let memlen = Bigarray.Array1.dim mem in
  if len < 0 || pos < 0 || pos > memlen - len then
    invalid_arg "Netsys_mem.zero_pages (index out of range)";
  netsys_zero_pages mem pos len

external as_value : memory -> int -> 'a
  = "netsys_as_value"

external netsys_value_area_add : memory -> unit 
  = "netsys_value_area_add"

external netsys_value_area_remove : memory -> unit 
  = "netsys_value_area_remove"

let value_area m =
  netsys_value_area_add m;
  Gc.finalise netsys_value_area_remove m;
  ()

external cmp_string : string -> string -> int
  = "netsys_cmp_string"

external netsys_init_string : memory -> int -> int -> unit
  = "netsys_init_string"

let init_string_bytelen len =
  let ws = Sys.word_size / 8 in  (* word size in bytes *)
  ((len + ws) / ws + 1) * ws
  

exception Out_of_space

let _ = 
  Callback.register_exception "Netsys_mem.Out_of_space" Out_of_space



let init_string mem offset len =
  let ws = Sys.word_size / 8 in  (* word size in bytes *)
  let memlen = Bigarray.Array1.dim mem in
  if offset < 0 || len < 0 then
    invalid_arg "Netsys_mem.init_string";
  let blen = init_string_bytelen len in
  if blen > memlen - offset then
    raise Out_of_space;
  netsys_init_string mem offset len;
  (offset+ws, blen)


type init_value_flag =
  | Copy_bigarray
  | Copy_custom_int
  | Copy_atom
  | Copy_simulate

type custom_ops = nativeint

external netsys_init_value : 
  memory -> int -> 'a -> init_value_flag list -> nativeint -> 
  (string * custom_ops) list -> (int * int)
  = "netsys_init_value_bc" "netsys_init_value"

let init_value ?targetaddr ?(target_custom_ops=[]) mem offset v flags =
  let taddr = 
    match targetaddr with
      | None ->
	  memory_address mem
      | Some a ->
	  a in
  netsys_init_value mem offset v flags taddr target_custom_ops

external get_custom_ops : 'a -> string * custom_ops
  = "netsys_get_custom_ops"

external copy_value : init_value_flag list -> 'a -> 'a
  = "netsys_copy_value"

external netsys_mem_read : Unix.file_descr -> memory -> int -> int -> int
  = "netsys_mem_read"

external netsys_mem_write : Unix.file_descr -> memory -> int -> int -> int
  = "netsys_mem_write"

let mem_read fd mem off len =
  if len < 0 || off < 0 || len > Bigarray.Array1.dim mem - off then
    invalid_arg "Netsys_mem.mem_read";
  netsys_mem_read fd mem off len

let mem_write fd mem off len =
  if len < 0 || off < 0 || len > Bigarray.Array1.dim mem - off then
    invalid_arg "Netsys_mem.mem_write";
  netsys_mem_write fd mem off len

external netsys_mem_recv : 
  Unix.file_descr -> memory -> int -> int -> Unix.msg_flag list -> int
  = "netsys_mem_recv"

(*
external netsys_mem_recvfrom :
  Unix.file_descr -> memory -> int -> int -> Unix.msg_flag list ->
  int * Unix.sockaddr
  = "netsys_mem_recvfrom"
 *)

external netsys_mem_send :
  Unix.file_descr -> memory -> int -> int -> Unix.msg_flag list -> int
  = "netsys_mem_send"

(*
external netsys_mem_sendto :
  Unix.file_descr -> memory -> int -> int -> Unix.msg_flag list -> 
  Unix.sockaddr -> int
  = "netsys_mem_sendto" "netsys_mem_sendto_native"
 *)

let mem_recv fd mem off len flags =
  if len < 0 || off < 0 || len > Bigarray.Array1.dim mem - off then
    invalid_arg "Netsys_mem.mem_recv";
  netsys_mem_recv fd mem off len flags

(*
let mem_recvfrom fd mem off len flags =
  if len < 0 || off < 0 || len > Bigarray.Array1.dim mem - off then
    invalid_arg "Netsys_mem.mem_recvfrom";
  netsys_mem_recvfrom fd mem off len flags
 *)

let mem_send fd mem off len flags =
  if len < 0 || off < 0 || len > Bigarray.Array1.dim mem - off then
    invalid_arg "Netsys_mem.mem_send";
  netsys_mem_send fd mem off len flags

(*
let mem_sendto fd mem off len flags addr =
  if len < 0 || off < 0 || len > Bigarray.Array1.dim mem - off then
    invalid_arg "Netsys_mem.mem_sendto";
  netsys_mem_sendto fd mem off len flags addr
 *)

let min_pool_factor = 4
let max_pool_factor = 64


type bigblock =
    { bb_id : < >;
      mutable bb_use_counter : int;
      mutable bb_mem : memory;
      mutable bb_age : int;
    }

type memory_pool =
    { pool_block_size : int;
      mutable pool_blocks : (int * bigblock * bool ref) list;
      (* The bool is set to [false] if the block is unused *)
      mutable pool_free_blocks : (memory * int * bigblock) list;
      (* The int is the GC age *)
      pool_mutex : Netsys_oothr.mutex;
      mutable pool_factor : int;
      mutable pool_free_age : int;
    }

let create_pool bsize =
  let page_size = 
    try getpagesize() 
    with Invalid_argument _ -> 4096 
      (* alloc_memory_pages is then also not supported *) in
  if bsize <= 0 || bsize mod page_size <> 0 then
    invalid_arg "Netsys_mem.create_pool";
  let m = !Netsys_oothr.provider # create_mutex() in
  { pool_block_size = bsize;
    pool_blocks = [];
    pool_free_blocks = [];
    pool_mutex = m;
    pool_factor = min_pool_factor;
    pool_free_age = 0;
  }

let pool_move_to_free_list p =
  let age = (Gc.quick_stat()).Gc.major_collections in
  let ub, fb =
    List.partition
      (fun (_, _, is_used) -> !is_used)
      p.pool_blocks in
  p.pool_blocks <- ub;
  (* prerr_endline ("Found new free blocks: " ^ string_of_int (List.length fb));*)
  p.pool_free_blocks <- 
    (List.map 
       (fun (k,bb,_) -> 
	  let m =
	    Bigarray.Array1.sub 
	      bb.bb_mem (k * p.pool_block_size) p.pool_block_size in
	  (m, k, bb)
       )
       fb
    ) @ p.pool_free_blocks;
  List.iter
    (fun (_,bb,_) -> 
       bb.bb_use_counter <- bb.bb_use_counter - 1;
       bb.bb_age <- age
    ) 
    fb


(*
let bb_finalise _ =
  prerr_endline "bb_finalise"
 *)

let pool_alloc_blocks p =
  pool_move_to_free_list p;
  if p.pool_free_blocks = [] then (
    let age = (Gc.quick_stat()).Gc.major_collections in
    (* Nothing free, so we have to allocate new blocks: *)
    let bigblock_size = p.pool_factor * p.pool_block_size in
    let bigblock_mem =
      try alloc_memory_pages bigblock_size
      with Invalid_argument _ -> (* OS does not support it... *)
        Bigarray.Array1.create
          Bigarray.char Bigarray.c_layout bigblock_size in
    let bigblock =
      { bb_id = (object end);
	bb_use_counter = 0;
	bb_mem = bigblock_mem;
	bb_age = age;
      } in
    (* Gc.finalise bb_finalise bigblock_mem; *)
    for k = 0 to p.pool_factor - 1 do
      let m =
	Bigarray.Array1.sub 
	  bigblock_mem (k * p.pool_block_size) p.pool_block_size in
      p.pool_free_blocks <- (m, k, bigblock) :: p.pool_free_blocks
    done;
    (* prerr_endline ("alloc blocks: " ^ string_of_int p.pool_factor); *)
    p.pool_factor <- min max_pool_factor (p.pool_factor * 2);
  )
  else
    p.pool_factor <- min_pool_factor

let pool_free_blocks p =
  let age = (Gc.quick_stat()).Gc.major_collections in
  if age > p.pool_free_age then (
    pool_move_to_free_list p;
    let db, fb =
      List.partition
	(fun (_, _, bb) -> bb.bb_use_counter = 0 && age - bb.bb_age >= 2)
	p.pool_free_blocks in
    (* Sort the free blocks, to achieve that big, filled blocks are preferred
       when new blocks are taken from the free list. So small and quite empty
       blocks are more likely to be given back to the OS.
       - Hopefully this is not too expensive.
     *)
    let fb_sorted =
      List.sort
	(fun (_,_,bb1) (_,_,bb2) ->
	   (* highest use counter first, then oldest *)
	   match bb2.bb_use_counter - bb1.bb_use_counter with
	     | 0 -> Oo.id bb1.bb_id - Oo.id bb2.bb_id
	     | d -> d
	)
	fb in
    p.pool_free_blocks <- fb_sorted;
    p.pool_free_age <- age;
    (* prerr_endline ("pool_free_blocks db=" ^ string_of_int (List.length db))*)
      (* unmap_file is not supported for the "bigblock" approach CHECK *)
  )

let set_false v _ =
  (* prerr_endline "finaliser"; *)
  v := false

let pool_alloc_memory p =
  Netsys_oothr.serialize
    p.pool_mutex
    (fun () ->
       let do_free_check = ref true in
       if p.pool_free_blocks = [] then (
	 pool_alloc_blocks p;
	 do_free_check := false
       );
       match p.pool_free_blocks with
	 | (m,k,bb) :: l ->
	     p.pool_free_blocks <- l;
	     bb.bb_use_counter <- bb.bb_use_counter + 1;
	     let is_used = ref true in
	     let free = set_false is_used in  (* avoid referencing m ! *)
	     if !do_free_check && l <> [] then
	       pool_free_blocks p;
	     p.pool_blocks <- (k,bb,is_used) :: p.pool_blocks;
	     Gc.finalise free m;
	     m
	 | [] ->
	     assert false
    )
    ()

let pool_block_size p =
  p.pool_block_size

let default_pool_size =
  let page_size = 
    try getpagesize() 
    with Invalid_argument _ -> 4096 
      (* alloc_memory_pages is then also not supported *) in
  page_size * 16

let default_pool =
  create_pool default_pool_size

let pool_report p =
  let b = Buffer.create 500 in
  bprintf b "POOL GENERAL PARAMETERS:\n\n";
  bprintf b "pool_block_size=%d\n" p.pool_block_size;
  bprintf b "pool_factor=%d\n" p.pool_factor;
  bprintf b "length pool_blocks=%d  (used+prop)\n" (List.length p.pool_blocks);
  bprintf b "length pool_free_blocks=%d\n\n" (List.length p.pool_free_blocks);
  
  let bb_tab = Hashtbl.create 10 in

  let add_bb bb =
    if not (Hashtbl.mem bb_tab bb.bb_id) then (
      let cnt_used = ref 0 in
      let cnt_prop = ref 0 in
      Hashtbl.add bb_tab bb.bb_id (bb, cnt_used, cnt_prop)
    )
  in

  List.iter (fun (_,bb,_) -> add_bb bb) p.pool_blocks;
  List.iter (fun (_,_,bb) -> add_bb bb) p.pool_free_blocks;
  
  List.iter
    (fun (k,bb,is_used) ->
       let (_,cnt_used,cnt_prop) = Hashtbl.find bb_tab bb.bb_id in
       if !is_used then 
	 incr cnt_used
       else
	 incr cnt_prop
    )
    p.pool_blocks;
  
  bprintf b "POOL BY BIGBLOCK:\n\n";
  Hashtbl.iter
    (fun bb_id (bb,cnt_used,cnt_prop) ->
       let size = Bigarray.Array1.dim bb.bb_mem / p.pool_block_size in
       bprintf b "block %d: age=%d size=%d used=%d propagate=%d free=%d\n"
	 (Oo.id bb_id)
	 bb.bb_age
	 size
	 !cnt_used
	 !cnt_prop
	 (size - !cnt_used - !cnt_prop)
    )
    bb_tab;

  Buffer.contents b
