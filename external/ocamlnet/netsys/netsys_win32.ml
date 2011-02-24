(* $Id: netsys_win32.ml 1505 2010-12-15 19:15:22Z gerd $ *)

(* Security of named pipes:

   - http://www.blakewatts.com/namedpipepaper.html
   - impersonation: http://msdn.microsoft.com/en-us/library/aa378832(VS.85).aspx
 *)

open Printf

external fill_random : string -> unit = "netsys_fill_random"


type c_event
type c_pipe_helper

(* How the proxy table works:

   The i_* records are the values in the proxy table. The keys are the
   proxy descriptors (which must be open all the time). When the last
   reference to the proxy descriptor is released, the GC will call the
   finalizer, and (with some trickery) the entry is removed from the
   proxy table. Of course, the i_* records must not contain the proxy
   descriptor - otherwise there would be a self-reference in the table,
   and the entry is never released.

   Because of this we define w_* types as a pair of the i_* records and
   the proxy descriptors. The w_* types are the public types. As no
   i_* can escape from its w_* value outside this module, it is ensured
   that all public references of i_* also imply public references of the
   proxy descriptor. So as long as there are w_* values the i_* values
   cannot be collected.

   When the proxy descriptor is accessed from outside the module, the
   caller becomes responsible for closing it. Therefore we track whether
   this is the case. The proxy descriptor is also stored in the c_* values 
   (i.e. in the values handled by the C part of this module), and so
   the C-written finalizer can close the proxy descriptor if required.
   There is a flag whether to do so (auto_close_*_proxy), and this flag
   is cleared when the caller takes over the ownership of the descriptor.
 *)

type i_event = c_event
type w32_event = i_event * Unix.file_descr
    (* The descriptor is the proxy descriptor *)

type pipe_mode = Pipe_in | Pipe_out | Pipe_duplex

type i_pipe =
    { pipe_name : string;
      pipe_mode : pipe_mode;
      pipe_helper : c_pipe_helper;
      (* mutable pipe_signal : w32_event option; *)
      pipe_rd_event : w32_event;
      pipe_wr_event : w32_event;
    }

type w32_pipe = i_pipe * Unix.file_descr
    (* The descriptor is the proxy descriptor *)

type i_pipe_server =
    { psrv_name : string;
      psrv_mode : pipe_mode;
      psrv_max : int;
      mutable psrv_first : bool;
      mutable psrv_queue : c_pipe_helper list;
      (* The queue of pipes waiting for an incoming connection *)
      mutable psrv_listen : int;
      (* The backlog parameter of [listen] (target length of psrv_queue) *)
      psrv_ready : c_pipe_helper Queue.t;
      (* The already accepted but not yet reported connections *)
      psrv_cn_event : w32_event;
      psrv_proxy_handle : c_event;
      psrv_mutex : Netsys_oothr.mutex;
    }
  (* As there is no C counterpart for the pipe server (no c_pipe_server),
     the question is how to ensure that the proxy descriptor is closed.
     For that reason we allocate an event (psrv_proxy_handle) and use
     this event as proxy descriptor. For events there is the possibility
     to let the C part of the module close the descriptor.
   *)

type w32_pipe_server = i_pipe_server * Unix.file_descr
    (* The descriptor is the proxy descriptor *)


type pipe_conn_state = Pipe_deaf | Pipe_listening | Pipe_connected | Pipe_down

type c_process
type i_process = c_process

type w32_process = i_process * Unix.file_descr
    (* The descriptor is the proxy descriptor *)

type i_input_thread =
  { ithr_descr : Unix.file_descr;
    (* One can send command to the thread by setting ithr_cmd, and signaling
       ithr_cmd_cond:
     *)
    ithr_cmd_cond : Netsys_oothr.condition;
    ithr_cmd_mutex : Netsys_oothr.mutex;
    mutable ithr_cmd : [ `Read | `Cancel ] option;
    mutable ithr_cancel_cmd : bool;   (* similar to ithr_cmd = Some `Cancel *)
    ithr_event : w32_event; (* The event is set when there is something to read *)
    ithr_buffer : string;
    mutable ithr_buffer_start : int;
    mutable ithr_buffer_len : int;
    mutable ithr_buffer_cond : [ `Cancelled | `EOF | `Exception of exn | `Data ];
    mutable ithr_thread : int32;   (* The Win32 thread ID *)
    ithr_read_mutex : Netsys_oothr.mutex;  (* to serialize user read accesses *)
    mutable ithr_running : bool;
    ithr_proxy_handle : c_event;  (* the proxy - same pattern as in pipe servers *)
  }

type w32_input_thread = i_input_thread * Unix.file_descr * < >
    (* The descriptor is the proxy descriptor *)


type i_output_thread =
  { othr_descr : Unix.file_descr;
    othr_cmd_cond : Netsys_oothr.condition;
    othr_cmd_mutex : Netsys_oothr.mutex;
    mutable othr_cmd : [ `Write | `Close | `Cancel ] option;
    mutable othr_cancel_cmd : bool; 
    othr_event : w32_event;
    othr_buffer : string;
    mutable othr_buffer_len : int;
    mutable othr_write_cond : [ `Cancelled | `Exception of exn ] option;
    mutable othr_thread : int32;   (* The Win32 thread ID *)
    othr_write_mutex : Netsys_oothr.mutex;
    mutable othr_running : bool;
    othr_proxy_handle : c_event;
  }

type w32_output_thread = i_output_thread * Unix.file_descr * < >
    (* The descriptor is the proxy descriptor *)


type i_object =
  | I_event of i_event
  | I_pipe of i_pipe
  | I_pipe_server of i_pipe_server
  | I_process of i_process
  | I_input_thread of i_input_thread * < >
  | I_output_thread of i_output_thread * < >

type w32_object =
  | W32_event of w32_event
  | W32_pipe of w32_pipe
  | W32_pipe_server of w32_pipe_server
  | W32_process of w32_process
  | W32_input_thread of w32_input_thread
  | W32_output_thread of w32_output_thread
      

type create_process_option =
  | CP_change_directory of string
  | CP_set_env of string
  | CP_std_handles of Unix.file_descr * Unix.file_descr * Unix.file_descr
  | CP_create_console
  | CP_detach_from_console
  | CP_inherit_console
  | CP_inherit_or_create_console
  | CP_unicode_environment
  | CP_ansi_environment
  | CP_new_process_group
  | CP_inherit_process_group



module Int64Map = Map.Make(Int64)


external int64_of_file_descr : Unix.file_descr -> int64
  = "netsys_int64_of_file_descr"
  (* Also occurs in netsys.ml! *)

external netsys_win32_set_debug : bool -> unit
  = "netsys_win32_set_debug"

module Debug = struct
  let enable = ref false

  let debug_c_wrapper = netsys_win32_set_debug
end

let dlog = Netlog.Debug.mk_dlog "Netsys_win32" Debug.enable
let dlogr = Netlog.Debug.mk_dlogr "Netsys_win32" Debug.enable

let () =
  Netlog.Debug.register_module "Netsys_win32" Debug.enable


module FD = struct
  type t = int64
  let equal (fd1) (fd2) =
    fd1=fd2
  let hash fd =
    Hashtbl.hash fd
end

module H = Hashtbl.Make(FD)
  (* Hash table mapping
     proxy file descriptors to the w32_object referenced by the descriptors.
     The keys are the handle values contained in the fd values. As we allow
     that proxies are [Unix.close]d it can happen that several fd's exist
     that have the same handle values. In this case, the address of the
     fd itself is used to distinguish between these same-looking fd's.
   *)

let proxies = ref (H.create 41)
let proxies_mutex = !Netsys_oothr.provider # create_mutex()
let proxies_gc_flag = ref false
let proxies_unreg_count = ref 0


let mk_weak x =
  let w = Weak.create 1 in
  Weak.set w 0 (Some x);
  w

let get_weak w =
  Weak.get w 0


let finalise_proxy cell _ =
  (* the GC finaliser *)
  proxies_unreg_count := !proxies_unreg_count + 1;
  cell := None


let gc_proxy() =
  (* Walk through the table and check. We have to take care that the
     order of the bindings for the same key is preserved, i.e. the most
     recent use of a descriptor number needs to be the visible binding
     in the table.
   *)
  let proxies' = H.create 41 in
  let n_old = ref 0 in
  let n_new = ref 0 in
  H.iter
    (fun fd_num entries ->
       let m = ref [] in
       List.iter
	 (fun entry ->
	    incr n_old;
	    let (_, cell) = entry in
	    if !cell <> None then (
	      incr n_new;
	      m := entry :: !m
	    )
	 )
	 entries;
       H.add proxies' fd_num (List.rev !m)
    )
    !proxies;
  proxies := proxies';
  proxies_unreg_count := 0;
  proxies_gc_flag := false;
  dlogr
    (fun () ->
       sprintf "register_proxy: keeping %d/%d entries in proxy tbl"
	 !n_new !n_old;
    );
  dlogr
    (fun () ->
       let b = Buffer.create 500 in
       Buffer.add_string b "\n";
       H.iter
	 (fun fd_num entries ->
	    List.iter
	      (fun (_,cell) ->
		 bprintf b " - proxy tbl %Ld -> %s\n"
		   fd_num
		   ( match !cell with
		       | None -> "<free>"
		       | Some(I_event _) -> "I_event"
		       | Some(I_pipe _) -> "I_pipe"
		       | Some(I_pipe_server _) -> "I_pipe_server"
		       | Some(I_process _) -> "I_process"
		       | Some(I_input_thread _) -> "I_input_thread"
		  | Some(I_output_thread _) -> "I_output_thread"
		   )
	      )
	      entries
	 )
	 proxies';
       Buffer.contents b
    )
	 

let register_proxy fd i_obj =
  let fd_num = int64_of_file_descr fd in
  (* Note that it is possible that we register several i_obj for the same
     fd_num. This can happen when fd is first closed, and then collected
     by the GC. So after the close the OS can reuse the fd_num for something
     else, but the old fd_num is still in the table.
   *)
  Netsys_oothr.serialize
    proxies_mutex
    (fun () ->
       if (!proxies_gc_flag && 
	     2 * !proxies_unreg_count > H.length !proxies)
       then (  (* do a GC pass *)
	 gc_proxy()
       );
       let cell = ref (Some i_obj) in
       let fd_weak = mk_weak fd in
       let l = try H.find !proxies fd_num with Not_found -> [] in
       H.replace !proxies fd_num ((fd_weak, cell) :: l);
       Gc.finalise (finalise_proxy cell) fd
    )
    ()


let unregister fd =
  (* called from user code *)
  let fd_num = int64_of_file_descr fd in
  Netsys_oothr.serialize
    proxies_mutex
    (fun () ->
       let l = try H.find !proxies fd_num with Not_found -> [] in
       let l' = 
	 List.filter
	   (fun (fd'_weak,cell) ->
	      match get_weak fd'_weak with
		| None -> false
		| Some fd' ->
		    !cell <> None && fd != fd'  (* phys. cmp! *)
	   )
	   l in
       H.replace !proxies fd_num l'
    )
    ()

let _ =
  Gc.create_alarm
    (fun () ->
       proxies_gc_flag := true
    )


let lookup fd =
  let fd_num = int64_of_file_descr fd in
  Netsys_oothr.serialize
    proxies_mutex
    (fun () ->
       let l = H.find !proxies fd_num in
       let (_, cell_opt) =
	 List.find
	   (fun (fd'_weak,cell) ->
	      match get_weak fd'_weak with
		| None -> false
		| Some fd' -> 
		    !cell <> None && fd == fd'  (* phys. cmp! *)
	   )
	   l in
       match !cell_opt with
	 | None ->
	     assert false
	 | Some i_obj ->
	     ( match i_obj with
		 | I_event i_ev -> 
		     W32_event(i_ev, fd)
		 | I_pipe i_pipe -> 
		     W32_pipe(i_pipe, fd)
		 | I_pipe_server i_psrv ->
		     W32_pipe_server(i_psrv, fd)
		 | I_process i_proc ->
		     W32_process(i_proc, fd)
		 | I_input_thread(i_thr, keep_alive) ->
		     W32_input_thread(i_thr, fd, keep_alive)
		 | I_output_thread(o_thr, keep_alive) ->
		     W32_output_thread(o_thr, fd, keep_alive)
	     )
    )
    ()


let lookup_pipe fd =
  try
    match lookup fd with
      | W32_pipe ph -> ph
      | _ -> raise Not_found
  with Not_found ->
    failwith "Netsys_win32.lookup_pipe: not found"

let lookup_pipe_server fd =
  try
    match lookup fd with
      | W32_pipe_server ph -> ph
      | _ -> raise Not_found
  with Not_found ->
    failwith "Netsys_win32.lookup_pipe_server: not found"

let lookup_event fd =
  try
    match lookup fd with
      | W32_event e -> e
      | _ -> raise Not_found
  with Not_found ->
    failwith "Netsys_win32.lookup_event: not found"

let lookup_process fd =
  try
    match lookup fd with
      | W32_process e -> e
      | _ -> raise Not_found
  with Not_found ->
    failwith "Netsys_win32.lookup_process: not found"

let lookup_input_thread fd =
  try
    match lookup fd with
      | W32_input_thread e -> e
      | _ -> raise Not_found
  with Not_found ->
    failwith "Netsys_win32.lookup_input_thread: not found"

let lookup_output_thread fd =
  try
    match lookup fd with
      | W32_output_thread e -> e
      | _ -> raise Not_found
  with Not_found ->
    failwith "Netsys_win32.lookup_output_thread: not found"


external get_active_code_page : unit -> int 
  = "netsys_getacp"

external netsys_real_select : 
         Unix.file_descr list -> 
         Unix.file_descr list -> 
         Unix.file_descr list -> 
         float ->
           (Unix.file_descr list * Unix.file_descr list * Unix.file_descr list)
  = "netsys_real_select"

let real_select = netsys_real_select


external netsys_test_close_on_exec : Unix.file_descr -> bool
  = "netsys_test_close_on_exec"

let test_close_on_exec = netsys_test_close_on_exec

external netsys_modify_close_on_exec : Unix.file_descr -> bool -> unit
  = "netsys_modify_close_on_exec"

let modify_close_on_exec = netsys_modify_close_on_exec

external netsys_is_crt_fd : Unix.file_descr -> int -> bool
  = "netsys_is_crt_fd"

let is_crt_fd = netsys_is_crt_fd



external netsys_create_event : unit -> c_event 
  = "netsys_create_event"

external netsys_event_descr : c_event -> Unix.file_descr
  = "netsys_event_descr"

external netsys_close_event : c_event -> unit
  = "netsys_close_event"

external netsys_set_auto_close_event_proxy : c_event -> bool -> unit
  = "netsys_set_auto_close_event_proxy"

let decorate_event e =
  let e_proxy = netsys_event_descr e in
  let ev = (e, e_proxy) in
  Gc.finalise netsys_close_event e;
  register_proxy e_proxy (I_event e);
  ev

let create_event() =
  let ev = decorate_event(netsys_create_event()) in
  dlogr (fun () -> 
	   sprintf "create_event: descr=%Ld" 
	     (int64_of_file_descr (snd ev)));
  ev


let event_descr (e,e_proxy) = 
  netsys_set_auto_close_event_proxy e false;
  e_proxy

external netsys_set_event : c_event -> unit
  = "netsys_set_event"

external netsys_reset_event : c_event -> unit
  = "netsys_reset_event"

external netsys_test_event : c_event -> bool
  = "netsys_test_event"

external netsys_event_wait : c_event -> int -> bool
  = "netsys_event_wait"

let set_event (e,e_proxy)   = 
  dlogr (fun () -> 
	   sprintf "set_event: descr=%Ld" 
	     (int64_of_file_descr e_proxy));
  netsys_set_event e

let reset_event (e,e_proxy) = 
  dlogr (fun () -> 
	   sprintf "reset_event: descr=%Ld" 
	     (int64_of_file_descr e_proxy));
  netsys_reset_event e

let test_event (e,_)  = netsys_test_event e

let event_wait (e,e_proxy) tmo =
  dlogr (fun () -> 
	   sprintf "event_wait: descr=%Ld tmo=%f" 
	     (int64_of_file_descr e_proxy) tmo);
  let flag =
    Netsys_impl_util.slice_time_ms
      (fun tmo_ms ->
	 if netsys_event_wait e tmo_ms then Some () else None
      )
      tmo
    <> None in
  dlogr (fun () -> 
	   sprintf "event_wait: descr=%Ld returning %b" 
	     (int64_of_file_descr e_proxy) flag);
  flag



external netsys_wsa_event_select :  
  c_event -> Unix.file_descr -> Netsys_posix.poll_req_events -> unit
  = "netsys_wsa_event_select"

external wsa_maximum_wait_events : unit -> int
  = "netsys_wsa_maximum_wait_events"

external netsys_wsa_wait_for_multiple_events : 
  c_event array -> int -> int option
  = "netsys_wsa_wait_for_multiple_events"

external netsys_wsa_enum_network_events : 
  Unix.file_descr -> c_event -> Netsys_posix.poll_act_events
  = "netsys_wsa_enum_network_events"

let wsa_event_select (e,e_proxy) fd pie =
  dlogr (fun () -> 
	   sprintf "wsa_event_select: evdescr=%Ld sockdescr=%Ld bits=%d" 
	     (int64_of_file_descr e_proxy) 
	     (int64_of_file_descr fd)
	     (Netsys_posix.int_of_req_events pie)
	);
  netsys_wsa_event_select e fd pie

let wsa_wait_for_multiple_events ea n =
  dlogr (fun () ->
	   sprintf "wsa_wait_for_multiple_events: descrs=%s tmo=%d"
	     (String.concat ","
		(Array.to_list
		   (Array.map
		      (fun (_,e_proxy) -> 
			 Int64.to_string(int64_of_file_descr e_proxy)) 
		      ea)))
	     n
	);
  let r =
    netsys_wsa_wait_for_multiple_events (Array.map fst ea) n in
  dlogr (fun () ->
	   sprintf "wsa_wait_for_multiple_events: returning %s"
	     (match r with
		| None -> "None"
		| Some k ->
		    let e_proxy = snd(ea.(k)) in
		    sprintf "Some %d (descr %Ld)"
		      k (int64_of_file_descr e_proxy)
	     ));
  r

let wsa_enum_network_events fd (e,e_proxy) =
  let r = netsys_wsa_enum_network_events fd e in
  dlogr (fun () ->
	   sprintf "wsa_enum_network_events: sockdescr=%Ld evdescr=%Ld bits=%d"
	     (int64_of_file_descr fd)
	     (int64_of_file_descr e_proxy)
	     (Netsys_posix.int_of_act_events r)
	);
  r



external netsys_pipe_free : 
  c_pipe_helper -> unit
  = "netsys_pipe_free"

external netsys_create_local_named_pipe :
  string -> pipe_mode -> int -> c_event -> bool -> c_pipe_helper
  = "netsys_create_local_named_pipe"

external netsys_pipe_listen :
  c_pipe_helper -> unit
  = "netsys_pipe_listen"

external netsys_pipe_deafen :
  c_pipe_helper -> unit
  = "netsys_pipe_deafen"

external netsys_pipe_connect :
  string -> pipe_mode -> c_pipe_helper
  = "netsys_pipe_connect"

external netsys_pipe_read :
  c_pipe_helper -> string -> int -> int -> int
  = "netsys_pipe_read"

external netsys_pipe_write :
  c_pipe_helper -> string -> int -> int -> int
  = "netsys_pipe_write"

external netsys_pipe_shutdown :
  c_pipe_helper -> unit
  = "netsys_pipe_shutdown"

external netsys_pipe_rd_event :
  c_pipe_helper -> c_event
  = "netsys_pipe_rd_event"

external netsys_pipe_wr_event :
  c_pipe_helper -> c_event
  = "netsys_pipe_wr_event"

external netsys_pipe_descr :
   c_pipe_helper -> Unix.file_descr
  = "netsys_pipe_descr"

external netsys_pipe_conn_state : 
  c_pipe_helper -> pipe_conn_state
  = "netsys_pipe_conn_state"

external netsys_pipe_signal :
  c_pipe_helper -> c_event -> unit
  = "netsys_pipe_signal"

external netsys_set_auto_close_pipe_proxy : c_pipe_helper -> bool -> unit
  = "netsys_set_auto_close_pipe_proxy"


let rev_mode =
  function
    | Pipe_in -> Pipe_out
    | Pipe_out -> Pipe_in
    | Pipe_duplex -> Pipe_duplex

let create_local_pipe_server name mode n =
  let cn_event = create_event() in
  let p_event = netsys_create_event() in
  let proxy = netsys_event_descr p_event in
  let psrv =
    { psrv_name = name;
      psrv_mode = mode;
      psrv_max = n;
      psrv_first = true;
      psrv_queue = [];
      psrv_listen = 0;
      psrv_ready = Queue.create();
      psrv_cn_event = cn_event;
      psrv_proxy_handle = p_event;
      psrv_mutex = !Netsys_oothr.provider # create_mutex();
    } in
  Gc.finalise netsys_close_event p_event;
  register_proxy proxy (I_pipe_server psrv);
  dlogr (fun () ->
	   sprintf "create_local_pipe_server: \
                    name=%s proxydescr=%Ld cnevdescr=%Ld"
	     name 
	     (int64_of_file_descr proxy) 
	     (int64_of_file_descr (snd cn_event))
	);
  (psrv, proxy)



let decorate_pipe_nogc ph name mode =
  let fd = netsys_pipe_descr ph in
  let pipe =
    { pipe_name = name;
      pipe_mode = mode;
      pipe_helper = ph;
      (* pipe_signal = None; *)
      pipe_rd_event = decorate_event(netsys_pipe_rd_event ph);
      pipe_wr_event = decorate_event(netsys_pipe_wr_event ph);
    } in
  register_proxy fd (I_pipe pipe);
  (pipe, fd)


let decorate_pipe ph name mode =
  Gc.finalise netsys_pipe_free ph;
  decorate_pipe_nogc ph name mode

let prefix = "\\\\.\\pipe\\"
let prefix_len = String.length prefix

let pipe_connect name mode =
  (* Check that name starts with the right prefix, to prevent security
     vulnerabilities:
   *)
  if String.length name < prefix_len ||
     (String.sub name 0 prefix_len <> prefix)
  then
    raise(Unix.Unix_error(Unix.EPERM, "pipe_connect", name));

  dlogr (fun () -> sprintf "pipe_connect: name=%s" name);
  let pipe = decorate_pipe(netsys_pipe_connect name mode) name mode in
  dlogr (fun () -> sprintf "pipe_connect: name=%s returning %Ld" 
	   name (int64_of_file_descr (snd pipe)));
  pipe

let pipe_server_descr (psrv, psrv_proxy) = 
  netsys_set_auto_close_event_proxy psrv.psrv_proxy_handle false;
  psrv_proxy

let pipe_descr (pipe, pipe_proxy) = 
  netsys_set_auto_close_pipe_proxy pipe.pipe_helper false;
  pipe_proxy

let pipe_server_endpoint psrv =
  let ph = 
    netsys_create_local_named_pipe
      psrv.psrv_name psrv.psrv_mode psrv.psrv_max 
      (fst psrv.psrv_cn_event) psrv.psrv_first in
  Gc.finalise netsys_pipe_free ph;
  netsys_pipe_listen ph;
  psrv.psrv_first <- false;
  ph

let pipe_listen_lck psrv n =
  if psrv.psrv_listen < n then (
    let d = n - psrv.psrv_listen in
    for k = 1 to d do
      let ph = pipe_server_endpoint psrv in
      psrv.psrv_queue <- ph :: psrv.psrv_queue
    done
  );
  (* else: we do nothing. You may consider this as a bug, but it is simply
     too risky to shut down server pipes because of race conditions
   *)
  psrv.psrv_listen <- n


let pipe_listen (psrv, psrv_proxy) n =
  dlogr (fun () -> sprintf "pipe_listen: name=%s proxydescr=%Ld n=%d" 
	   psrv.psrv_name (int64_of_file_descr psrv_proxy) n);
  Netsys_oothr.serialize
    psrv.psrv_mutex
    (fun () -> pipe_listen_lck psrv n)
    ()


let check_for_connections psrv =
  let rec find_delete l =
    match l with
      | [] -> 
	  []
      | ph :: l' ->
	  let s = netsys_pipe_conn_state ph in
	  if s = Pipe_connected then (
	    Queue.push ph psrv.psrv_ready;
	    find_delete l'
	  )
	  else
	    ph :: find_delete l'
  in

  let queue' = find_delete psrv.psrv_queue in
  let old_listen = psrv.psrv_listen in
  psrv.psrv_listen <- List.length queue';
  psrv.psrv_queue <- queue';
  pipe_listen_lck psrv old_listen
    
(* In rare cases it may happen that cn_event is reset for a short
   period of time, and then set again.
 *)

let rec pipe_accept_1 psrv =
  match Queue.length psrv.psrv_ready with
    | 0 ->
	ignore(event_wait psrv.psrv_cn_event (-1.0));
	reset_event psrv.psrv_cn_event;
	check_for_connections psrv;
	if not(Queue.is_empty psrv.psrv_ready) then
	  set_event psrv.psrv_cn_event;
	pipe_accept_1 psrv
    | 1 ->
	let ph = Queue.take psrv.psrv_ready in
	reset_event psrv.psrv_cn_event;
	check_for_connections psrv;
	if not(Queue.is_empty psrv.psrv_ready) then
	  set_event psrv.psrv_cn_event;
	ignore(netsys_pipe_read ph "" 0 0);     (* check for errors *)
	decorate_pipe_nogc ph psrv.psrv_name psrv.psrv_mode
    | _ ->
	let ph = Queue.take psrv.psrv_ready in
	ignore(netsys_pipe_read ph "" 0 0);     (* check for errors *)
	decorate_pipe_nogc ph psrv.psrv_name psrv.psrv_mode


let pipe_accept (psrv, psrv_proxy) =
  dlogr (fun () -> sprintf "pipe_accept: name=%s proxydescr=%Ld" 
	   psrv.psrv_name (int64_of_file_descr psrv_proxy));
  let pipe =
    Netsys_oothr.serialize
      psrv.psrv_mutex
      (fun () -> pipe_accept_1 psrv)
      () in
  dlogr (fun () -> sprintf "pipe_accept: name=%s proxydescr=%Ld returning %Ld" 
	   psrv.psrv_name (int64_of_file_descr psrv_proxy)
	   (int64_of_file_descr (snd pipe))
	);
  pipe
  

let pipe_rd_event (pipe,_) =
  pipe.pipe_rd_event

let pipe_wr_event (pipe,_) =
  pipe.pipe_wr_event

let pipe_connect_event (psrv,_) =
  psrv.psrv_cn_event


let pipe_read (pipe,pipe_proxy) s pos len =
  if pos < 0 || len < 0 || pos > String.length s - len then
    invalid_arg "Netsys_win32.pipe_read";
  dlogr (fun () -> sprintf "pipe_read: name=%s proxydescr=%Ld len=%d" 
	   pipe.pipe_name (int64_of_file_descr pipe_proxy) len);
  try
    let n = netsys_pipe_read pipe.pipe_helper s pos len in
    dlogr (fun () -> sprintf "pipe_read: name=%s proxydescr=%Ld returning %d" 
	     pipe.pipe_name (int64_of_file_descr pipe_proxy) n);
    n
  with
    | error when !Debug.enable ->
	dlogr (fun () -> 
		 sprintf "pipe_read: name=%s proxydescr=%Ld exception %s" 
		   pipe.pipe_name (int64_of_file_descr pipe_proxy) 
		   (Netexn.to_string error)
	      );
	raise error


let pipe_write (pipe,pipe_proxy) s pos len =
  if pos < 0 || len < 0 || pos > String.length s - len then
    invalid_arg "Netsys_win32.pipe_write";
  dlogr (fun () -> sprintf "pipe_write: name=%s proxydescr=%Ld len=%d" 
	   pipe.pipe_name (int64_of_file_descr pipe_proxy) len);
  try
    let n = netsys_pipe_write pipe.pipe_helper s pos len in
    dlogr (fun () -> sprintf "pipe_write: name=%s proxydescr=%Ld returning %d" 
	     pipe.pipe_name (int64_of_file_descr pipe_proxy) n);
    n
  with
    | error when !Debug.enable ->
	dlogr (fun () -> 
		 sprintf "pipe_write: name=%s proxydescr=%Ld exception %s" 
		   pipe.pipe_name (int64_of_file_descr pipe_proxy) 
		   (Netexn.to_string error)
	      );
	raise error


let pipe_shutdown (pipe,pipe_proxy) = 
  dlogr (fun () -> sprintf "pipe_shutdown: name=%s proxydescr=%Ld" 
	   pipe.pipe_name (int64_of_file_descr pipe_proxy));
  netsys_pipe_shutdown pipe.pipe_helper

let pipe_shutdown_server (psrv,psrv_proxy) =
  dlogr (fun () -> sprintf "pipe_shutdown_server: name=%s proxydescr=%Ld" 
	   psrv.psrv_name (int64_of_file_descr psrv_proxy));
  Netsys_oothr.serialize
    psrv.psrv_mutex
    (fun () ->
       List.iter
	 (fun ph ->
	    netsys_pipe_shutdown ph
	 )
	 psrv.psrv_queue;
       psrv.psrv_queue <- [];
       psrv.psrv_listen <- 0
    )
    ()


let pipe_wait_rd (pipe,pipe_proxy) tmo =
  dlogr (fun () -> sprintf "pipe_wait_rd: name=%s proxydescr=%Ld" 
	   pipe.pipe_name (int64_of_file_descr pipe_proxy));
  event_wait pipe.pipe_rd_event tmo

let pipe_wait_wr (pipe,pipe_proxy) tmo =
  dlogr (fun () -> sprintf "pipe_wait_wr: name=%s proxydescr=%Ld" 
	   pipe.pipe_name (int64_of_file_descr pipe_proxy));
  event_wait pipe.pipe_wr_event tmo

let pipe_wait_connect (psrv,psrv_proxy) tmo =
  dlogr (fun () -> sprintf "pipe_wait_connect: name=%s proxydescr=%Ld" 
	   psrv.psrv_name (int64_of_file_descr psrv_proxy));
  event_wait psrv.psrv_cn_event tmo

let pipe_name (pipe,_) =
  pipe.pipe_name

let pipe_server_name (psrv,_) =
  psrv.psrv_name

let pipe_mode (pipe,_) =
  pipe.pipe_mode

let pipe_server_mode (psrv,_) =
  psrv.psrv_mode


let counter = ref 0
let counter_mutex = !Netsys_oothr.provider # create_mutex()

let unpredictable_pipe_name() =
  let n = (
    counter_mutex # lock();
    let n = !counter in
    incr counter;
    counter_mutex # unlock();
    n
  ) in
  let random = String.make 16 ' ' in
  fill_random random;
  let name =
    "\\\\.\\pipe\\ocamlnet" ^ 
      string_of_int (Unix.getpid()) ^ "_" ^ string_of_int n ^ "_" ^ 
      Digest.to_hex random in
  name

let pipe_pair mode =
  (* FIXME: If somebody guesses the pipe name (which is hard),
     it is possible to connect from the outside to lph. We detect
     this problem, and give up on the pipe pair, but external code can 
     make our programs unreliable.
   *)
  dlog "pipe_pair";
  let mode' =
    match mode with
      | Pipe_in -> Pipe_out
      | Pipe_out -> Pipe_in
      | Pipe_duplex -> Pipe_duplex in
  let name = unpredictable_pipe_name() in
  let psrv = create_local_pipe_server name mode 1 in
  pipe_listen psrv 1;
  let rph = pipe_connect name mode' in
  ( try
      pipe_listen psrv 0;
      let lph = pipe_accept psrv in
      ( try
	  let s = String.create 0 in
	  ignore(pipe_write lph s 0 0);
	  dlogr 
	    (fun () -> 
	       sprintf "pipe_pair: returning \
                        name=%s proxydescr1=%Ld proxydescr2=%Ld" 
		 name
		 (int64_of_file_descr (snd lph)) 
		 (int64_of_file_descr (snd rph)) 
	    );
	  (lph, rph)
	with e -> 
	  pipe_shutdown lph; 
	  raise e
      )
    with e -> 
      pipe_shutdown rph; 
      raise e
  )


external netsys_create_process : string -> string -> 
  create_process_option list -> c_process 
  = "netsys_create_process"

external netsys_close_process : c_process -> unit
  = "netsys_close_process"

external netsys_get_process_status : c_process -> int
  = "netsys_get_process_status"

external netsys_as_process_event : c_process -> c_event
  = "netsys_as_process_event"

external netsys_emulated_pid : c_process -> int
  = "netsys_emulated_pid"

external netsys_win_pid : c_process -> int
  = "netsys_win_pid"

external netsys_process_free : c_process -> unit
  = "netsys_process_free"

external netsys_process_descr : c_process -> Unix.file_descr
  = "netsys_process_descr"

external netsys_set_auto_close_process_proxy : c_process -> bool -> unit
  = "netsys_set_auto_close_process_proxy"

external netsys_terminate_process : c_process -> unit
  = "netsys_terminate_process"

let close_process (c_proc, _) =
  netsys_process_free c_proc

let get_process_status (c_proc,_) =
  try
    let code = netsys_get_process_status c_proc in
    Some(Unix.WEXITED code)
  with
    | Not_found -> None

let default_opts =
  [ CP_inherit_or_create_console;
    CP_ansi_environment;
    CP_inherit_process_group
  ]

let create_process cmd cmdline opts =
  let opts = (* prepend defaults: *)
    default_opts @ opts in
  let c_proc = netsys_create_process cmd cmdline opts in
  let proxy = netsys_process_descr c_proc in
  register_proxy proxy (I_process c_proc);
  Gc.finalise netsys_process_free c_proc;
  ignore(get_process_status (c_proc,proxy));
  (* The new process seems to remain suspended until the caller waits
     for the process handle. So we do this here.
   *)
  (c_proc, proxy)

let terminate_process (c_proc,_) =
  netsys_terminate_process c_proc

let as_process_event (c_proc,_) =
  let ev = netsys_as_process_event c_proc in
  decorate_event ev

let emulated_pid (c_proc,_) =
  netsys_emulated_pid c_proc

let win_pid (c_proc, _) =
  netsys_win_pid c_proc

let process_descr (c_proc, fd) =
  netsys_set_auto_close_process_proxy c_proc false;
  fd

let cp_set_env env =
  CP_set_env(String.concat "\000" (Array.to_list env) ^ "\000")
    (* another null byte is implicitly added by the ocaml runtime! *)

external search_path : string option -> string -> string option -> string
  = "netsys_search_path"


type w32_console_attr =
    { mutable cursor_x : int;
      mutable cursor_y : int;
      mutable cursor_size : int;
      mutable cursor_visible : bool;
      mutable text_attr : int;
    }

type w32_console_info =
    {
      mutable width : int;
      mutable height : int;
    }

type w32_console_mode = 
    { mutable enable_echo_input : bool;
      mutable enable_insert_mode : bool;
      mutable enable_line_input : bool;
      mutable enable_processed_input : bool;
      mutable enable_quick_edit_mode : bool;
      mutable enable_processed_output : bool;
      mutable enable_wrap_at_eol_output : bool;
    }

external has_console : unit -> bool
  = "netsys_has_console"

external is_console : Unix.file_descr -> bool
  = "netsys_is_console"

external alloc_console : unit -> unit
  = "netsys_alloc_console"

let get_console_input() =
  if not(has_console()) then
    alloc_console();
  Unix.openfile "CONIN$" [Unix.O_RDWR] 0
    (* O_RDONLY is insufficient for certain console ops *)


let get_console_output() =
  if not(has_console()) then
    alloc_console();
  Unix.openfile "CONOUT$" [Unix.O_RDWR] 0
    (* O_WRONLY is insufficient for certain console ops *)

external get_console_attr : unit -> w32_console_attr
  = "netsys_get_console_attr"

external set_console_attr : w32_console_attr -> unit
  = "netsys_set_console_attr"

external get_console_info : unit -> w32_console_info
  = "netsys_get_console_info"

let fg_blue = 1
let fg_green = 2
let fg_red = 4
let fg_intensity = 8
let bg_blue = 16
let bg_green = 32
let bg_red = 64
let bg_intensity = 128

external get_console_mode : unit -> w32_console_mode
  = "netsys_get_console_mode"

external set_console_mode : w32_console_mode -> unit
  = "netsys_set_console_mode"

external init_console_codepage : unit -> unit
  = "netsys_init_console_codepage"

type clear_mode =
  | EOL | EOS | All

external clear_console : clear_mode -> unit
  = "netsys_clear_console"

let clear_until_end_of_line() = clear_console EOL

let clear_until_end_of_screen() = clear_console EOS

let clear_console() = clear_console All


external get_current_thread_id : unit -> int32
  = "netsys_get_current_thread_id"

external cancel_synchronous_io : int32 -> unit
  = "netsys_cancel_synchronous_io"
  (* Only implemented on Vista (and newer). *)



module InputThread = struct
  let rec thread_body (ithr : i_input_thread) =
    (* Check for new commands: *)
    dlogr (fun () ->
	     sprintf "input_thread_body: descr=%Ld waiting"
	       (int64_of_file_descr ithr.ithr_descr));
    ithr.ithr_cmd_mutex # lock();
    while ithr.ithr_cmd = None && not ithr.ithr_cancel_cmd do
      ithr.ithr_cmd_cond # wait ithr.ithr_cmd_mutex
    done;
    let next_cmd =
      if ithr.ithr_cancel_cmd then
	`Cancel
      else
	match ithr.ithr_cmd with
          | None -> 
              assert false
          | Some c -> 
              ithr.ithr_cmd <- None;
              c in
    let continue =
      match next_cmd with
	| `Cancel ->
	    dlogr (fun () ->
		     sprintf "input_thread_body: descr=%Ld got `Cancel"
		       (int64_of_file_descr ithr.ithr_descr));
	    ithr.ithr_buffer_cond <- `Cancelled;
	    false
	| `Read ->
	    dlogr (fun () ->
		     sprintf "input_thread_body: descr=%Ld got `Read"
		       (int64_of_file_descr ithr.ithr_descr));
	    ( try
		let n = 
		  Unix.read 
		    ithr.ithr_descr
		    ithr.ithr_buffer
		    0
		    (String.length ithr.ithr_buffer) in
		if n = 0 then (
		  ithr.ithr_buffer_cond <- `EOF;
		  ithr.ithr_buffer_start <- 0;
		  ithr.ithr_buffer_len <- 0;
		  false
		) 
		else (
		  ithr.ithr_buffer_cond <- `Data;
		  ithr.ithr_buffer_start <- 0;
		  ithr.ithr_buffer_len <- n;
		  true
		)
	      with
		| Unix.Unix_error(Unix.EPIPE,_,_) ->  (* same as EOF *)
		    ithr.ithr_buffer_cond <- `EOF;
		    ithr.ithr_buffer_start <- 0;
		    ithr.ithr_buffer_len <- 0;
		    false
		| error ->
		    ithr.ithr_buffer_cond <- `Exception error;
		    ithr.ithr_buffer_start <- 0;
		    ithr.ithr_buffer_len <- 0;
		    false
	    )
    in
    dlogr (fun () ->
	     sprintf "input_thread_body: descr=%Ld unblocking"
	       (int64_of_file_descr ithr.ithr_descr));
    set_event ithr.ithr_event;
    ithr.ithr_cmd_mutex # unlock();
    if continue then 
      thread_body ithr
    else (
      (* clean-up: *)
      dlogr (fun () ->
	       sprintf "input_thread_body: descr=%Ld terminating"
		 (int64_of_file_descr ithr.ithr_descr));
      Unix.close ithr.ithr_descr;
      ithr.ithr_running <- false
    )
      

  let i_cancel_input_thread ithr =
    dlogr (fun () ->
	     sprintf "cancel_input_thread: descr=%Ld"
	       (int64_of_file_descr ithr.ithr_descr));
    ithr.ithr_cancel_cmd <- true;  (* don't mess with locks here *)
    ithr.ithr_cmd_cond # signal();
    (* This is clearly a race condition... The thread may terminate
       right now, and cancel_io_thread is called with an invalid thread
       ID.
     *)
    if ithr.ithr_running then (
      try
	cancel_synchronous_io ithr.ithr_thread
      with _ -> ()
    )

  let f_cancel_input_thread ithr _ =
    i_cancel_input_thread ithr

  let cancel_input_thread (ithr,_,_) =
    i_cancel_input_thread ithr 

  let create_input_thread fd =
    let oothr = !Netsys_oothr.provider in
    let init_cond = oothr#create_condition() in
    let init_mutex = oothr#create_mutex() in
    let p_event = netsys_create_event() in
    let proxy = netsys_event_descr p_event in
    let ithr =
      { ithr_descr = fd;
	ithr_cmd_cond = oothr#create_condition();
	ithr_cmd_mutex = oothr#create_mutex();
	ithr_cmd = Some `Read;
	ithr_cancel_cmd = false;
	ithr_event = create_event();
	ithr_buffer = String.create 4096;
	ithr_buffer_start = 0;
	ithr_buffer_len = 0;
	ithr_buffer_cond = `Data;
	ithr_thread = 0l;  (* initialized below *)
	ithr_read_mutex = oothr#create_mutex();
	ithr_running = true;
	ithr_proxy_handle = p_event;
      } in
    let _ =
      oothr # create_thread
	(fun () ->
	   ithr.ithr_thread <- get_current_thread_id();
	   init_cond # signal();
	   thread_body ithr
	)
	() in
    init_cond # wait init_mutex;
    let f = f_cancel_input_thread ithr in
    let keep_alive = (object end) in
    Gc.finalise f keep_alive;
    Gc.finalise netsys_close_event p_event;
    register_proxy proxy (I_input_thread(ithr, keep_alive));
    (ithr, proxy, keep_alive)
    

  let input_thread_read (ithr,_,_) s pos len =
    if pos < 0 || len < 0 || pos > String.length s - len then
      invalid_arg "Netsys_win32.input_thread_read";
    
    if len = 0 then
      0
    else (
      Netsys_oothr.serialize
	ithr.ithr_read_mutex   (* only one reader at a time *)
	(fun () ->
	   let b = test_event ithr.ithr_event in
	   if b then (
	     ithr.ithr_cmd_mutex # lock();
	   (* Look at what we have: *)
	   match ithr.ithr_buffer_cond with
	     | `EOF ->
		 ithr.ithr_cmd_mutex # unlock();
		 0
	     | `Exception e ->
		 ithr.ithr_cmd_mutex # unlock();
		 raise e
	     | `Cancelled ->
		 ithr.ithr_cmd_mutex # unlock();
		 raise(Unix.Unix_error(Unix.EPERM, 
				       "Netsys_win32.input_thread_read", ""))
	     | `Data ->
		 let n = min len ithr.ithr_buffer_len in
		 String.blit 
		   ithr.ithr_buffer ithr.ithr_buffer_start
		   s pos
		   n;
		 ithr.ithr_buffer_start <- ithr.ithr_buffer_start + n;
		 ithr.ithr_buffer_len <- ithr.ithr_buffer_len - n;
		 if ithr.ithr_buffer_len = 0 then (
		   ithr.ithr_cmd <- Some `Read;
		   ithr.ithr_cmd_cond # signal();
		   reset_event ithr.ithr_event;
		 );
		 ithr.ithr_cmd_mutex # unlock();
		 n
	   )
	   else
	     raise(Unix.Unix_error(Unix.EAGAIN, 
				   "Netsys_win32.input_thread_read", ""))
	)
	()
    )

  let input_thread_event (ithr,_,_) =
    ithr.ithr_event

  let input_thread_proxy_descr (ithr, proxy, _) = 
    netsys_set_auto_close_event_proxy ithr.ithr_proxy_handle false;
    proxy

end

let create_input_thread = InputThread.create_input_thread
let input_thread_event = InputThread.input_thread_event
let input_thread_read = InputThread.input_thread_read
let cancel_input_thread = InputThread.cancel_input_thread
let input_thread_proxy_descr = InputThread.input_thread_proxy_descr
let input_thread_descr (ithr,_,_) = ithr.ithr_descr


module OutputThread = struct
  let rec write_string othr pos len =
    if len = 0 || othr.othr_cancel_cmd then
      ()
    else
      let n = Unix.single_write othr.othr_descr othr.othr_buffer pos len in
      write_string othr (pos+n) (len-n)


  let rec thread_body (othr : i_output_thread) =
    (* Check for new commands: *)
    dlogr (fun () ->
	     sprintf "output_thread_body: descr=%Ld waiting"
	       (int64_of_file_descr othr.othr_descr));
    othr.othr_cmd_mutex # lock();
    while othr.othr_cmd = None && not othr.othr_cancel_cmd do
      othr.othr_cmd_cond # wait othr.othr_cmd_mutex
    done;
    let next_cmd =
      if othr.othr_cancel_cmd then
	`Cancel
      else
	match othr.othr_cmd with
          | None -> 
              assert false
          | Some c -> 
              othr.othr_cmd <- None;
              c in
    let continue =
      match next_cmd with
	| `Cancel ->
	    dlogr (fun () ->
		     sprintf "output_thread_body: descr=%Ld got `Cancel"
		       (int64_of_file_descr othr.othr_descr));
	    othr.othr_buffer_len <- 0;
	    othr.othr_write_cond <- Some `Cancelled;
	    false
	| `Close ->
	    dlogr (fun () ->
		     sprintf "output_thread_body: descr=%Ld got `Close"
		       (int64_of_file_descr othr.othr_descr));
	    othr.othr_write_cond <- Some `Cancelled;
	    false
	| `Write ->
	    dlogr (fun () ->
		     sprintf "output_thread_body: descr=%Ld got `Write"
		       (int64_of_file_descr othr.othr_descr));
	    ( try
		write_string othr 0 othr.othr_buffer_len;
		othr.othr_buffer_len <- 0;
		true
	      with
		| error ->
		    othr.othr_write_cond <- Some (`Exception error);
		    false
	    )
    in
    dlogr (fun () ->
	     sprintf "output_thread_body: descr=%Ld unblocking"
	       (int64_of_file_descr othr.othr_descr));
    set_event othr.othr_event;
    othr.othr_cmd_mutex # unlock();
    if continue then 
      thread_body othr
    else (
      (* clean-up: *)
      dlogr (fun () ->
	       sprintf "output_thread_body: descr=%Ld terminating"
		 (int64_of_file_descr othr.othr_descr));
      Unix.close othr.othr_descr;
      othr.othr_running <- false
    )


  let i_cancel_output_thread othr =
    dlogr (fun () ->
	     sprintf "cancel_output_thread: descr=%Ld"
	       (int64_of_file_descr othr.othr_descr));
    othr.othr_cancel_cmd <- true;  (* don't mess with locks here *)
    othr.othr_cmd_cond # signal();
    (* This is clearly a race condition... The thread may terminate
       right now, and cancel_io_thread is called with an invalid thread
       ID.
     *)
    if othr.othr_running then (
      try
	cancel_synchronous_io othr.othr_thread
      with _ -> ()
    )

  let f_cancel_output_thread othr _ =
    i_cancel_output_thread othr

  let cancel_output_thread (othr,_,_) =
    i_cancel_output_thread othr 

  let create_output_thread fd =
    let oothr = !Netsys_oothr.provider in
    let init_cond = oothr#create_condition() in
    let init_mutex = oothr#create_mutex() in
    let p_event = netsys_create_event() in
    let proxy = netsys_event_descr p_event in
    let othr =
      { othr_descr = fd;
	othr_cmd_cond = oothr#create_condition();
	othr_cmd_mutex = oothr#create_mutex();
	othr_cmd = None;
	othr_cancel_cmd = false;
	othr_event = create_event();
	othr_buffer = String.create 4096;
	othr_buffer_len = 0;
	othr_write_cond = None;
	othr_thread = 0l;  (* initialized below *)
	othr_write_mutex = oothr#create_mutex();
	othr_running = true;
	othr_proxy_handle = p_event;
      } in
    set_event othr.othr_event;
    let _ =
      oothr # create_thread
	(fun () ->
	   othr.othr_thread <- get_current_thread_id();
	   init_cond # signal();
	   thread_body othr
	)
	() in
    init_cond # wait init_mutex;
    let f = f_cancel_output_thread othr in
    let keep_alive = (object end) in
    Gc.finalise f keep_alive;
    Gc.finalise netsys_close_event p_event;
    register_proxy proxy (I_output_thread(othr, keep_alive));
    (othr, proxy, keep_alive)
    

  let output_thread_write (othr,_,_) s pos len =
    if pos < 0 || len < 0 || pos > String.length s - len then
      invalid_arg "Netsys_win32.output_thread_write";
    
    if len = 0 then
      0
    else (
      Netsys_oothr.serialize
	othr.othr_write_mutex   (* only one writer at a time *)
	(fun () ->
	   let b = test_event othr.othr_event in
	   if b then (
	     othr.othr_cmd_mutex # lock();
	     (* Look at what we have: *)
	     match othr.othr_write_cond with
	       | Some(`Exception e) ->
		   othr.othr_cmd_mutex # unlock();
		   raise e
	       | Some `Cancelled ->
		   othr.othr_cmd_mutex # unlock();
		   raise(Unix.Unix_error(Unix.EPERM, 
					 "Netsys_win32.output_thread_write",
					 ""))
	       | None ->
		   assert(othr.othr_buffer_len = 0);
		   let n = min len (String.length othr.othr_buffer) in
		   String.blit 
		     s pos
		     othr.othr_buffer 0
		     n;
		   othr.othr_buffer_len <- n;
		   othr.othr_cmd <- Some `Write;
		   othr.othr_cmd_cond # signal();
		   reset_event othr.othr_event;
		   othr.othr_cmd_mutex # unlock();
		   n
	   )
	   else
	     raise(Unix.Unix_error(Unix.EAGAIN, 
				   "Netsys_win32.output_thread_write", ""))
	)
	()
    )

  let close_output_thread (othr,_,_) =
    Netsys_oothr.serialize
      othr.othr_write_mutex   (* only one writer at a time *)
      (fun () ->
	 let b = test_event othr.othr_event in
	 if b then (
	   othr.othr_cmd_mutex # lock();
	   (* Look at what we have: *)
	   match othr.othr_write_cond with
	     | Some(`Exception e) ->
		 othr.othr_cmd_mutex # unlock();
		 raise e
	     | Some `Cancelled ->
		 othr.othr_cmd_mutex # unlock();
		 raise(Unix.Unix_error(Unix.EPERM, 
				       "Netsys_win32.close_output_thread",
				       ""))
	     | None ->
		 assert(othr.othr_buffer_len = 0);
		 othr.othr_cmd <- Some `Close;
		 othr.othr_cmd_cond # signal();
		 reset_event othr.othr_event;
		 othr.othr_cmd_mutex # unlock();
	 )
	 else
	   raise(Unix.Unix_error(Unix.EAGAIN, 
				 "Netsys_win32.close_output_thread", ""))
      )
      ()



  let output_thread_event (othr,_,_) =
    othr.othr_event

  let output_thread_proxy_descr (othr, proxy, _) = 
    netsys_set_auto_close_event_proxy othr.othr_proxy_handle false;
    proxy

end

let create_output_thread = OutputThread.create_output_thread
let output_thread_event = OutputThread.output_thread_event
let output_thread_write = OutputThread.output_thread_write
let cancel_output_thread = OutputThread.cancel_output_thread
let close_output_thread = OutputThread.close_output_thread
let output_thread_proxy_descr = OutputThread.output_thread_proxy_descr
let output_thread_descr (othr,_,_) = othr.othr_descr
