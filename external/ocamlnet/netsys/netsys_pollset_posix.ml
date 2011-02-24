(* $Id: netsys_pollset_posix.ml 1450 2010-05-06 19:13:30Z gerd $ *)

open Netsys_pollset

let fd_equal =
  match Sys.os_type with
    | "Win32" ->
	(fun fd1 fd2 -> fd1=fd2)
    | _ ->
	(fun (fd1:Unix.file_descr) fd2 ->
	   (Obj.magic fd1 : int) = (Obj.magic fd2 : int)
	)

let fd_hash =
  match Sys.os_type with
    | "Win32" ->
	(fun fd -> Hashtbl.hash fd)
    | _ ->
	(fun fd -> (Obj.magic fd : int))


module FdTbl =
  Hashtbl.Make
    (struct
       type t = Unix.file_descr
       let equal = fd_equal
       let hash = fd_hash
     end
    )


let oothr = !Netsys_oothr.provider

let while_locked mutex f =
  Netsys_oothr.serialize mutex f ()

let pipe_limit = 4
  (* We keep up to [pipe_limit] pairs of pipes for interrupting [poll].
     If more than this number of pairs become unused, they are closed.
   *)

let pipes = ref []
let pipes_m = oothr # create_mutex()
let pipe_pid = ref None
  (* When the process is forked, we give up our saved pipes, to avoid the
     confusion when several processes use the same descriptors
   *)

let reset_locked() =
  let l = !pipes in
  pipes := [];
  pipe_pid := None;
  List.iter
    (fun (p1,p1_sn,p2,p2_sn) -> 
       Netlog.Debug.release_fd ~sn:p1_sn p1;
       Netlog.Debug.release_fd ~sn:p2_sn p2;
       Unix.close p1; 
       Unix.close p2
    )
    l


let reset() =
  while_locked 
    pipes_m
    reset_locked


let() =
  Netsys_posix.register_post_fork_handler
    ( object
	method name = "Netsys_pollset_posix"
	method run() = reset()
      end
    )


let get_pipe_pair() =
  while_locked
    pipes_m
    (fun () ->
       let pid = Unix.getpid() in
       if !pipe_pid <> None && !pipe_pid <> Some pid then (
	 reset_locked();
       );
       pipe_pid := Some pid;
       let pp =
	 match !pipes with
	   | [] ->
	       let (p1,p2) = Unix.pipe() in
	       Netsys.set_close_on_exec p1;
	       Netsys.set_close_on_exec p2;
	       let p1_sn = Netlog.Debug.new_serial() in
	       let p2_sn = Netlog.Debug.new_serial() in
	       Netlog.Debug.track_fd
		 ~sn:p1_sn 
		 ~owner:"Netsys_pollset_posix" 
		 ~descr:"Event injection (rd)"
		 p1;
	       Netlog.Debug.track_fd
		 ~sn:p2_sn 
		 ~owner:"Netsys_pollset_posix" 
		 ~descr:"Event injection (wr)"
		 p2;
	       (p1,p1_sn,p2,p2_sn)
	   | (p1,p1_sn,p2,p2_sn) :: r ->
	       pipes := r;
	       (p1,p1_sn,p2,p2_sn) in
       pp
    )


let return_pipe_pair ((p1,p1_sn,p2,p2_sn) as pp) =
  while_locked 
    pipes_m
    (fun () ->
       if List.length !pipes >= pipe_limit then (
	 Netlog.Debug.release_fd ~sn:p1_sn p1;
	 Netlog.Debug.release_fd ~sn:p2_sn p2;
	 Unix.close p1;
	 Unix.close p2
       )
       else
	 pipes := pp :: !pipes
    )


let rounded_pa_size l =
  let n = ref 32 in
  while !n < l do
    n := 2 * !n
  done;
  !n



let poll_based_pollset () : pollset =
object(self)
  val mutable ht = FdTbl.create 10
    (* maps fd to req events *)

  val mutable spa = Netsys_posix.create_poll_array 32
    (* saved poll array - for the next time, so we don't have to allocate
       it again for every [wait]
     *)

  val mutable intr_fd = None
    (* The pipe that can be written to for interrupting waiting *)

  val mutable intr_flag = false
    (* Whether interruption happened *)

  val mutable cancel_flag = false
    (* The cancel flag and its mutex *)

  val mutable intr_m = oothr # create_mutex()
    (* Mutex protecting intr_fd, intr_flag, and cancel_flag *)

  val s1 = String.make 1 'X'


  method find fd =
    FdTbl.find ht fd

  method add fd ev =
    FdTbl.replace ht fd ev

  method remove fd =
    FdTbl.remove ht fd

  method wait tmo =
    if oothr # single_threaded then (
      if cancel_flag then
	[]
      else
	self # wait_1 tmo None
    )
    else (
      let (p_rd, p_rd_sn, p_wr, p_wr_sn) = get_pipe_pair() in
      let have_intr_lock = ref false in
      let r =
	try
	  let no_wait = (
	    intr_m # lock();
	    have_intr_lock := true;
	    if cancel_flag then (
	      intr_fd <- None;
	      have_intr_lock := false;
	      intr_m # unlock();
	      true
	    )
	    else (
	      intr_flag <- false;
	      intr_fd <- Some p_wr;
	      have_intr_lock := false;
	      intr_m # unlock();
	      false
	    )
	  ) in
	  if no_wait then
	    []
	  else (
	    let r = self # wait_1 tmo (Some p_rd) in
	    intr_m # lock();
	    have_intr_lock := true;
	    if intr_flag then (
	      try let _ = Netsys.restart(Unix.read p_rd s1 0) 1 in ()
	      with _ -> assert false
	    );
	    intr_fd <- None;
	    have_intr_lock := false;
	    intr_m # unlock();
	    r
	  )
	with
	  | err ->
	      if !have_intr_lock then intr_m # unlock();
	      return_pipe_pair (p_rd, p_rd_sn, p_wr, p_wr_sn);
	      raise err in
      return_pipe_pair (p_rd, p_rd_sn, p_wr, p_wr_sn);
      r
    )

  method private wait_1 tmo extra_fd_opt =
    let have_extra_fd = extra_fd_opt <> None in
    let ht_l = FdTbl.length ht in
    let l = ht_l + if have_extra_fd then 1 else 0 in
    let pa = 
      if l < Netsys_posix.poll_array_length spa then
	spa
      else (
	let pa = Netsys_posix.create_poll_array(rounded_pa_size l) in
	spa <- pa;
	pa
      ) in
    let j = ref 0 in
    FdTbl.iter
      (fun fd ev ->
	 let c = 
	   { Netsys_posix.poll_fd = fd;
	     poll_req_events = ev;
	     poll_act_events = Netsys_posix.poll_null_events()
	   } in
	 Netsys_posix.set_poll_cell pa !j c;
	 incr j
      )
      ht;
    ( match extra_fd_opt with
	| None -> ()
	| Some fd ->
	    let c = 
	      { Netsys_posix.poll_fd = fd;
		poll_req_events = 
		  Netsys_posix.poll_req_events true false false;
		poll_act_events = Netsys_posix.poll_null_events()
	      } in
	    Netsys_posix.set_poll_cell pa !j c;
    );
    let n = ref(Netsys_posix.poll pa l tmo) in
    let r = ref [] in
    let k = ref 0 in
    while !n > 0 && !k < ht_l do
      let c = Netsys_posix.get_poll_cell pa !k in
      if Netsys_posix.poll_result c.Netsys_posix.poll_act_events then (
	r := (c.Netsys_posix.poll_fd,
	      c.Netsys_posix.poll_req_events, 
	      c.Netsys_posix.poll_act_events) :: !r;
	decr n
      );
      incr k
    done;
    !r
    

  method dispose() = ()


  method cancel_wait b =
    while_locked
      intr_m
      (fun () ->
	 cancel_flag <- b;
	 if b && not intr_flag then (
	   match intr_fd with
	     | None -> ()
	     | Some fd ->
		 let _n = 
		   try Netsys.restart(Unix.single_write fd s1 0) 1
		   with _ -> assert false
		 in
		 intr_flag <- true
	 )
      )
end
