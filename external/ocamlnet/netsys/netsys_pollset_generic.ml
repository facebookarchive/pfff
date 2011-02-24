(* $Id: netsys_pollset_generic.ml 1219 2009-04-14 13:28:56Z ChriS $ *)

open Netsys_pollset

let standard_pollset() =
  match Sys.os_type with
    | "Win32" ->
	if ( !Netsys_oothr.provider ) # single_threaded then
	  Netsys_pollset_win32.pollset()
	else
	  Netsys_pollset_win32.threaded_pollset()
    | _ ->
	Netsys_pollset_posix.poll_based_pollset()


let performance_pollset = standard_pollset


let select_emulation (pset : Netsys_pollset.pollset) =
  let tab = Hashtbl.create 20 in

  let enter fd new_req1 =
    let old_req, new_req2 =
      try Hashtbl.find tab fd
      with Not_found -> (0,0) in
    Hashtbl.replace tab fd (old_req, new_req1 lor new_req2) in

  let shift() =
    let l = ref [] in
    Hashtbl.iter
      (fun fd pair -> l := (fd, pair) :: !l)
      tab;
    List.iter
      (fun (fd, (old_req, new_req)) ->
	 Hashtbl.replace tab fd (new_req, 0)
      )
      !l in

  fun rdlist wrlist prilist tmo ->
    shift();
    List.iter (fun fd -> enter fd Netsys_posix.const_rd_event) rdlist;
    List.iter (fun fd -> enter fd Netsys_posix.const_wr_event) wrlist;
    List.iter (fun fd -> enter fd Netsys_posix.const_pri_event) prilist;

    let to_del = ref [] in
    Hashtbl.iter
      (fun fd (old_req, new_req) ->
	 if new_req = 0 then (
	   pset # remove fd;
	   to_del := fd :: !to_del
	 )
	 else
	   pset # add fd (Netsys_posix.req_events_of_int new_req)
      )
      tab;
    List.iter (fun fd -> Hashtbl.remove tab fd) !to_del;

    let l = pset # wait tmo in
    
    let rdout = ref [] in
    let wrout = ref [] in
    let priout = ref [] in
    List.iter
      (fun (fd, req, ev) ->
	 let req = Netsys_posix.int_of_req_events req in
	 let p_rd = (req land Netsys_posix.const_rd_event) <> 0 in
	 let p_wr = (req land Netsys_posix.const_wr_event) <> 0 in
	 let p_pri = (req land Netsys_posix.const_pri_event) <> 0 in
	 let err = Netsys_posix.poll_err_result ev in
	 let hup = Netsys_posix.poll_hup_result ev in
	 let nval = Netsys_posix.poll_nval_result ev in
	 if nval then
	   raise(Unix.Unix_error(Unix.EBADF, "select_emulation", ""));
	 if p_rd && (err || hup || Netsys_posix.poll_rd_result ev) then
	   rdout := fd :: !rdout;
	 if p_wr && (err || Netsys_posix.poll_wr_result ev) then
	   wrout := fd :: !wrout;
	 if p_pri && (err || Netsys_posix.poll_pri_result ev) then
	   priout := fd :: !priout;
      )
      l;

    (!rdout, !wrout, !priout)

    
		 
