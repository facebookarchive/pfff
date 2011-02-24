(* $Id: netsys_impl_util.ml 1201 2008-08-31 23:41:22Z gerd $ *)

(* this & that *)


let rec restart f arg =
  try 
    f arg
  with
    | Unix.Unix_error(Unix.EINTR,_,_) ->
	restart f arg



let restart_tmo f tmo =
  let t0 = Unix.gettimeofday() in
  let rec tryagain t_elapsed =
    let tmo' = tmo -. t_elapsed in
    try
      f (max tmo' 0.0)
    with
      | Unix.Unix_error(Unix.EINTR,_,_) ->
	  let t1 = Unix.gettimeofday() in
	  tryagain (t1 -. t0)
  in
  if tmo > 0.0 then
    tryagain 0.0
  else
    restart f tmo



let max_int_32 = 
  1073741823


let slice_time_ms f tmo =
  (* We assume f is called with a timeout argument [f tmo0] where the argument
     is given in milliseconds up to max_int_32. [tmo] is the requested timeout
     as float in seconds. The task is to call [f] several times with an int
     argument until [f] returns a non-None result, or until [tmo] has elapsed.
     Negative values are allowed and interpreted as infinite timeout.
   *)
  let tmo_ms = tmo *. 1000.0 in
  let max_int64_as_float = Int64.to_float Int64.max_int in
  let max_int_as_int64 = Int64.of_int max_int_32 in
  if tmo < 0.0 || tmo_ms >= max_int64_as_float then
    f (-1)
  else (
    let remaining_tmo_ms = ref (Int64.of_float tmo_ms) in
    let result = ref None in
    while !result = None && !remaining_tmo_ms > 0L do
      if !remaining_tmo_ms > max_int_as_int64 then (
	result := f max_int_32;
	remaining_tmo_ms := Int64.sub !remaining_tmo_ms max_int_as_int64
      )
      else (
	result := f (Int64.to_int !remaining_tmo_ms);
	remaining_tmo_ms := 0L;
      )
    done;
    !result
  )


let mem_sorted_array x a =
  let rec search l h =
    if l < h then (
      let m = (l+h) / 2 in
      let r = Pervasives.compare x a.(m) in
      if r = 0 then
	true
      else
	if r < 0 then
	  search l m
	else
	  search (m+1) h
    )
    else false
  in
  search 0 (Array.length a)
