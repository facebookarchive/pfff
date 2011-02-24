(* $Id: netaux.ml 1003 2006-09-24 15:17:15Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

module KMP = struct

  type pattern = { len : int; 
		   p : string; 
		   fail : int array;
		   rex : Pcre.regexp;
		 }

  let rec delta pat state c =
    if pat.p.[state] = c then state + 1
    else if state = 0 then 0
    else delta pat pat.fail.(state - 1) c

  let make_pattern p =
    let l = String.length p in
    if l = 0 then invalid_arg "Netaux.KMP.make_pattern";
    let rex = Pcre.regexp (Pcre.quote (String.make 1 p.[0])) in
    let pat = { len = l; p = p; fail = Array.make l 0; rex = rex } in
    for n = 0 to l - 2 do
      pat.fail.(n + 1) <- delta pat pat.fail.(n) p.[n]
    done;
    pat


  let run rex len p fail s endpos state pos =
    let rec run_loop state pos =
      if (state = len) || (pos = endpos) then (state,pos)
      else 
	if p.[state] = s.[pos] then
	  run_loop (state+1) (pos+1)
	else
	  if state = 0 then
	    (* run_loop 0 (pos+1) *)
	    run_pcre (pos+1)
	  else
	    let state' = fail.(state-1) in
	    run_delta p.[state'] state' pos
	      
    and run_delta c state pos =
      if c = s.[pos] then 
	run_loop (state+1) (pos+1)
      else 
	if state = 0 then 
	  run_loop 0 (pos+1)
	else 
	  let state' = fail.(state-1) in
	  run_delta p.[state'] state' pos

    and run_pcre pos =
      (* Does the same as [run_loop 0 pos], but uses PCRE to skip all the
       * non-matching characters. Improves the speed of bytecode dramatically,
       * but does not cost very much for native code.
       *)
      let pos' =
	try
	  (* Note: setting s.[endpos] <- p.[0] would be a working guard,
	   * but this might lead to problems in multi-threaded programs.
	   * So we don't do it here. Better fix Pcre someday...
	   *)
	  let a = Pcre.pcre_exec ~rex ~pos s in  (* FIXME: no ~len *)
	  a.(0)
	with
	    Not_found -> endpos
      in
      if pos' < endpos then
	run_loop 0 pos'
      else
	run_loop 0 endpos
    in
    run_loop state pos

  let find_pattern pat ?(pos=0) ?len s =
    let endpos = 
      match len with
	  None -> String.length s
	| Some l -> pos+l in
    if pos < 0 || endpos > String.length s || pos > endpos then
      invalid_arg "Netaux.KMP.find_pattern";
    let (state,pos) = run pat.rex pat.len pat.p pat.fail s endpos 0 pos in
    pos - state
end


module ArrayAux = struct
  let int_blit_ref =
    ref 
      (fun (src:int array) srcpos dest destpos len ->
	 (* A specialised version of Array.blit for int arrays.
	  * Faster than the polymorphic Array.blit for
	  * various reasons.
	  *)
	 if (len < 0 || srcpos < 0 || 
	     srcpos+len > Array.length src ||
	     destpos < 0 ||
	     destpos+len > Array.length dest) then
	   invalid_arg "Netaux.ArrayAux.int_blit";
	 if src != dest || destpos <= srcpos then (
	   for i = 0 to len-1 do
	     Array.unsafe_set 
	       dest 
	       (destpos+i) 
	       (Array.unsafe_get src (srcpos+i))
	   done
	 ) else (
	   for i = len-1 downto 0 do
	     Array.unsafe_set 
	       dest 
	       (destpos+i) 
	       (Array.unsafe_get src (srcpos+i))
	   done
	 )
      )

  let int_blit src srcpos dest destpos len = 
    !int_blit_ref src srcpos dest destpos len

  let int_series_ref =
    ref
      (fun src srcpos dst dstpos len n ->
	 if (len < 0 || srcpos < 0 || dstpos < 0 ||
	     srcpos+len > Array.length src ||
	     dstpos+len > Array.length dst)
	 then
	   invalid_arg "Netaux.ArrayAux.int_series";

	 let s = ref n in
	 for i = 0 to len-1 do
	   Array.unsafe_set dst (dstpos+i) !s;
	   s := !s + Array.unsafe_get src (srcpos+i)
	 done
      )

  let int_series src srcpos dst dstpos len n =
    !int_series_ref src srcpos dst dstpos len n

end
