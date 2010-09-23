(* ********************************************************************** *)
(* Program: tailwtmp *)
(* ********************************************************************** *)
let pleac_Program__tailwtmp () = 
  (*pp camlp4o -I /path/to/bitstring bitstring.cma pa_bitstring.cmo *)
  
  (* tailwtmp - watch for logins and logouts; *)
  (* uses linux utmp structure, from utmp(5)  *)
  
  let days = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |]
  let months = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun";
                  "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]
  
  let string_of_tm tm =
    Printf.sprintf "%s %s %2d %02d:%02d:%02d %04d"
      days.(tm.Unix.tm_wday)
      months.(tm.Unix.tm_mon)
      tm.Unix.tm_mday
      tm.Unix.tm_hour
      tm.Unix.tm_min
      tm.Unix.tm_sec
      (tm.Unix.tm_year + 1900)
  
  let trim_asciiz s =
    try String.sub s 0 (String.index s '\000')
    with Not_found -> s
  
  let () =
    let sizeof = 384 in
    let wtmp = open_in "/var/log/wtmp" in
    seek_in wtmp (in_channel_length wtmp);
    while true do
      let buffer = Bitstring.bitstring_of_chan_max wtmp sizeof in
      (bitmatch buffer with
         | { ut_type : 16 : littleendian;
             _ : 16; (* padding *)
             ut_pid : 32 : littleendian;
             ut_line : 256 : string;
             ut_id : 32 : littleendian;
             ut_user : 256 : string;
             ut_host : 2048 : string;
             ut_exit : 32 : littleendian;
             ut_session : 32 : littleendian;
             ut_tv_sec : 32 : littleendian;
             ut_tv_usec : 32 : littleendian;
             ut_addr_v6 : 128 : string } ->
             Printf.printf "%1d %-8s %-12s %10ld %-24s %-16s %5ld %-32s\n%!"
               ut_type (trim_asciiz ut_user) (trim_asciiz ut_line) ut_id
               (string_of_tm (Unix.localtime (Int32.to_float ut_tv_sec)))
               (trim_asciiz ut_host) ut_pid (Digest.to_hex ut_addr_v6)
         | { _ } -> ());
      if pos_in wtmp = in_channel_length wtmp
      then Unix.sleep 1
    done
  

