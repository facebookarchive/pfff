(* ********************************************************************** *)
(* Reading Fixed-Length Records *)
(* ********************************************************************** *)
let pleac_Reading_Fixed_Length_Records () = 
  (* Using the Bitstring library by Richard W.M. Jones.
     http://code.google.com/p/bitstring/ *)
  let () =
    try
      while true do
        let bitstring = Bitstring.bitstring_of_chan_max file recordsize in
        let fields = unpack bitstring in
        (* ... *)
        ()
      done
    with Match_failure _ -> ()
  
  (*-----------------------------*)
  
  (* Layout based on /usr/include/bits/utmp.h for a Linux system. *)
  let recordsize = 384
  let unpack bits =
    bitmatch bits with
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
          (ut_type, ut_pid, ut_line, ut_id, ut_user, ut_host,
           ut_exit, ut_session, ut_tv_sec, ut_tv_usec, ut_addr_v6)
  

