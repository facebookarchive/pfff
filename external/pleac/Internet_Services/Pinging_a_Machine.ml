(* ********************************************************************** *)
(* Pinging a Machine *)
(* ********************************************************************** *)
let pleac_Pinging_a_Machine () = 
  #!/usr/bin/ocaml
  (* ping - send and receive ICMP echo packets *)
  
  (* There do not appear to be any libraries available for pinging
     servers from OCaml, ICMP or otherwise. In this recipe, we will
     make a diversion from the Perl recipe, which simply determines
     if a host is up, and instead write a lookalike for the "ping"
     shell command. We might as well, if we're going to all the
     trouble of building ICMP packets directly. *)
  
  (* Import Unix and enable threads using findlib for convenience. *)
  #use "topfind";;
  #require "unix";;
  #thread;;
  
  (* The Packet module defines a data type and operations for building,
     parsing, and checking the integrity of ICMP packets. *)
  module Packet = struct
    exception Invalid_length of int
    exception Invalid_checksum of int * int
  
    (* type' and code define the ICMP message type. An echo message
       has type'=8, code=0, and an echo reply has type'=0, code=0.
       The id is a unique identifier for the current process to help
       distinguish between replies for other processes. seq is the
       sequence number, which is usually incremented with each message.
       data is the message body whose contents depend on the type of
       message. *)
    type t = { type' : int;
               code : int;
               id : int;
               seq : int;
               data : string }
  
    (* Define a convenience function for constructing packets. *)
    let make ?(type'=8) ?(code=0) ~id ~seq data =
      {type'=type'; code=code; id=id; seq=seq; data=data}
  
    (* Calculate a checksum for a message by adding its contents, two
       bytes at a time, folding the high order bits into the low order
       bits, and taking the logical complement. The result will be an
       int with 16-bit precision. *)
    let checksum s =
      let num_bytes = String.length s in
      let num_shorts = num_bytes / 2 in
      let rec sum_shorts i sum =
        if i < num_shorts then
          let short = Int32.of_int (int_of_char s.[i * 2] lsl 8
                                    + int_of_char s.[i * 2 + 1]) in
          sum_shorts (i + 1) (Int32.add sum short)
        else sum in
      let sum = sum_shorts 0 0l in
      let sum =
        if num_bytes mod 2 = 1 then
          Int32.add sum
            (Int32.of_int (int_of_char s.[num_bytes - 1] lsl 8))
        else sum in
      let sum =
        Int32.add
          (Int32.shift_right sum 16)
          (Int32.logand sum 0xffffl) in
      Int32.to_int
        (Int32.logand
           (Int32.lognot (Int32.add (Int32.shift_right sum 16) sum))
           0xffffl)
  
    (* Convert a packet to a string that can be sent over a socket. *)
    let to_string {type'=type'; code=code; id=id; seq=seq; data=data} =
      let b = Buffer.create 20 in
      Buffer.add_char b (char_of_int type');
      Buffer.add_char b (char_of_int code);
      Buffer.add_char b '\000';  (* checksum hi *)
      Buffer.add_char b '\000';  (* checksum lo *)
      Buffer.add_char b (char_of_int (id lsr 8 land 0xff));
      Buffer.add_char b (char_of_int (id land 0xff));
      Buffer.add_char b (char_of_int (seq lsr 8 land 0xff));
      Buffer.add_char b (char_of_int (seq land 0xff));
      Buffer.add_string b data;
      let packet = Buffer.contents b in
      let sum = checksum packet in
      packet.[2] <- char_of_int (sum lsr 8 land 0xff);
      packet.[3] <- char_of_int (sum land 0xff);
      packet
  
    (* Parse a string into a packet structure. If the string is less than
       8 bytes long, an Invalid_length exception will be raised. If the
       checksum does not match the contents, an Invalid_checksum
       exception will be raised. *)
    let of_string s =
      if String.length s < 8 then raise (Invalid_length (String.length s));
      let s' = String.copy s in
      s'.[2] <- '\000';
      s'.[3] <- '\000';
      let sum = int_of_char s.[2] lsl 8 + int_of_char s.[3] in
      let sum' = checksum s' in
      if sum <> sum' then raise (Invalid_checksum (sum, sum'));
      {type'=int_of_char s.[0];
       code=int_of_char s.[1];
       id=int_of_char s.[4] lsl 8 + int_of_char s.[5];
       seq=int_of_char s.[6] lsl 8 + int_of_char s.[7];
       data=String.sub s 8 (String.length s - 8)}
  end
  
  (* Define a data structure for the message body of our echo requests. *)
  type payload = { timestamp : float; data : string }
  
  (* Send a single ICMP echo request to the given socket and address. *)
  let ping socket sockaddr id seq =
    let payload =
      Marshal.to_string {timestamp=Unix.gettimeofday ();
                         data="abcdefghijklmnopqrstuvwxyz0123456"} [] in
    let message = Packet.to_string (Packet.make ~id ~seq payload) in
    ignore
      (Unix.sendto socket message 0 (String.length message) [] sockaddr)
  
  (* Loop forever waiting for echo replies, printing them to the
     console along with their hostname, IP, and round-trip time. *)
  let pong socket id =
    let buffer = String.make 256 '\000' in
    while true do
      let length, sockaddr =
        Unix.recvfrom socket buffer 0 (String.length buffer) [] in
      let response =
        Packet.of_string (String.sub buffer 20 (length - 20)) in
      match sockaddr, response with
        | Unix.ADDR_INET (addr, port),
          {Packet.type'=0; code=0; id=id'; seq=seq; data=data}
            when id = id' ->
            let host_entry = Unix.gethostbyaddr addr in
            let payload = Marshal.from_string data 0 in
            Printf.printf
              "%d bytes from %s (%s): icmp_seq=%d time=%.3f ms\n%!"
              (String.length data)
              host_entry.Unix.h_name
              (Unix.string_of_inet_addr addr)
              seq
              ((Unix.gettimeofday () -. payload.timestamp) *. 1000.)
        | _ -> ()
    done
  
  (* Read hostname from command line. *)
  let host =
    if Array.length Sys.argv <> 2
    then (Printf.eprintf "Usage: %s host\n" Sys.argv.(0); exit 1)
    else Sys.argv.(1)
  
  (* Use DNS to find the IP address and canonical name. *)
  let name, addr =
    try
      let h = Unix.gethostbyname host in
      h.Unix.h_name, h.Unix.h_addr_list.(0)
    with Not_found ->
      Printf.eprintf "%s: unknown host %s\n" Sys.argv.(0) host;
      exit 2
  
  (* Make sure we are running as root, since this is required to
     open a socket with SOCK_RAW and send ICMP packets. *)
  let () =
    if Unix.getuid () <> 0
    then (Printf.eprintf "%s: icmp ping requires root privilege\n"
            Sys.argv.(0);
          exit 3)
  
  (* Start the ping loop. *)
  let () =
    Printf.printf "PING %s (%s)\n" name (Unix.string_of_inet_addr addr);
  
    (* Build a socket and destination address. *)
    let proto = (Unix.getprotobyname "icmp").Unix.p_proto in
    let socket = Unix.socket Unix.PF_INET Unix.SOCK_RAW proto in
    let sockaddr = Unix.ADDR_INET (addr, 0) in
  
    (* Use the PID as the ID for packets, and create a counter for
       the sequence number. *)
    let id = Unix.getpid () in
    let seq = ref 0 in
  
    (* Start a background thread to print the echo replies. *)
    ignore (Thread.create (pong socket) id);
  
    (* Loop forever sending echo requests and sleeping. *)
    while true do
      incr seq;
      ping socket sockaddr id !seq;
      Unix.sleep 1
    done
  

