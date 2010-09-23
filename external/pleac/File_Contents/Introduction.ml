(* ********************************************************************** *)
(* Introduction *)
(* ********************************************************************** *)
let pleac_Introduction () = 
  let () =
    try
      while true do
        let line = input_line datafile in
        let size = String.length line in
        Printf.printf "%d\n" size             (* output size of line *)
      done
    with End_of_file -> ()
  
  (*-----------------------------*)
  
  let line_stream_of_channel channel =
    Stream.from
      (fun _ -> try Some (input_line channel) with End_of_file -> None)
  
  let output_size line =
    Printf.printf "%d\n" (String.length line) (* output size of line *)
  
  let () =
    Stream.iter output_size (line_stream_of_channel datafile)
  
  (*-----------------------------*)
  
  let lines =
    let xs = ref [] in
    Stream.iter
      (fun x -> xs := x :: !xs)
      (line_stream_of_channel datafile);
    List.rev !xs
  
  (*-----------------------------*)
  
  let slurp_channel channel =
    let buffer_size = 4096 in
    let buffer = Buffer.create buffer_size in
    let string = String.create buffer_size in
    let chars_read = ref 1 in
    while !chars_read <> 0 do
      chars_read := input channel string 0 buffer_size;
      Buffer.add_substring buffer string 0 !chars_read
    done;
    Buffer.contents buffer
  
  let slurp_file filename =
    let channel = open_in_bin filename in
    let result =
      try slurp_channel channel
      with e -> close_in channel; raise e in
    close_in channel;
    result
  
  let whole_file = slurp_file filename
  
  (*-----------------------------*)
  
  let () =
    (* Onetwothree *)
    List.iter (output_string handle) ["One"; "two"; "three"];
  
    (* Sent to default output handle *)
    print_string "Baa baa black sheep\n"
  
  (*-----------------------------*)
  
  let buffer = String.make 4096 '\000'
  let rv = input handle buffer 0 4096
  (* rv is the number of bytes read, *)
  (* buffer holds the data read *)
  
  (*-----------------------------*)
  
  #load "unix.cma";;
  let () =
    Unix.ftruncate descr length;
    Unix.truncate (Printf.sprintf "/tmp/%d.pid" (Unix.getpid ())) length
  
  (*-----------------------------*)
  
  let () =
    let pos = pos_in datafile in
    Printf.printf "I'm %d bytes from the start of datafile.\n" pos
  
  (*-----------------------------*)
  
  let () =
    seek_in in_channel pos;
    seek_out out_channel pos
  
  #load "unix.cma";;
  let () =
    Unix.lseek descr 0     Unix.SEEK_END; (* seek to the end    *)
    Unix.lseek descr pos   Unix.SEEK_SET; (* seek to pos        *)
    Unix.lseek descr (-20) Unix.SEEK_CUR; (* seek back 20 bytes *)
  
  (*-----------------------------*)
  
  #load "unix.cma";;
  let () =
    let written =
      Unix.write datafile mystring 0 (String.length mystring) in
    let read =
      Unix.read datafile mystring 5 256 in
    if read <> 256 then Printf.printf "only read %d bytes, not 256\n" read
  
  (*-----------------------------*)
  
  #load "unix.cma";;
  let () =
    (* don't change position *)
    let pos = Unix.lseek handle 0 Unix.SEEK_CUR in
    (* ... *)
    ()
  

