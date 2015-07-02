
let read_file filename =
  let ch = open_in_bin filename in
  let size = in_channel_length ch in
  let res = String.create size in
  really_input ch res 0 size;
  close_in ch;
  res


let string_of_format = function
  | `pb -> "pb"
  | `json -> "json"
  | `json_pretty -> "json_pretty"
  | `xml -> "xml"
  | `xml_pretty -> "xml_pretty"
  | `piq -> "piq"
  | `pib -> "pib"


let printf = Printf.printf


let test f n =
  printf "count: %d\n%!" n;

  let t1 = Unix.gettimeofday () in

  for i = 1 to n
  do
    f ();
    (*
    Gc.minor ();
    *)
    Gc.minor ();
  done;

  let t2 = Unix.gettimeofday () in
  let seconds = t2 -. t1 in

  let per_second = (float_of_int n) /. seconds in
  let per_second = truncate per_second in

  printf "time: %f seconds\n%!" seconds;
  printf "rate: %d calls per second\n\n%!" per_second;

  per_second


let test_convert codec format input n =
  let f = fun () -> codec input format in
  test f n


let test_rw reader writer (format: Piqirun_ext.output_format) bytes n =
  printf "size of Protobuf binary: %d\n%!" (String.length bytes);

  (* read the object into OCaml term representation *)
  let output = reader bytes `pb in

  (* write the object into desired test input format *)
  let input = writer output format in
  (*
  printf "input: %s\n" input;
  *)

  let input_format =
    match format with
      | `json_pretty -> `json
      | `xml_pretty -> `xml
      | #Piqirun_ext.input_format as x -> x
  in

  printf "reading %s objects...\n%!" (string_of_format format);
  let i_rate = test_convert reader input_format input n in

  printf "writing %s objects...\n%!" (string_of_format format);
  let o_rate = test_convert writer format output n in

  printf "%s read/write rate: %d/%d\n\n%!" (string_of_format format) i_rate o_rate;

  print_newline ();

  ()


let test_rw_all reader writer bytes n =
  let formats = [`pb; `json; `json_pretty; `xml; `xml_pretty; `piq; `pib;] in
  List.iter (fun format -> test_rw reader writer format bytes n) formats


let test_addressbook () =
  printf "*** testing OCaml serialization of medium objects ***\n\n";

  let filename = "addressbook.piq.pb" in

  (* Read the addressbook encoded in Protobuf format *)
  let bytes = read_file filename in

  let reader = Addressbook_piqi_ext.parse_address_book in
  let writer = Addressbook_piqi_ext.gen_address_book in

  let n = 100000 in

  test_rw_all reader writer bytes n;
  (*
  test_rw_all reader writer bytes n;

  test_rw reader writer `pb bytes n;
  test_rw reader writer `json bytes n;
  test_rw reader writer `json_pretty bytes n;
  test_rw reader writer `xml bytes n;
  test_rw reader writer `xml_pretty bytes n;
  test_rw reader writer `pib bytes n;
  test_rw reader writer `piq bytes n;
  *)
  ()


let test_piqi () =
  printf "*** testing OCaml serialization of big objects ***\n\n";

  let filename = "piqi.piq.pb" in

  (* Read the Piqi self-specification encoded in Protobuf format *)
  let bytes = read_file filename in

  let reader = Piqi_obj_piqi_ext.parse_piqi in
  let writer = Piqi_obj_piqi_ext.gen_piqi in

  let n = 20000 in

  test_rw_all reader writer bytes n;
  (*
  test_rw_all reader writer bytes n;

  test_rw reader writer `pb bytes n;
  test_rw reader writer `json bytes n;
  test_rw reader writer `json_pretty bytes n;
  test_rw reader writer `xml bytes n;
  test_rw reader writer `xml_pretty bytes n;
  test_rw reader writer `pib bytes n;
  test_rw reader writer `piq bytes n;
  *)
  ()


let set_gc_options () =
  (* Don't set custom options if the OCAMLRUNPARAM environment variable is
   * defined *)
  try ignore (Sys.getenv "OCAMLRUNPARAM")
  with Not_found ->
    let opt = Gc.get () in
    opt.Gc.minor_heap_size <- 4 * 1024 * 1024; (* Minor heap size: 4m *)
    opt.Gc.space_overhead <- 200; (* run major GC less frequently, but waste more RAM *)
    Gc.set opt


let _ =
  (*
  Gc.compact ();
  Gc.print_stat stdout;
  print_newline ();
  *)
  set_gc_options ();

  test_addressbook ();
  test_piqi ();
  ()

