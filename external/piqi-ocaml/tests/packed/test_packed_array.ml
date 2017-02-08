
let t () =
  print_endline "testing packed repeated fields represented as OCaml arrays";
  let ich = open_in_bin "test-all.piq.pb" in
  let buf = Piqirun.init_from_channel ich in
  let piqi = Packed_array_piqi.parse_r_all buf in

  let och = open_out_bin "test-all.piq.pb.packed-array" in
  let data = Packed_array_piqi.gen_r_all piqi in
  Piqirun.to_channel och data;

  close_in ich;
  close_out och;
  ()


let _ = t ()
