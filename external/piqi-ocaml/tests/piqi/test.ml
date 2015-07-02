
let t () =
  let ich = open_in_bin "piqi.piqi.pb" in
  let buf = Piqirun.init_from_channel ich in
  let piqi = Piqi_piqi.parse_piqi buf in

  let och = open_out_bin "piqi.piqi.pb.pb" in
  let data = Piqi_piqi.gen_piqi piqi in
  Piqirun.to_channel och data;

  close_in ich;
  close_out och;
  ()


let _ = t ()
