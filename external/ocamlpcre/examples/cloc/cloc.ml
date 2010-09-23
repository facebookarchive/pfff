open Pcre

let read_whole_channel ch =
  let size = 4096 in
  let strbuf = String.create size
  and buf = Buffer.create 65536
  and len = ref size in
  while !len <> 0 do
    len := input ch strbuf 0 size;
    Buffer.add_substring buf strbuf 0 !len; done;
  Buffer.contents buf

let _ =
  let str = read_whole_channel stdin in
  let str = qreplace ~pat:"/\\*(.|\n)*?\\*/" str in
  let str = qreplace_first ~pat:"^(\n|\\s)+" str in
  let str = qreplace ~pat:"\n+((\n|\\s)\n)*" ~templ:"\n" str in
  print_string str
