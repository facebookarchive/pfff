
let once = ref false
let ustring_of_uchar x y =
  if not !once then begin
    prerr_string "(lib-json)ustring_of_uchar: Todo\n"; flush stderr;
    once := true
  end;
  "PBUSTRINGOFCHAR"
