
let once = ref false
let ustring_of_uchar x y =
  if not !once then begin
    print_string "(lib-json)ustring_of_uchar: Todo\n"; flush stdout;
    once := true
  end;
  "PBUSTRINGOFCHAR"
