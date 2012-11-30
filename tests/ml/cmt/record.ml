type record = { fld1: int; fld2: float }
let use_record = function
  { fld1 = i; _} -> i
