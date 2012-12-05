let f x = 
  let rec foo x =
    bar x
  and bar x = 
    foo x
  in
  foo x

