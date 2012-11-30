let test_for () =

  for i = 0 to 10 do
    let _ = i in
    ()
  done

let i = "global"

let test_for2 () =

  for i = 0 to 10 do
    let _ = i in
    ()
  done;
  let _ = i in
  ()

