
let go_() =
  let x = 1 in
  let y = 2 in
  (* Where this callback is actually called? *)
  (fun () -> x + y)

let go () =
  go_ ()

let f g () =
  (* it's called here, because it flows from the return of go_, which
   * flows to the return of go, which flows into the local callback
   * variable, which then flows in the parameter g, which
   * is then called here.
   *)
  g ()

let main () = 
  let callback = go() in
  f callback

