(* ********************************************************************** *)
(* Using Closures Instead of Objects *)
(* ********************************************************************** *)
let pleac_Using_Closures_Instead_of_Objects () = 
  (* Since record field names must be unique to their enclosing module,
     define a module to encapsulate the fields of the record type that
     will contain the "methods". *)
  module Counter = struct
    type t = { next  : unit -> int;
               prev  : unit -> int;
               last  : unit -> int;
               get   : unit -> int;
               set   : int  -> unit;
               bump  : int  -> unit;
               reset : unit -> int }
  
    let make count =
      let start = count in
      let count = ref start in
      let prev () = decr count; !count in
      { next  = (fun () -> incr count; !count);
        prev  = prev; last = prev;
        get   = (fun () -> !count);
        set   = (fun count' -> count := count');
        bump  = (fun count' -> count := !count + count');
        reset = (fun () -> count := start; !count)
      }
  end
  
  (* Create and use a couple of counters. *)
  let () =
    let c1 = Counter.make 20 in
    let c2 = Counter.make 77 in
  
    Printf.printf "next c1: %d\n" (c1.Counter.next ()); (* 21 *)
    Printf.printf "next c2: %d\n" (c2.Counter.next ()); (* 78 *)
    Printf.printf "next c1: %d\n" (c1.Counter.next ()); (* 22 *)
    Printf.printf "last c1: %d\n" (c1.Counter.prev ()); (* 21 *)
    Printf.printf "old  c2: %d\n" (c2.Counter.reset ()) (* 77 *)
  
  (* Same as above, but using a "local open" to temporarily expose
     the record fields for convenience. *)
  let () =
    let c1 = Counter.make 20 in
    let c2 = Counter.make 77 in
    let module Local = struct
      open Counter
      let () =
        Printf.printf "next c1: %d\n" (c1.next ()); (* 21 *)
        Printf.printf "next c2: %d\n" (c2.next ()); (* 78 *)
        Printf.printf "next c1: %d\n" (c1.next ()); (* 22 *)
        Printf.printf "last c1: %d\n" (c1.prev ()); (* 21 *)
        Printf.printf "old  c2: %d\n" (c2.reset ()) (* 77 *)
    end in ()
  

