(* ********************************************************************** *)
(* Making Numbers Even More Random *)
(* ********************************************************************** *)
let pleac_Making_Numbers_Even_More_Random () = 
  (* This requires installation of the third party the cryptokit library... *)
  let prng = Cryptokit.Random.secure_rng;;
  let buf = String.make 10 ' ';;
  (* random_bytes buf pos len stores len random bytes in string buf, starting at position pos *)
  prng#random_bytes buf 0 10;;  (* buf now contains 10 random bytes *)
  
  

