(* ********************************************************************** *)
(* Copying Data Structures *)
(* ********************************************************************** *)
let pleac_Copying_Data_Structures () = 
  (* Immutable data structures such as int, char, float, tuple, list, Set,
     and Map can be copied by assignment. *)
  let v2 = v1
  let r2 = ref !r1
  
  (* Objects can be shallow-copied using Oo.copy. *)
  let o2 = Oo.copy o1
  
  (* Several built-in types include copy functions. *)
  let a2 = Array.copy a1
  let h2 = Hashtbl.copy h1
  let s2 = String.copy s1
  
  (* Any data structure can be deep-copied by running it through Marshal,
     though this is not very efficient. *)
  let (copy : 'a -> 'a) =
    fun value ->
      Marshal.from_string
        (Marshal.to_string value [Marshal.Closures])
        0
  

