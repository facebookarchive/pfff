(* ********************************************************************** *)
(* Example: Module Template *)
(* ********************************************************************** *)
let pleac_Example__Module_Template () = 
  (* Some.ml *)
  module Module =
  struct
    (* set the version for version checking *)
    let version = "0.01"
  
    (* initialize module globals (accessible as Some.Module.var1 *)
    let var1 = ref ""
    let hashit = Hashtbl.create 0
  
    (* file-private lexicals go here *)
    let priv_var = ref ""
    let secret_hash = Hashtbl.create 0
  
    (* here's a file-private function *)
    let priv_func () =
      (* stuff goes here. *)
      ()
  
    (* make all your functions, whether exported or not *)
    let func1 () = (* ... *) ()
    let func2 () = (* ... *) ()
    let func3 a b = (* ... *) ()
    let func4 h = (* ... *) ()
  
    (* module clean-up code here *)
    let () =
      at_exit
        (fun () ->
           (* ... *)
           ())
  end
  
  (* Some.mli *)
  module Module :
  sig
    val version : string
    val var1 : string ref
    val hashit : (string, string) Hashtbl.t
    (* priv_var, secret_hash, and priv_func are omitted,
       making them private and inaccessible... *)
    val func1 : unit -> unit
    val func2 : unit -> unit
    val func3 : 'a -> 'b -> unit
    val func4 : (string, string) Hashtbl.t -> unit
  end
  

