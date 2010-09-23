(* ********************************************************************** *)
(* Defining a Module's Interface *)
(* ********************************************************************** *)
let pleac_Defining_a_Module_s_Interface () = 
  (* Interfaces, also known as module types or signatures, are usually
     saved in files with the same name as the corresponding module but
     with a ".mli" extension. For instance, if the module is defined in
     "YourModule.ml", the interface will be in "YourModule.mli". *)
  
  (* YourModule.mli *)
  val version : string
  
  (* YourModule.ml *)
  let version = "1.00"
  
  (* As with modules, interfaces can also be defined explicitly inside
     of a source file. *)
  
  module type YourModuleSignature =
  sig
    val version : string
  end
  
  module YourModule : YourModuleSignature =
  struct
    let version = "1.00"
  end
  
  (* Signatures can also be anonymous. *)
  
  module YourModule :
  sig
    val version : string
  end =
  struct
    let version = "1.00"
  end
  

