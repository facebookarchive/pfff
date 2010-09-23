(*s: type_php.mli *)

(*s: type phptype *)
type phptype = phptypebis list  (* sorted list, cf typing_php.ml *)
   (* old: | Union of phptype list *)

  and phptypebis = 
    | Basic       of basictype

    | ArrayFamily of arraytype

    (* duck typing style, dont care too much about the name of the class 
     * TODO qualified name ?phpmethod_type list * string list ? *)
    | Object      of string list (* sorted list, class names *)

    (* opened file or mysql connection *)
    | Resource 

    (* PHP 5.3 has closure *)
    | Function of  
        phptype option (* when have default value *) list * 
        phptype (* return type *)

    | Null

    (* TypeVar is used by the type inference and unifier algorithn.
     * It should use a counter for fresh typevariables but it's
     * better to use a string so can give readable type variable like 
     * x_1 for the typevar of the $x parameter.
     *)
    | TypeVar of string

    (* kind of bottom *)
    | Unknown
    (* Top aka Variant, but should never be used *)
    | Top 
(*x: type phptype *)
    and basictype =
      | Bool
      | Int
      | Float
      | String
          
      (* in PHP certain expressions are really more statements *)
      | Unit 
(*x: type phptype *)
    and arraytype = 
      | Array  of phptype
      | Hash   of phptype
      (* duck typing style, ordered list by fieldname *)
      | Record of (string * phptype) list

 (*s: tarzan annotation *)
  (* with tarzan *)
 (*e: tarzan annotation *)
(*e: type phptype *)

exception BadType of string

(*x: type_php.mli *)
val string_of_phptype: phptype -> string
(*e: type_php.mli *)
