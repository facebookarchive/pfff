(*****************************************************************************)
(* Empty wrappers *)
(*****************************************************************************)

module Ident = struct
    let t env x =  ()
    let name = Ident.name
end
module Longident = struct
    let t env x = ()
end

let path_t env x = ()

module TypesOld = Types
module Types = struct
    let value_description env x = ()
    let class_declaration env x = ()
    let class_type env x = ()
    let class_signature env x = ()
    let module_type env x = ()
    let signature env x = ()
    let type_declaration env x = ()
    let exception_declaration env x = ()
    let class_type_declaration env x = ()
end

let v_option f xs = Common.do_option f xs

let v_string x = ()
let v_ref f x = ()

let meth env x = ()
let class_structure env x = ()

let module_type env x = ()
let module_coercion env x = ()
let module_type_constraint env x = ()

let constant env x = ()
let constructor_description env x = ()
let label env x = ()
let row_desc env x = ()
let label_description env x = ()
let partial env x =  ()
let optional env x = ()
