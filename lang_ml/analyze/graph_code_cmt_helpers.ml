(*****************************************************************************)
(* Empty wrappers *)
(*****************************************************************************)

module Ident = struct
    let t _env _x =  ()
    let name = Ident.name
end
module Longident = struct
    let t _env _x = ()
end

let path_t _env _x = ()

module TypesOld = Types
module Types = struct
    let value_description _env _x = ()
    let class_declaration _env _x = ()
    let class_type _env _x = ()
    let class_signature _env _x = ()
    let module_type _env _x = ()
    let signature _env _x = ()
    let type_declaration _env _x = ()
    let exception_declaration _env _x = ()
    let class_type_declaration _env _x = ()
end

let v_option f xs = Common.do_option f xs

let v_string _x = ()
let v_ref _f _x = ()

let meth _env _x = ()
let class_structure _env _x = ()

let module_type _env _x = ()
let module_coercion _env _x = ()
let module_type_constraint _env _x = ()

let constant _env _x = ()
let constructor_description _env _x = ()
let label _env _x = ()
let row_desc _env _x = ()
let label_description _env _x = ()
let partial _env _x =  ()
let optional _env _x = ()
