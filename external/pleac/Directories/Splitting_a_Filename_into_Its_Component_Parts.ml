(* ********************************************************************** *)
(* Splitting a Filename into Its Component Parts *)
(* ********************************************************************** *)
let pleac_Splitting_a_Filename_into_Its_Component_Parts () = 
  let splitext name =
    try
      let root = Filename.chop_extension name in
      let i = String.length root in
      let ext = String.sub name i (String.length name - i) in
      root, ext
    with Invalid_argument _ ->
      name, ""
  
  let dir = Filename.dirname path
  let file = Filename.basename path
  let name, ext = splitext file
  

