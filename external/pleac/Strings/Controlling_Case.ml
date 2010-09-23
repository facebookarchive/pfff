(* ********************************************************************** *)
(* Controlling Case *)
(* ********************************************************************** *)
let pleac_Controlling_Case () = 
  
  (* Just use the String module's uppercase, lowercase, capitalize and
   * uncapitalize *)
  
  let big = String.uppercase little;;    (* "bo peep" -> "BO PEEP" *)
  let little = String.lowercase big;;    (* "JOHN" -> "john" *)
  let big = String.capitalize little;;   (* "bo" -> "Bo" *)
  let little = String.uncapitalize big;; (* "BoPeep" -> "boPeep" *)
  
  (* Capitalize each word's first character, downcase the rest *)
  let text = "thIS is a loNG liNE";;
  let text = String.capitalize (String.lowercase text);;
  print_endline text;;
  
  (*
  This is a long line
  *)
  
  (* To do case insensitive comparisons *)
  if String.uppercase a = String.uppercase b then
    print_endline "a and b are the same\n";;
  
  let randcap fn =
    let s = slurp_to_string fn in
    for i = 0 to String.length s - 1 do
      if Random.int 100 < 20 then
        String.blit (String.capitalize (String.sub s i 1)) 0 s  i 1
    done;
    print_string s;;
  
  
  (*
  # randcap "/etc/passwd";;
  
  ##
  # User DatAbAse
  # 
  # Note That this fIle is consuLTed wHen the sysTeM Is runninG In single-user
  # modE.  At other times this iNformAtion is handlEd by one or moRe oF:
  # lOokupD DIrectorYServicEs  
  # By default, lOOkupd getS inFormaTion frOm NetInFo, so thiS fIle will 
  # not be cOnsultEd unless you hAvE cHaNged LOokupd's COnfiguratiOn.
  # This fiLe is usEd while in siNgle UseR Mode.
  #
  # TO Use this file for noRmal aUthEnticatIon, you may eNable it With
  # /ApPlicatiOns/Utilities/DiRectory AccEss.
  ##
  
  < ... snip ... >
  *)
  

