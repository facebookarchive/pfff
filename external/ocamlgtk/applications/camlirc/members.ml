(**************************************************************************)
(*     Lablgtk - Camlirc                                                  *)
(*                                                                        *)
(*    * You are free to do anything you want with this code as long       *)
(*      as it is for personal use.                                        *)
(*                                                                        *)
(*    * Redistribution can only be "as is".  Binary distribution          *)
(*      and bug fixes are allowed, but you cannot extensively             *)
(*      modify the code without asking the authors.                       *)
(*                                                                        *)
(*    The authors may choose to remove any of the above                   *)
(*    restrictions on a per request basis.                                *)
(*                                                                        *)
(*    Authors:                                                            *)
(*      Nobuaki Yoshida  <nyoshi@dd.iij4u.or.jp>                          *)
(*      Jacques Garrigue <garrigue@kurims.kyoto-u.ac.jp>                  *)
(*                                                                        *)
(**************************************************************************)

(* $Id: members.ml 1354 2007-07-20 04:18:38Z garrigue $ *)

exception Members

class members_signals ~(selected: string GUtil.signal) =
  object
    inherit GUtil.ml_signals [selected#disconnect]
    method selected = selected#connect ~after
  end

let nick_regexp = Str.regexp "@*\\(.+\\)"

let non_prefixed_nick nick =
  if Str.string_match nick_regexp nick 0 then
    Str.matched_group 1 nick else nick

class members ?width ?packing ?show () =
  let adj_mem = GData.adjustment ()
  in
  let mem = GList.clist ~columns:1 ~vadjustment:adj_mem ~titles:["nickname"] 
      ?width ?packing ?show ()
  and sb_mem = 
    GRange.scrollbar `VERTICAL ~adjustment:adj_mem ?packing ?show ()
  and get_nid n l = 
    try
      (n, false, List.assoc n l)
    with
      Not_found -> (("@"^n), true, List.assoc ("@"^n) l)
  and selected = new GUtil.signal ()
  in
  object (self)
    val mutable mem_list = []
    method append n = mem_list <- (n, mem#append [n])::mem_list
    method clear () =
      begin
	mem_list <- [];
	mem#clear ()
      end
    method remove n = 
      try
	let 
	    (real_n, _, nid) = get_nid n mem_list
	in
	mem#remove ~row:nid;
	mem_list <- List.remove_assoc real_n mem_list
      with Not_found -> ()
    method member_list = List.map 
	(fun (n,_) -> non_prefixed_nick n) mem_list
    method check n =
      try
	let _ = get_nid n mem_list
	in true
      with Not_found -> false
    method change n new_n =
      try
	let (real_n, prefixed, nid) = get_nid n mem_list
	in
	mem#remove ~row:nid;
	let
	    new_real_n = if prefixed then "@"^new_n else new_n
	in
	begin
	  mem#insert ~row:nid [new_real_n];
	  mem_list <- (new_real_n, nid)::(List.remove_assoc real_n mem_list)
	end
      with Not_found -> ()
    method selected = selected
    method connect = new members_signals ~selected:self#selected
    initializer
      mem#connect#select_row
	~callback:
	(fun ~row ~column ~event ->
	  begin
	    let
		nick = mem#cell_text row column
	    in
	    begin
	      if Str.string_match nick_regexp nick 0 then
		let nick = Str.matched_group 1 nick in
		selected#call nick;
		()
	      else ();
	      ()
	    end
	  end);
      ()
  end
