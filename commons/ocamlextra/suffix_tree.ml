(* made by Sebastien Ferre *)

(* type of nodes in suffix trees *)
type node = {
    seqid : int; (* sequence index in which the positions start and final are defined *)
    mutable start : int;     (* start and final position of the word labelling the node *)
    final : int ref;
    mutable link : node; (* suffix link *)
    v : node_value
  }
and node_value =
  | Children of (char,node) Hashtbl.t (* for non-leaves: children nodes *)
      (* for the key '\000', all values are relevant (use Hashtbl.find_all) *)
  | Index of int (* for leaves: position of recognized suffix *)

(* type of suffix trees *)
type t = string array * node

(* the initial root node *)
let empty : unit -> node =
  fun () ->
    let rec root = {seqid= -1; start=0; final=ref (-1); link=root; v=Children (Hashtbl.create 2)} in
    root


(* --------------------------------------------------------------------------------
   Operations on substrings of sequences
   -------------------------------------------------------------------------------- *)

type subseq = string * int * int  (* (seq, pos, len) *)

let subseq_empty = ("",0,0)  (* non-significant subseq *)

let subseq_is_empty (s,pos,len) = len = 0

let subseq_get (s,pos,len) i = s.[pos+i]

let subseq_length (s,pos,len) = len

let subseq_sub (s,pos,len) pos' len' = (s,pos+pos',len')

let subseq_extend (s,pos,len) = (s,pos,len+1)

(* -------------------------------------------------------------------------------
   Operations on implicit nodes (explicit, implicit, child : node * subseq * node)
   the snd node [child] is significant only when [implicit] is not the empty string,
   and is the child that recognizes [implicit] starting from [explicit]. [implicit] is
   defined by a sequence, a start and a length.
   ------------------------------------------------------------------------------- *)

let eq_char c1 c2 =
  c1<>'\000' & c1=c2  (* ensures that 2 terminal symbols '\000' are pairwise different (for GST only, not necessary for ST) *)

(* returns the child node that recognizes [implicit] from the node [explicit] *)
let get_child seqar (explicit,implicit) =
  if subseq_is_empty implicit
  then explicit
  else
    let c = subseq_get implicit 0 in
    if c = '\000'
    then raise Not_found
    else
      match explicit.v with
      | Children h -> Hashtbl.find h c
      | Index _ -> raise Not_found
    (* List.find (fun child -> eq_char seqar.(child.seqid).[child.start] c) explicit.children *)

(* ensures that implicit does not span over another node below [explicit] *)
let rec canonical seqar (explicit,implicit,child) =
  if subseq_is_empty implicit
  then (explicit,implicit,child)
  else
    let l = !(child.final) - child.start + 1 in
    let a = subseq_length implicit in
    if a < l
    then (explicit,implicit,child)
    else
      let implicit' = subseq_sub implicit l (a-l) in
      canonical seqar (child, implicit', get_child seqar (child,implicit'))

(* test whether an implicit node is the root node *)
let is_root root (explicit,implicit,_) =
  explicit == root & subseq_is_empty implicit

(* test whether the extension of an implicit node by [seqar.(k).[i]] is still recognized in the GST,
   and if yes, returns the implicit node extended by 1 position, otherwise returns [None]. *)
let has_child seqar (explicit,implicit,child) (k,i) =
  let a = subseq_length implicit in
  if a <> 0 then
    if eq_char seqar.(child.seqid).[child.start+a] seqar.(k).[i]
    then Some (explicit, subseq_extend implicit, child)
    else None
  else
    try
      let implicit' = (seqar.(k),i,1) in
      Some (explicit, implicit', get_child seqar (explicit,implicit'))
    with Not_found -> None

(* --------------------------------
   creation of new nodes and leaves
   -------------------------------- *)

let add_leaf (seqar,root) node seqid start final_ref index =
  match node.v with
  | Children h ->
      Hashtbl.add h
	seqar.(seqid).[start]
	{seqid=seqid; start=start; final=final_ref; link=root; v=(Index index)}
  | Index _ -> raise (Invalid_argument "Suffix_tree.add_leaf: 2nd argument must not be a leaf")

(* make explicit an implicit node by inserting a new node between [explicit] and [child] *)
let insert_node (seqar,root) (explicit,implicit,child) =
  let a = subseq_length implicit in
  if a = 0
  then explicit
  else
    match explicit.v with
    | Children h ->
	let c_child_old = seqar.(child.seqid).[child.start] in
	let c_child_new = seqar.(child.seqid).[child.start+a] in
	let n' = {
	  seqid = child.seqid;
	  start = child.start;
	  final = ref (child.start+a-1);
	  link = root;
	  v = Children (let h' = Hashtbl.create (Hashtbl.length h) in Hashtbl.add h' c_child_new child; h')
	} in
	child.start <- child.start+a;
	Hashtbl.replace h c_child_old n';
	n'
    | Index _ -> raise (Invalid_argument "Suffix_tree.insert_node: first part of 2nd argument must not be a leaf")

(* add a suffix link from [pred_opt] (if defined) to [explicit] *)
let add_link root pred_opt explicit =
  (*if explicit != root then*) (* create a new suffix link *)
    match pred_opt with
    | Some n -> (*if n.link = None then*) n.link <- explicit
    | None -> ()

(* ------------ 
   suffix links
   ------------ *)

(* get the node refered by the suffix link at [n] *)
(*
let suffix_link (root : node) (n : node) : node =
  match n.link with
  | None -> root  (* by default, the suffix link points to the root node *)
  | Some n' -> n'
*)

(* extend suffix_link for implicit nodes *)
let link (seqar,root) = function  (* TODO *)
  | (explicit,implicit,_) when subseq_is_empty implicit ->
      let explicit' = explicit.link (*suffix_link root explicit*) in
      (explicit', subseq_empty, explicit')
  | (explicit,implicit,_) ->
      if explicit == root
      then
	let implicit' = subseq_sub implicit 1 (subseq_length implicit - 1) in
	canonical seqar (root, implicit', get_child seqar (root,implicit'))
      else
	let explicit' = explicit.link (*suffix_link root explicit*) in
	canonical seqar (explicit', implicit, get_child seqar (explicit',implicit))

(* --------------------------------------------------------------
   GST update for the new character c at position i in sequence k
   -------------------------------------------------------------- *)

(* state for 'update' *)
type res = {
    terminal : int ref;
    mutable startj : int;
    mutable startnode : node * subseq * node
  }

let rec update (seqar,root) (k,i) res pred_opt =
  (* c = seqar.(k).[i] *)
  match has_child seqar res.startnode (k,i) with
  | Some extended_startnode -> (* startnode can be extended by [c] *)
      let explicit, implicit, _ = res.startnode in
      assert (pred_opt = None or subseq_is_empty implicit);
        (* if a link has been followed after node creation, then we are on an explicit node *)
      add_link root pred_opt explicit;
      res.startnode <- canonical seqar extended_startnode
  | None -> (* startnode cannot be extended by [c] ... *)
      let n' = insert_node (seqar,root) res.startnode in (* ... so we insert a new node ... *)
      add_link root pred_opt n';  (* ... a suffix link from the last created node (if defined) ... *)
      if seqar.(k).[res.startj] <> '\000' then
	add_leaf (seqar,root) n' k i res.terminal res.startj;  (* ... and a new leaf for the suffix at position [res.startj] *)
      res.startj <- res.startj + 1; (* prepare for the next suffix *)
      if not (is_root root res.startnode)
      then begin (* while [res.startnode] is not the root, and cannot be extended by [c] ... *)
	res.startnode <- link (seqar,root) res.startnode; (* ... follow the suffix link to find the next suffix ... *)
	update (seqar,root) (k,i) res (Some n') end  (* ... and loop on [update] *)

(* -------------------------------
   implementing the .mli interface
   ------------------------------- *)

let make : string list -> t =
  fun l_seq ->
    let l = List.length l_seq in
    let seqar = Array.make l "" in
    let root = empty () in
    let st = (seqar, root) in
    ignore (List.fold_left
      (fun k seq -> (* for every sequence/string [seq], numbered [k] ... *)
	seqar.(k) <- seq ^ String.make 1 '\000'; (* add a terminal symbol ... *)
	let res = {terminal=ref (-1); startj=0; startnode=(root,subseq_empty,root)} in (* initialize for [update] ... *)
	for i = 0 to String.length seqar.(k) - 1 do (* for every position [i] in the sequence ... *)
	  incr res.terminal; (* increment the leaves final position ... *)
	  update st (k,i) res None (* call [update] for updating the suffix tree with the character at position [i] *)
	done;
	k+1)
      0 l_seq);
    st

let string (seqar,root : t) (k : int) =
  let seq = seqar.(k) in
  String.sub seq 0 (String.length seq - 1) (* removing the terminal symbol *)

let string_list (seqar,root : t) =
  List.map (fun seq -> String.sub seq 0 (String.length seq - 1)) (Array.to_list seqar)

let root (seq,root : t) = root

let word (seqar,root) node =
  if node == root
  then ""
  else String.sub seqar.(node.seqid) node.start (!(node.final) - node.start + (match node.v with Children _ -> 1 | Index _ -> 0))

let children (gst : t) node =
  match node.v with
  | Children h ->
      Hashtbl.fold (fun c n l -> n::l) h []
  | Index _ -> []

let index (seq,root) node : int * int =
  match node.v with
  | Children _ -> raise (Invalid_argument "Suffix_tree.index: 2nd argument must be a leaf")
  | Index i -> (node.seqid, i)

let linked_node (seqar,root : t) (n : node) : node =
  n.link (*suffix_link root n*)

let rec implicit_node (seqar,node : t) (word : string) =
  let (explicit, (s,i,len), child) = implicit_node_aux (seqar,node) (word,0,String.length word) in
  (explicit, String.sub s i len, child)
and implicit_node_aux (seqar,node) implicit =
  let w = subseq_length implicit in
  let child = get_child seqar (node,implicit) in
  let l = !(child.final) - child.start + 1 in
  let a = ref 1 in
  while !a < l & !a < w & eq_char seqar.(child.seqid).[child.start + !a] (subseq_get implicit !a) do
    incr a
  done; (* [!a] is the first mismatch position, or the length of [child] label *)
  if !a < w then
    if !a < l
    then raise Not_found
    else implicit_node_aux (seqar,child) (subseq_sub implicit !a (w - !a))
  else (node,implicit,child) 

(*
let rec synthesized (seqar,root : t) (f : 'a list -> node -> 'a) =
  synthesized_node (seqar,root) f root
and synthesized_node st f node =
  f (List.map (synthesized_node st f) (children st node)) node
*)

(* general fold *)
let rec fold : t -> ('h -> node -> bool) -> ('h -> node -> 'h) -> ('s list -> 'h -> node -> 's) -> 'h -> 's =
  fun gst f h s init ->
    fold_node gst f h s init (root gst)
and fold_node gst f h s h_node node =
  s
    (List.map
       (fun child -> fold_node gst f h s (h h_node child) child)
       (List.filter (f h_node) (children gst node)))
    h_node
    node

(* synthesized attributes only *)
let fold_s_node gst s node = fold_node gst (fun _ _ -> true) (fun _ _ -> ()) (fun l _ n -> s l n) () node
let fold_s gst s = fold_s_node gst s (root gst)

(* filtering and synthesizing, no inheritance *)
let fold_fs gst f s = fold gst (fun _ n -> f n) (fun _ _ -> ()) (fun l _ n -> s l n) ()


type tree = Node of string * tree list | Leaf of string * (int * int)

let readable gst =
  fold_s gst
    (fun l n ->
      let w = word gst n in
      if l=[]
      then Leaf (w, index gst n)
      else Node (w, l))

(* applications of suffix trees *)

let exact_matches : t -> string -> (int * int) list =
  fun gst word ->
    try
      let explicit, implicit, child = implicit_node gst word in
    fold_s_node gst
	(fun l n -> if l=[] then [index gst n] else List.concat l)
	child
    with Not_found -> []



let contained_string gst word = 
  List.map (fun (i,j) -> Array.get (fst gst) i)  (exact_matches gst word)



