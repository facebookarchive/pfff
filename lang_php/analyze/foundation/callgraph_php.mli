(*s: callgraph_php.mli *)

(* Used by xdebug.ml  *)
type kind_call = 
    | FunCall of string
    | ObjectCall of string (* class *) * string (* method *)
    | ClassCall of string (* module *) * string

type call = Call of Entity_php.id (* from *) * 
                    Entity_php.id (* to *) * 
                    kind_call_and_pos 
   and kind_call_and_pos = 
     (* Direct encompasses function calls as well as static method calls as
      * both are statically direct calls.
      *)
     | Direct     of Namespace_php.nameS Ast_php.wrap
     | MethodCall of Namespace_php.nameS Ast_php.wrap * call_extra_info
     | IndirectTodo (* of Ast.expression * call_extra_info *)

     and call_extra_info = {
       todo: unit;
     }

(* Specialized types used in database, to avoid storing each time one
 * of the id as this id will already be the key of the table.
 * For function call it also factorize things. We define two types
 * because depending on the direction we can or not factorize things.
 *)
type callsites_opt = 
  | DirectCallToOpt of 
      Namespace_php.nameS * Ast_php.info list (* instances *) * 
        Entity_php.id list (* candidates *)

  (* For now we dont factorize method calls. Could. *)
  | MethodCallToOpt of 
      Entity_php.id * Namespace_php.nameS Ast_php.wrap * call_extra_info
  | IndirectFuncPtCallToOptTodo
      (* of Entity.id * Ast.expression * call_extra_info *)

type callersinfo_opt = 
  | DirectCallerIsOpt of 
      Entity_php.id * Namespace_php.nameS * int (* nb instances *)
  | MethodCallerIsOpt of 
      Entity_php.id * Namespace_php.nameS Ast_php.wrap * call_extra_info
  | IndirectFuncPtCallerIsOptTodo
      (* of Entity.id * Ast.expression * call_extra_info *)


(* types used mostly by callees_of_id and callers_of_id wrappers in
 * database_php.ml *)
type callsite = 
  CallSite of Entity_php.id * kind_call_and_pos
type callerinfo = 
  CallerInfo of Entity_php.id * kind_call_and_pos

(* ---------------------------------------------------------------------- *)
val default_call_extra_info : call_extra_info

type analysis_confidence = int
val no_info_confidence: analysis_confidence

type node = {
  name: string;
  id: Entity_php.id; (* can be interpreted as the caller or callee *)
  extra: call option;
  confidence: analysis_confidence;
  gray: bool;
}

type idtree = node Common.treeref
type calltree = node Common.treeref


(* as argument for the building functions *)
type calltree_preferences = {
  squeeze_duplicate: bool; (* when want abstract about each instance call *)
  squeeze_duplicate_graph: bool; 
  filter_id: Entity_php.id -> bool; (* for instance to filter non-x86 entities *)
  filter_confidence: analysis_confidence -> bool;
  put_at_end_filtered_and_gray_ize: bool;
}
val default_calltree_preferences: calltree_preferences

(* ---------------------------------------------------------------------- *)
val s_of_kind_call: kind_call -> string

(* ---------------------------------------------------------------------- *)
val callerinfo_to_call: callerinfo -> Entity_php.id -> call
val callsite_to_call: Entity_php.id -> callsite -> call

val callsites_opt_to_callsites: 
  callsites_opt -> callsite list
val callersinfo_opt_to_callersinfo: 
  callees_of_id:(Entity_php.id -> callsites_opt list) ->
  Entity_php.id ->
  callersinfo_opt -> callerinfo list

val id_of_callerinfo:  callerinfo -> Entity_php.id
val id_of_callsite:    callsite   -> Entity_php.id

(* ---------------------------------------------------------------------- *)

(* used mainly by gui *)
val calltree_callers_of_f:
  Entity_php.id ->
  depth:int ->
  parent_and_extra_opt: 
    (Entity_php.id * kind_call_and_pos * analysis_confidence * bool) option ->
  namefunc:(Entity_php.id -> string) ->
  callersfunc:(Entity_php.id -> callerinfo list) -> 
  fullid_info:(Entity_php.id -> Entity_php.fullid) ->
  preferences:calltree_preferences -> 
  calltree

val calltree_callees_of_f:
  Entity_php.id ->
  depth:int ->
  parent_and_extra_opt: 
    (Entity_php.id * kind_call_and_pos * analysis_confidence * bool) option ->
  namefunc:(Entity_php.id -> string) ->
  calleesfunc:(Entity_php.id -> callsite list) -> 
  fullid_info:(Entity_php.id -> Entity_php.fullid) ->
  preferences:calltree_preferences -> 
  calltree


(* ---------------------------------------------------------------------- *)
(* The function here returns only local information. For a full
 * caller/callee analysis then need to use this local information and
 * perform a global analysis. cf database.ml and functions below. 
 *)
val callees_of_any: 
  Ast_php.any -> Namespace_php.nameS Ast_php.wrap list

val method_callees_of_any: 
  Ast_php.any -> Namespace_php.nameS Ast_php.wrap list


(* If we process the body of a method, this method can use PHP sugar with 
 * the self:: and parent:: syntax, hence the extra arguments.
 *)
val static_method_callees_of_any: 
  self: string option ->
  parent: string option ->
  Ast_php.any -> Namespace_php.nameS Ast_php.wrap list

(*x: callgraph_php.mli *)
(*e: callgraph_php.mli *)
