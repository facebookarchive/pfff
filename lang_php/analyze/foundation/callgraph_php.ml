(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

open Common 

open Ast_php

module Ast = Ast_php

module N = Namespace_php
module E = Entity_php

module V = Visitor_php
module V2 = Visitor2_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * This file was containing mostly type definitions. As a callgraph analysis
 * requires a global analysis, it requires information from database.ml
 * and so most of the code was in database.ml. But I have moved now
 * code from database and relation here to centralize all the code
 * for the callgraph generation. As it still requires some global analysis,
 * some functions are incomplete as they need some special functions 
 * and so they are passed as parameters from the database.ml file.
 * 
 * The goal here is just like for type annotation: put enough information
 * locally so algorithms can do useful work. So when we have a direct funcall, 
 * the user may want to fastly know the possible candidates, and also
 * how many instances of such funcall there is in this function and 
 * their places in the code. Also the user may want to know also for
 * a function the set of callers. For an indirect function call, this
 * is the same. Hence the types callsite and callerinfo below and 
 * their use in database.ml
 * 
 * Some things could be done more lazily. For instance
 * we could just store in the callees table and so callsite type
 * the set of strings and  their instances. Having 
 * that, the user can use other parts of 
 * our database with helpers functions to find the actual id candidates.
 * But this require an extra action in the database and so we currently
 * inline directly the information in the callee table, just to 
 * be a little more convenient. But note that
 * even if this candidate resolution is not necessary to be put in the
 * callees table for direct function calls, it is still necessary 
 * to resolve those candidates in the building of the callgraph as
 * we have to know the actual ids to extend the caller information
 * of those ids. This is even more true for indirect function pointer.
 * 
 * 
 * 
 * See also the file aliasing_function.ml and database_build.ml.
 * The full story to build the callgraph information is:
 * 
 *  - obviously parse the files and store their ast, done in Database_build
 * 
 *  - build the direct caller/callee table in Database_build too using
 *    the callee_of_ast below.
 * 
 *  - For the indirect caller/callees we need more information, especially
 *    type information and candidates so:
 * 
 *     * reannotate typing information, trying to use global information
 *       about definition of diverse entities that we were not
 *       able to find but that database can, especially 
 *       Database_query.type_of_ident. See also typing.ml
 * 
 *     * compute list of candidates based on their type,
 *       in database_build, using functions from aliasing_function.ml
 * 
 *     * iterate again and find potential indirect call site and use 
 *       both the candidates and the better typing information to 
 *       find a match, using functions from aliasing_function.ml.
 * 
 *) 


(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* 
 * history: 
 *  - simple version:  
 *     id (func) vs  string list, and the keys in the callers_of and 
 *     callees_of was also talking about string
 * 
 *  - handling function pointer analysis: have the IndirectFuncPtCall,
 *    but were losing some precision as was still using some string
 *    as key of the table.
 * 
 *  - id vs id, so resolve candidates and avoid losing precision 
 *    for the indirect case. Resolving candidates and using ids
 *    mean a degradation of performance.
 * 
 *  - remove some of the 'of ... string' which were really just cache 
 *    and so redundant.
 * 
 *  - find better names to the caller/callee types 
 * 
 *  - introduce the general 'call' type (obvious retrospectively ...)
 * 
 *  - add position information in DirectCall too, in some way like
 *    IndirectFuncPtCall, more consistent. Putting multiple instances
 *    also mean a degradation of performance. Where before we 
 *    were just keeping a caller/callee at the string level and 
 *    on coarse grained, we now do finer grained by taking account
 *    multiple instances and we also work at id level, not string
 *    level.
 * 
 *  - optimized version where gather all instances and candidates to 
 *    avoid having a cartesian product each time, which would make
 *    the callsites list huge, and in the end the time to build 
 *    the callgraph on the disk too long. 
 *    So introduce the callsites_opt and callersinfo_opt types and
 *    converters. This is because of call 10 times spin_lock and 
 *    spin_lock have 20 possible definitions, then get 200 
 *    callsites in the db. With the xxx_opt we factorize that a little.
 * 
 *  - use new id that is more compact, but have less information and 
 *    so need now pass more functions from database_c to get fullid
 * 
 * 
 * Why had 'DirectCall of string' before, instead of id ? Was simpler. 
 * When have 
 * 
 *  int foo() {
 *     ...
 *     bar()
 *     ...
 *  (and now also)
 *     x.func()
 * 
 * Then 'foo' has an id, but 'bar' has not, and so can be associated with
 * many different id with same name via db.defs . Also
 * 'x.func' can also point to many things and correspond to many
 * different id through the pointer analysis.
 * 
 * The 'id' below corresponds to the resolved function called, through
 * the function pointer analysis. Can also resolve the string
 * by putting all the 'id' having this name.
 * 
 * 
 * 
 * 
 * TODO, later can store also the nodei.     
 * string list should be   id_func_pos * nodei 
 * 
 * TODO, maybe can move code from database_c here by giving the func
 * to access the global information as a parameter ?
 *     
 * TODO: have a position instead of string wrap ?
 * 
 *)

(* Used by xdebug.ml  *)
type kind_call = 
    | FunCall of string
    | ObjectCall of string (* class *) * string (* method *)
    | ClassCall of string (* module *) * string


type call = Call of Entity_php.id (* from *) * 
                    Entity_php.id (* to *) * 
                    kind_call_and_pos


   (* for the indirect case we need more than the id; we also need the 
    * expression involved to find back the place in the code where 
    * the pointed-function is called. We can not just use the name
    * of the function (otherwise it would have been a DirectCall).
    * 
    * update: we also need related information for Direct too because
    * we want to know all the instances of a function call.
    *)
   and kind_call_and_pos = 
     | Direct   of N.nameS Ast_php.wrap (* to have pos too, more consistent *)
     | MethodCall of Namespace_php.nameS Ast_php.wrap * call_extra_info
     | IndirectTodo (* of Ast.expression * call_extra_info  *)
     (* MethodCall !!! *)

     and call_extra_info = 
     {
       (* to store analysis information about the methodcall, whether 
        * it was same number of arguments, etc. Can be used to 
        * give a score to this callsite
        *)
       todo: unit;
     }

(* ---------------------------------------------------------------------- *)
(* old: before had DirectCall of string and string could correspond to
 * many functions because of cpp, but now for symetry and other pbs we
 * resolve the id. 
 * 
 * That also means that even if here we have a DirectCall of id, where
 * id is unique, in practice for a funcall we will generate multiple
 * DirectCall of id where each time the id is a function with the 
 * same name. In a way it's similar to what we do with IndirectFuncPtCall.
 * A single function pointer call instance can generate multiple
 * IndirectFuncPtCall as we can have multiple candidates for the callee.
 * 
 * todo: just store id, call_extra_info, and range_pos ? 
 * could do the same for DirectCall ? (could, to be symetrical, 
 * but this take space and can refound easily)
 * 
 * Removed and put back the callsite and callersite intermediate. Thought
 * that it was useless now that we have an  optimized version to keep
 * this intermediate type. But one advantage of having a callsite/callersite
 * type is that sometimes we want to get the callers, and 
 * without this type we will return a 'call list' that you will
 * have to interpret and use in the right direction. Here we can
 * return a callsite list.
 * But at least simplified it a lot by factoriwing the information
 * already in kind_call
 * 
 *)
type callsite = 
  CallSite of Entity_php.id * kind_call_and_pos


(* mean 'id' call you through a string (name of func) or an expression
 * which is a indirect function pointer call. 
 * 
 * old: before had DirectCaller of Entity.id * string but the string
 * is actually redundant with the name of the callee (the key of the
 * assoc in general.
 * 
 * Could we factorize the 2 types ? No, cos depending on the case
 * the id does not correspond to the same thing so it is useful to clearly
 * see when are talking about the caller or callee.
 * 
 * Was called callersite, but bad name, callerinfo is in fact more precise.
 * After all there is no notion of callersite; there is just one notion,
 * a call-site, then you may decide to talk more about the caller or callee.
 * 
 *)
type callerinfo = 
  CallerInfo of Entity_php.id * kind_call_and_pos


(* ---------------------------------------------------------------------- *)
(* 
 * Just put the information that could not be recomputed in a fast
 * way as it would require a whole program analyis (for instance 
 * all callers related information).
 *  - factorize
 *  - don't need to store information in DirectCallerIsOpt
 *    like  string * Ast.info list as this can be easily
 *    retrieved from its reversed table
 * 
 * TODO: optimize even more ? 
 * 
 * - don't need the string in DirectCallToOpt as this can be 
 *   retrieved from database.ml knowing the id quite fast  too. 
 * - don't need to store the candidates for DirectCallToOpt for the
 *   same reason (but of course must keep the id in IndirectFuncPtCallToOpt).
 * - use position_of_site instead of Ast_c.info or even full expression
 * 
 * Right now I have inlined some information in those types that could
 * be recomputed on demand in a fast way as we have all the needed
 * information in the database_c. But this is more convenient for now
 * to inline it. It enables the user to not have to access the db
 * and have it as a parameter to perform some algorithms.
 *)

type callsites_opt = 
  | DirectCallToOpt of 
      N.nameS * Ast_php.info list (* instances *) * 
        Entity_php.id list (* candidates *)
  (* for now we dont factorize method calls. could *)
  | MethodCallToOpt of 
      Entity_php.id * Namespace_php.nameS Ast_php.wrap * call_extra_info
  | IndirectFuncPtCallToOptTodo
      (* of E.id * Ast_c.expression * call_extra_info *)

(* Really useful to not repeat the string and instances in 
 * DirectCallerIsOpt and just put nbinstances. Also
 * good to have asymetry, just to see if can easily change the 
 * CallOpt data-structure, and balance the inlining vs on-demand lazily
 *
 * Normally, just have to optimize the Caller case because
 * this is this one that is often and randomly accessed.
 * Moreover some info can even be recomputed from its
 * reverse table such as the set of instances, so 
 * easy to optimize.
 * 
 * Note that I need to keep the nameS for DirectCallerIsOpt because 
 * some functions can have different names, and be called through different
 * names. Indeed A::foo() and B::foo() can be the same if B inherits
 * from A and does not redefine foo.
 * 
 *)

type callersinfo_opt = 
  | DirectCallerIsOpt of 
      Entity_php.id * Namespace_php.nameS * int (* nb instances *) 
  | MethodCallerIsOpt of 
      Entity_php.id * Namespace_php.nameS Ast_php.wrap * call_extra_info
  | IndirectFuncPtCallerIsOptTodo
      (* of E.id * Ast_c.expression * call_extra_info *)



(* ---------------------------------------------------------------------- *)
type analysis_confidence = int

(* can be used in a gui for the model of a treeview *)
type node = {
  name: string;
  id: Entity_php.id; (* can be interpreted as the caller or callee *)
  extra: call option; (* useful info to highlight call in gui for instance *)
  confidence: analysis_confidence;
  gray: bool;
}

type idtree = node Common.treeref
type calltree = node Common.treeref

type calltree_preferences = {
  squeeze_duplicate: bool; (* when want abstract about each instance call *)
  squeeze_duplicate_graph: bool; 
  filter_id: Entity_php.id -> bool; (* for instance to filter non-x86 entities *)
  filter_confidence: analysis_confidence -> bool;
  put_at_end_filtered_and_gray_ize: bool;
}

(*****************************************************************************)
(* Builders *)
(*****************************************************************************)
let default_calltree_preferences = 
  {
    squeeze_duplicate = true;
    squeeze_duplicate_graph = true;
    filter_id = (fun _ -> true);
    filter_confidence = (fun _ -> true);
    put_at_end_filtered_and_gray_ize = true;
  }

let default_call_extra_info = 
  {
    todo = ();
  }

(*****************************************************************************)
(* String of *)
(*****************************************************************************)

let s_of_kind_call = function
  | FunCall s -> s
  | ObjectCall (s1, s2) -> s1 ^ "->" ^ s2
  | ClassCall (s1, s2) -> s1 ^ "::" ^ s2


(*****************************************************************************)
(* Getters *)
(*****************************************************************************)

let id_of_callsite (CallSite (id, _)) = id
let id_of_callerinfo (CallerInfo (id, _)) = id

(* note: if have recursive call, then id1 can be equal to id2 and so the 
 * other_id is the same *)
let other_id_of_call id (Call (id1, id2, kind_call)) = 
  assert (id = id1 || id = id2);
  if id = id1 
  then id2 
  else id1

(*****************************************************************************)
(* Converters *)
(*****************************************************************************)

let (callsite_to_call: Entity_php.id -> callsite -> call) = 
 fun id (CallSite (id2, kind)) -> 
   Call (id, id2, kind)

let (callerinfo_to_call: callerinfo -> Entity_php.id -> call) = 
 fun (CallerInfo (id2, kind)) id -> 
   Call (id2, id, kind)

(* ---------------------------------------------------------------------- *)

let callsites_opt_to_callsites x = 
  match x with
  | DirectCallToOpt (s, instances, ids) -> 
      instances +> List.map (fun info -> 
        ids +> List.map (fun id -> 
          CallSite (id, Direct (s,info))
        )) +> List.flatten
  | MethodCallToOpt (id, e, info) ->
      [CallSite (id, MethodCall (e, info))]
  | IndirectFuncPtCallToOptTodo -> raise Todo (* (id, e, info) -> 
      [IndirectFuncPtCallTo (id, e, info)] *)




(* The callsites_opt type contains more information than the callersinfo_opt;
 * only the number of instances are stored in the callersinfo_opt. When 
 * the user wants to know what are those instances, we can lazily recompute
 * this information from the corresponding callsites_opt.
 *)
let find_caller_instances_with_name 
  idcallee name nbinstances callsites_opt_list = 

  let res = ref [] in
  callsites_opt_list +> List.iter (function
  | DirectCallToOpt (name2, instances, ids) -> 
      if List.mem idcallee ids && name = name2
      then begin
        assert (List.length instances = nbinstances);
        Common.push2 (name2, instances) res;
      end
  | MethodCallToOpt _ ->
      ()
  | IndirectFuncPtCallToOptTodo _ -> 
      ()
  );
  (* Can we have the same id in multiple things and List.length !res > 1 ?
   *
   * Well it can be in both direct and indirect but 
   * as we skip for now the indirect calls, this pb should not come.
   * 
   * Then normally it should not be in multiple Direct
   * because a function have only a single name and so 
   * its different instances should have been grouped.
   * 
   * But with static method calls, this situation can happen.
   * Both A::foo() and B::foo() can refer to the same function if
   * B inherits from A and does not define a foo(). Then if a function
   * bar() calls both A::foo() and B::foo(), the idcallee of A::foo() 
   * (which is the same than B::foo()) could be in two DirectCallToOpt.
   * That's why we now store in DirectCallerIsOpt not only the id
   * of the caller and the number of instances of the call, but also 
   * the name through which idcallee was called so there is no
   * ambiguity. Hence the   'name = name2' check above.
   *)
  Common.list_to_single_or_exn !res



let callersinfo_opt_to_callersinfo ~callees_of_id idcallee x =
  match x with
  | DirectCallerIsOpt (id, name, nbinstances) -> 
      let name, instances = 
        let xs = callees_of_id id in
        find_caller_instances_with_name idcallee name nbinstances xs
      in
      instances +> List.map (fun info -> 
        CallerInfo(id, Direct (name, info))
      )
  | MethodCallerIsOpt (id, e, xtra) ->
      [CallerInfo(id, MethodCall (e, xtra))]
      
  | IndirectFuncPtCallerIsOptTodo -> raise Todo (* (id, e, xtra) -> 
      [IndirectFuncPtCallerIs (id, e, xtra)] *)


(*****************************************************************************)
(* For caller/callees *)
(*****************************************************************************)

(* 
 * The functions here returns only local information, such as the
 * local callees. For a full caller/callee analysis then need to use
 * this local information and perform a global analysis. cf
 * database.ml, aliasing_function, and some other functions below.
 *
 * 
 * Just get regular callees, not the call through function pointer.
 * ex:
 *   int foo() {
 *   ...
 *   bar();
 *   }
 * will return "bar"
 * 
 * old: opti: use a hash, cos can have duplicate calls to the same func
 * let res = Hashtbl.create 13 in
 *  Hashtbl.replace res f true
 *  Common.hashset_to_list res
 * tagged as old: cos now we want to get all the instances
 * 
 *)
let callees_of_ast id_ast =
  let res = ref [] in

  let hooks = { Visitor_php.default_visitor with

    (* TODO if nested function ??? still wants to report ? *)
    Visitor_php.klvalue = (fun (k,vx) x ->
      match Ast_php.untype  x with
      | FunCallSimple (callname, args) ->

         (* note that as opposed to C, f cant be the name of a local var or
          * a function pointer because the namespace are different 
          * (T_STRING vs T_VARIABLE).
          * This variable name can not shadow an existing function 
          *)
          Common.push2 (N.name_to_nameS_wrap callname) res;
          k x
      | _ -> 
          k x
    );
  } 
  in
  let visitor = V2.mk_visitor hooks in
  visitor.V2.vid_ast id_ast;
  !res


let method_callees_of_ast idast = 
  V2.do_visit_with_ref (fun aref ->
    { V.default_visitor with
      V.klvalue = (fun (k,vx) x ->
        match Ast.untype  x with
        | MethodCallSimple (var, t1, methname, args) ->
            Common.push2 (N.name_to_nameS_wrap methname) aref;
            k x
        | _ -> 
            k x
      );
    })
    (fun visitor -> visitor.V2.vid_ast idast)

(* We pass the information for self:: and parent:: as extra arguments. We
 * could also in the caller or at parsing time replace all occurences
 * of self:: and parent:: by their corresponding value. This information
 * is computable statically. This is what HPHP does.
 *)
let static_method_callees_of_ast ~self ~parent idast = 
  V2.do_visit_with_ref (fun aref ->
    { V.default_visitor with
      V.klvalue = (fun (k,vx) x ->
        match Ast.untype  x with
        | StaticMethodCallSimple (qu, methname, args) ->
            let sclass = 
              match qu with
              | Qualifier (classname, info1) ->
                  Ast.name classname

              | Self (info_self, info1) ->
                 (match self with
                 | None ->
                     pr2 (spf "PB: Use of self:: outside of a class def, %s"
                                  (Ast.str_of_info info_self));
                    "TODO"
                 | Some sclass -> sclass
                 )
                      
              | Parent (info_parent, info1) ->
                 (match parent with
                 | None ->
                     pr2
                       (spf "PB: Use of parent:: but could not find parent, %s"
                                  (Ast.str_of_info info_parent));
                     "TODO"
                 | Some sclass -> sclass
                 )
            in
            (match methname with
            | Name (smeth, info2) ->
                let e = N.NameQualifiedS (sclass, smeth), info2 in
                Common.push2 e aref;
                k x
                
                  
            | XhpName _ -> 
                failwith "TODO: XhpName"
            )
        | _ -> 
            k x
      );
    })
    (fun visitor -> visitor.V2.vid_ast idast)


(*****************************************************************************)
(* Precision of analysis *)
(*****************************************************************************)
let no_info_confidence = 0

let confidence_of_call_extra_info ~fullid_info (id1x, id2x) x = 
  let score = ref no_info_confidence in

  let id1 = fullid_info id1x in
  let id2 = fullid_info id2x in

(*
  if x.use_same_struct_field 
  then score += 13;
*)

  (if id1.E.file = id2.E.file 
  then score += 10
  else 
      if Common.dirname id1.E.file = 
         Common.dirname id2.E.file 
      then score += 5
  );
(*
  if x.use_type_of_arguments 
  then score -= 20;
*)

  !score


(* When have multiple possible defs with same name that
 * are called, favor the one in same file or same dir.
 * So have also heuristic for DirectCall.
 * If only 1 candidate then give it 100. If multiple then
 * give less, and big score for callee in same file
 *)
let confidence_of_directcall_when_multi ~fullid_info (id1x, id2x) nbbrothers = 
  
  let id1 = fullid_info id1x in
  let id2 = fullid_info id2x in

  if id1.E.file = id2.E.file 
  then 100
  else 
    (if (Common.dirname id1.E.file = 
        Common.dirname id2.E.file 
       )
    then 50
    else 30) / nbbrothers

(* must take whole list, so can do the regrouping and see list of stuff *)
let confidence_of_related_calls ~fullid_info (kind_call, calls) = 
  (* todo: assert indeed related calls, they should have something in common *)
  match kind_call with
  | Direct _ -> 
      (match calls with
      | [] -> raise Impossible
      | [x] -> [100]
      | x::y::xs -> 
          calls +> List.map (fun (Call (id1,id2,kind)) -> 
            confidence_of_directcall_when_multi ~fullid_info (id1, id2) 
              (List.length (x::y::xs))
          )
      )
  | MethodCall (name, xtra) ->
      calls +> List.map (fun (Call (id1, id2, kind)) ->
        confidence_of_call_extra_info ~fullid_info (id1, id2) xtra
      )
  | IndirectTodo -> 
      raise Todo
(*
      calls +> List.map (fun (Call (id1,id2,kind)) -> 
        match kind with
        | Direct _ -> raise Impossible (* group_related_calls_xxx bug *)
        | Indirect (e,xtra) -> 
            confidence_of_call_extra_info ~fullid_info (id1, id2) xtra
      )
*)


let is_good_confidence (Call (id1,id2, kind)) confidence = 
  match kind with
  | Direct _ -> 
      confidence >= 20
  | MethodCall _ ->
      confidence > 10 
  | IndirectTodo _ -> 
      confidence > 10


(*****************************************************************************)
(* Grouping of calls *)
(*****************************************************************************)

(* Do the reverse job that was done in add_callees that
 * were flattening things (flattening but still keeping
 * enough information to do reverse by storing the
 * string wrap, and the call_extra_info for indirect call
 * that allow us to know what approximation were made for
 * this indirect funcall).
 * 
 * Just a different view on callersite and callerinfo list,
 * Add one more level with grouping
 * (can even do one more if regroup directcall that have
 * all same name (or indirect using same expression (after
 * abstract_line)).
 * 
 * todo: now that has the DirectCallOpt structure, this code
 * could be more direct. Right now we start from DirectCallOpt in
 * bdb table, then transform it in a DirectCall, and here in some way
 * we reconvert in DirectCallOpt. So remove intermediate step.
 *)
let (group_related_calls_same_instance_different_targets: 
 call list -> (kind_call_and_pos * call list) list) = fun xs ->
  Common.group_by_mapped_key (fun (Call (_,_,kind_call)) -> kind_call) xs

let (group_related_calls_same_target_different_instances:
  Entity_php.id -> call list -> (Entity_php.id * call list) list) = fun id xs -> 
  Common.group_by_mapped_key (fun call -> other_id_of_call id call) xs




(*****************************************************************************)
(* Main entry *)
(*****************************************************************************)

(* help to factorize the code between the 2 functions below *)

let get_direct_children_scored_and_sorted_and_with_kindcall_and_more 
  (idparent, parent_is_gray) (xs: call list)
  ~namefunc ~fullid_info
  ~preferences 
  
 = 

  let xs = 
    if preferences.squeeze_duplicate 
    then
      let grouped = 
        group_related_calls_same_target_different_instances idparent xs in
      grouped +> List.map (fun (id, calls) -> 
        List.hd calls (* just take one *)
      )
    else xs 
  in
      

  let grouped = group_related_calls_same_instance_different_targets xs in

  let scored_calls = 
    grouped +> List.map (fun (kind, calls) -> 
      let scores = confidence_of_related_calls ~fullid_info (kind, calls) in
      Common.zip calls scores
    ) +> List.flatten in

  let ys = 
    scored_calls +> List.map (fun (call, score) -> 
      let id = other_id_of_call idparent call in
      let gray = 
        (preferences.put_at_end_filtered_and_gray_ize && 
        not (is_good_confidence call score))
        || parent_is_gray
      in

      call,
      namefunc id, 
      score,
      gray
    )
   (* how to sort ? would like to group based on dir, or file, but at 
    * the same time sometimes convenient to see id with same name
    * together and in alphabetic order. So better to sort based on name,
    * and quite often this will also sort by file as name of entity often
    * contains a prefix of the name of the file.
    * 
    * For callees could also sort based on position, so that the first
    * called are listed first.
    *)

    +> List.sort (fun (_,namea, scorea,_) (_,nameb, scoreb,_) -> 
      compare scoreb scorea
    )
  in
  let direct_children = 
    ys +> List.map (fun ((Call (_,_,kind) as call), name, confidence, gray) -> 
      let id = other_id_of_call idparent call in
      id, (idparent, kind, confidence, gray)
    )
  in
  if List.length direct_children > 100
  then begin
    (* I return empty list cos this would make the gui gtk list widget
     * too slow anyway to display all this list.
     *)
    pr2 "PB: probably imprecise function pointer analysis:";
    pr2 ("PB: too many children: " ^ (namefunc idparent));
    Common.take_safe 100 direct_children
    end
  else direct_children


(* ---------------------------------------------------------------------- *)
(* ex: 
 *   int foo() {
 *    ...
 *    x.fnopen();
 *   }
 *   int bar() {
 *    ...
 *    foo_open();
 *   }
 *   int foo_open() { ... }
 * 
 * Here wants to know callers of foo_open, so 
 * id = 'foo_open', and callers are 'foo' through a function pointer fnopen();
 * and also 'bar' through a regular direct call.
 * So want return 3 nodes, one with foo_open, and one with foo where
 * know that what you really want to search is not the call to foo_open but
 * the expression x.fnopen(); and the one from bar().
 * When we will recurse to the id of foo itself, we will build the node
 * for 'foo' with as extra information the x.fnopen() call corresponding
 * to foo_open.
 *)

let rec calltree_callers_of_f id
    ~depth ~parent_and_extra_opt   ~namefunc ~callersfunc ~fullid_info
    ~preferences
    
 = 
  let name = namefunc id in
  
  let callopt, confidence, gray = 
    match parent_and_extra_opt with 
    | None -> 
        None, 0, false 
    | Some (idparent, kind_call, confidence, gray) -> 
        (* not idparent, id,  because we start from the callee so 
         * the parent is not the caller but the callee.
         * Maybe I should use a better word than parentid for this param ...
         *)
        Some (Call (id, idparent, kind_call)), confidence, gray
  in

  let node = {
    name = name;
    id = id;
    extra = callopt;
    confidence = confidence;
    gray = gray;
    (* extrabis = "";
     * TODO: put string_of_extra_id_info_db here, not in view.ml? 
     * match_result = None;
     *)
  }
  in

  if depth = 0
  then NodeRef (node, ref [])
  else 
    (* old: why strid and not directly id ? cos callersfunc will be built
     * from db.callers_of_f and take a string not an id.
     * update: now callersfunc take an id.
     * 
     * callersfunc =~ 
     *  db.callers_of_f#assoc (IdFuncPos (strid +> string_of_id_string, id))
     * 
     *)
    let (callers: callerinfo list) = 
      try callersfunc id
      with Not_found -> []
    in
    let calls = callers +> List.map (fun callerinfo -> 
      callerinfo_to_call callerinfo id
    ) in
    let direct_children = 
      get_direct_children_scored_and_sorted_and_with_kindcall_and_more 
        (id, gray) calls ~namefunc ~fullid_info   ~preferences 
    in
    let children = 
      direct_children +> List.map (fun (idcaller, parent_and_extra) -> 
        (* recurse *)
        calltree_callers_of_f 
          idcaller
          ~depth:(depth -1)  
          ~parent_and_extra_opt:(Some parent_and_extra)
         ~namefunc ~callersfunc ~fullid_info
         ~preferences 
      )
    in
    NodeRef(node, ref children)


(* ---------------------------------------------------------------------- *)
(* ex: 
 *   int foo() {
 *    ...
 *    bar();
 *    ...
 *    x.fnopen();
 *   }
 *   int bar() {  ... }
 *   int foo_open() { ... }
 * 
 * Here wants to know callees of foo, so 
 * id = 'foo', and callees are 'foo_open' through a function pointer fnopen();
 * and also 'bar' through a regular direct call.
 * So want return 3 nodes, one with foo, and one with foo_open where
 * know that what you really want to search is not the call to foo_open but
 * the expression x.fnopen(); and the one with bar().

 * ??When we will recurse to the id of foo_open itself, we will build the node
 * ??for 'foo_open'' with as extra information the x.fnopen() call corresponding
 * ??to foo_open.
 * 
 * quite similar to previous function
 *)

let rec calltree_callees_of_f id 
  ~depth  ~parent_and_extra_opt  ~namefunc ~calleesfunc ~fullid_info
  ~preferences
 =

  let name = namefunc id in

  let callopt, confidence, gray = 
    match parent_and_extra_opt with 
    | None -> 
        None, 0, false 
    | Some (idparent, kind_call, confidence, gray) -> 
        (* idparent, id, here  because we start from the caller so 
         * the parent is the caller.
         *)
        Some (Call (idparent, id, kind_call)), confidence, gray
  in

  let node = {
    name = name;
    id = id;
    extra = callopt;
    confidence = confidence;
    gray = gray;
    (*
      extrabis = "";

      match_result = None;
    *)
  }
  in

  if depth = 0
  then NodeRef (node, ref [])
  else 
    let (callees: callsite list) = 
      try calleesfunc id
      with Not_found -> []
    in
    let calls = callees +> List.map (fun callsite -> 
      callsite_to_call id callsite
    ) in
    let direct_children = 
      get_direct_children_scored_and_sorted_and_with_kindcall_and_more 
        (id, gray) calls ~namefunc ~fullid_info    ~preferences
    in
    let children = 
      direct_children +> List.map (fun (idcallee, parent_and_extra) -> 
        (* recurse *)
        calltree_callees_of_f 
          idcallee
          ~depth:(depth -1)  
          ~parent_and_extra_opt:(Some parent_and_extra)
          ~namefunc ~calleesfunc ~fullid_info
          ~preferences 
      )
    in
    NodeRef(node, ref children)
