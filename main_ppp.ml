open Common

open Ast_php
open Parse_info

module Ast = Ast_php
module V = Visitor_php

module S = Scope_code


(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)

(* 
 * Closure implementation via souce-to-source transformation to classes.
 * 
 * TODO should be moved in demos/ maybe later, or in phpext/ ? 
 * and LP-ized and shown as a tutorial on pfff and source-to-source
 * transformation.
 * 
 * history: 
 *  - came up with simple source-to-source transformation idea,
 *    see tests/proto/closure_transfo.php, but that would have
 *    required small support from runtime
 *  - sgrimm proposed a simple trick, using call_user_func
 *    to remove the need from runtime support
 *  - implemented first proto
 *  - erling broke my proto when using nested lambdas, 
 *    see erling_sk.php
 *  - implemented second proto
 *  - drew broke second proto when using dynamic vars ($$x), 
 *    see drew_foo3.php
 *  - implemented third proto using trick proposed by iain
 *    so that no need to replace the $x or $$x by some $this->x 
 *    or ${$this-x} that would not have worked anyway.
 *  - use my tool on my own php code to do some with_open, iter, 
 *    and turns out we need to pass stuff by reference in constructor.
 * 
 * Was it useful ? After all iproctor will probably implement closures
 * in HPHP in a more direct way. But it was the occasion to
 * force me to implement a basic pretty printer, map_php, fix bugs
 * in the visitor, and it can be a good basis for 
 * a tutorial on code transfo using pfff :)
 * Moreover maybe they will use it. Without that, may have to wait 
 * months before iproctor implements it ... Maybe this will put some
 * pressure on iproctor :)
 * Moreover, for php projects like phpunit, who wants to support legacy
 * PHP code, they can use internally closures but
 * output still code compliant with php 5.2 :)
 * 
 * futur work:
 *  - transform our code to use that :) and see how more beautiful it is
 *  - bench ?
 *
 * I could have used a template like below instead of manually
 * constructing the AST and use a camlp4 quasiquotation, or 
 * camlmix system. But the code I generate is small so it's ok. 
 * Moreover it shows* how to do things manually :)
 *)
let template_class = "
class _anon_closure_12 extends Closure {
  private $x;
  function __construct($a) {
    $this->x = $a;
  }

  function eval_closure($y) {
    return $this->x + $y;
  }
}
"

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

let verbose = ref true
let pr2 s = if !verbose then Common.pr2 s else ()

(* action mode *)
let action = ref ""

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let fkt str = { Parse_info.
  token = Parse_info.FakeTokStr (str, None);
  transfo = Parse_info.NoTransfo;
  comments = ();
}
let fkdname s = 
  DName (s, fkt ("$" ^ s))


(* We must generate a unique name that will not conflict with other classes,
 * and also with other generated anonymous classes. Can still just
 * use the name of the file and its file position to have a quite
 * good unique name.
 *)
let gensym info = 
  (* less: for now just use line and col info, later should also use filename 
   * could simply do a md5sum on the all file:line:col string
   *)
  let (l,c) = Ast.line_of_info info, Ast.col_of_info info in
  spf "anon_class_closure_l%d_c%d" l c



(*****************************************************************************)
(* Ast builders *)
(*****************************************************************************)


let mk_method s params body =
  Method ({
    f_tok = fkt "function";
    f_ref = None;
    f_name = Name (s, fkt s);
    f_params = (fkt "(", params, fkt ")");
    f_body = (fkt "{", body, fkt "}");
    f_return_type = None;
    f_type = MethodRegular;
    f_modifiers = [];
  })

let mk_param s = { 
  p_type = None;
  p_ref = Some (fkt "&"); (* we want passed by ref *)
  p_name = fkdname s;
  p_default = None;
}
  

let mk_obj_access s = 
  (ObjAccessSimple
      ((Var
           (DName ("this", fkt "$this"),
           Ast.noScope())
       ),
      fkt "->",
      Name (s, fkt s)))

(* use sgrimm technique 
 * TODO may have to prefix certain closed_vars with this because
 * when we have nested lambda, we are called recursively to
 * generate also the body of the eval_closure method, which 
 * now have itself some closed var and private vars that 
 * when mentionned in the use ($x, ...) of nested lambdas
 * should be transformed to call to the 'new' anon_class
 * but with as parameters some ($this->x)
*)
let mk_new_anon_class_call s 
  private_vars_enclosing_closure_class 
  closed_vars_of_this_closure = 
 (ArrayLong
  (fkt "array",
  (fkt "(",
   [Left (ArrayExpr
     (New
       (fkt "new",
       ClassNameRefStatic
        (ClassName(Name 
          (s,
           fkt s))),
       Some
        (fkt "(",
         closed_vars_of_this_closure +> List.map (fun closed_var ->

           let var =
             if List.mem closed_var private_vars_enclosing_closure_class
             then mk_obj_access closed_var
             else
               (Var
                   (fkdname closed_var,
                   {contents = S.NoScope})
               )
           in        
           (* todo: should introduce some comma with Right too *)
           Left (Arg (Lv var))
         )
         ,
        fkt ")"
         ))
      ));
    Right (fkt ",");
    Left (ArrayExpr
     (Sc
       (C
         (String
           ("eval_closure",
           fkt "'eval_closure'"
            )))
      ))],
   fkt ")"))
 )



let mk_private_affect s = 
  let expr = 
    (AssignRef
        ((ObjAccessSimple
             ((Var
                  (DName ("this", fkt "$this"),
                  Ast.noScope())
              ),
             fkt "->",
             Name (s, fkt s))
         ),
        fkt "=",
        fkt "&", (* want assign by ref *)
        (
            (Var
                (fkdname s,
                Ast.noScope ())
            )
        ))
    )
  in
  Stmt (ExprStmt (expr, fkt ";"))

let mk_call_user_func_call var args_paren =
  let (op, args, cp) = args_paren in
  let arg1 = Arg (Lv var) in
  let args' = (Left arg1)::args in
  let str = "call_user_func" in
  FunCallSimple ((Name (str, fkt str)), (op, args', cp))



let mk_aliasing_for_member_in_body () = 
[Stmt (Foreach
      (fkt "foreach",
      fkt "(",
      (Lv
        (Var
          (DName
            ("this",
             fkt "$this"),
          {contents = S.NoScope})
         )
       ),
      fkt "as",
      Common.Left
       (None,
        (Var
          (DName
            ("p",
             fkt "$p"),
          {contents = S.NoScope})
         )),
      Some
       (fkt "=>",
        (Some
          (fkt "&"),
         (Var
           (DName
             ("v",
              fkt "$v"),
           {contents = S.NoScope})
          ))),
      fkt ")",
      SingleStmt
       (Block
         (fkt "{",
          [Stmt
            (ExprStmt
              ((AssignRef
                 ((Indirect
                    ((Var
                       (DName
                         ("p",
                          fkt "$p"),
                       {contents = S.NoScope})
                      ),
                    Dollar
                     (fkt "$"))
                   ),
                 fkt "=",
                 fkt "&",
                 (Var
                   (DName
                     ("v",
                      fkt "$v"),
                   {contents = S.NoScope})
                  ))
                ),
              fkt ";"))],
          fkt "}")))
)
]


(*****************************************************************************)
(* Main parts *)
(*****************************************************************************)

(* replace every occurence of $x with $this->x when x is part of the
 * closed environment.
 *
 * was called mk_body_eval_closure, but with nested lambdas
 * we can not in one pass generate the full body of eval_closure.
 * It has to be a recursive process done by the caller of this func.
 * So this func really just do a simple job of substituting the right
 * variables by adding the this prefix.
 * 
 * update: turns out this scheme does not work well when people
 * use dynamic var as in $$x. See tests/closures/drew_foo3.php.
 * The pb is that what was a local var before, and so referenced as
 * $bar = 'foo'; and later naively as ${$this->bar}}
*)
let (add_this_to_closed_var_in_body: 
     string list ->  
     stmt_and_def list ->
     stmt_and_def list
  ) = fun closed_vars xs ->
  (* visit original code and replace. 
   * TODO take care of scope ? shadowing ? 
   *)
  xs +> List.map (fun stmt_and_def ->
    let hook = { Map_php.default_visitor with
      Map_php.klvalue = (fun (k, _) v ->

        match v with
        | Var (v1, v2) -> 
            let s = Ast.dname v1 in
            if List.mem s closed_vars 
            then mk_obj_access s
            else k v
        | _ -> k v
      );
      (* bugfix: and dont go inslide nested lambdas *)
      Map_php.kexpr = (fun (k, _) v ->
        match v with
        | Lambda def -> 
            (* no recursive processing *)
            v
        | _ -> k v
      );
    }
    in
    (Map_php.mk_visitor hook).Map_php.vstmt_and_def stmt_and_def
  )


let closed_vars (l_use, l) = 
  match l_use with
  | None -> []
  | Some (_use, vars) -> 
      Ast.unparen vars +> Ast.uncomma +> List.map (function
      | LexicalVar (is_ref, dname) ->
          if is_ref = None 
          then Ast.dname dname
          else failwith "not handling ref in closures"
      )
  

  
(* This function was previously generating the sym, and the body
 * of eval_closure, but as said previously, it has to be a recursive
 * process, so now this function really just do boilerplate stuff:
 * add the remaining class, __construct, and the eval_closure method
 * header.
 *)
let (mk_anon_class: 
   string (* name of gensymed class *) ->
   Ast.lambda_def -> Ast.stmt_and_def list -> Ast.class_def) 
 = fun anon_str l body_eval_closure ->


  let closed_vars = closed_vars l in

  (* what if lambda contains a lambda ? 
   * old:
   * let body_eval_closure = 
   * mk_body_eval_closure 
   * closed_vars 
   * (Ast.unbrace l.l_body)
   * in
   *)
  let params_eval_closure = 
    Ast.unparen (snd l).f_params
  in

  let privates = 
    closed_vars +> List.map (fun s ->
      ClassVariables (
        VModifiers [Private, fkt "private"],
        None,
        [Left (fkdname s, None)],
        fkt ";"
      )
    )
  in
  let construct = 
    let s = "__construct" in
    let params = closed_vars +> List.map mk_param 
      (* todo: introduce commas *)
      +> List.map (fun p -> Left3 p)
    in
    let constr_body = closed_vars +> List.map (fun s ->
      (* via using expr_of_string "$this->x = $x" in toplevel *)
      mk_private_affect s
    )
    in
    mk_method s params constr_body
  in
  let eval_closure = 
    let s = "eval_closure" in
    let params = params_eval_closure in
    mk_method s params body_eval_closure
  in
  let body = 
    privates ++ [construct] ++ [eval_closure]
  in

  let aclass = {
    c_type = ClassRegular (fkt "class");
    c_name = Name (anon_str, fkt anon_str);
    c_extends = None;
      (* Some (fkt "extends", Name ("Closure", fkt "Closure")); *)
    c_implements = None;
    c_body = 
      fkt "{", body, fkt "}";
  }
  in
  aclass



(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

let (all_classes: class_def list ref) = ref [] 

let rec (transfo: string list -> stmt_and_def list -> stmt_and_def list) =
 fun current_closed_vars_when_in_eval_closure    stmts ->

   let hook = { Map_php.default_visitor with
     Map_php.kexpr = (fun (k, _) v ->
       match v with
       | Lambda (l_use, ldef) -> 
           let info = ldef.f_tok in
           
           let anon_str = gensym info in
           let closed_vars = closed_vars (l_use, ldef) in

           let res = 
             mk_new_anon_class_call anon_str
               current_closed_vars_when_in_eval_closure 
               closed_vars
           in

           let body_closure = Ast.unbrace ldef.f_body in
           
           (* this will not do the subst inside the possible nested
            * lambdas 
            *)
           let body' = 
             (* old: add_this_to_closed_var_in_body closed_vars body_closure 
              * this was not working well when using dynamic vars.
              * Thx to drew and ian for better proposal.
              *)
             body_closure
           in
           let body'' = 
             transfo closed_vars body'
           in
           let body'' =
             mk_aliasing_for_member_in_body () ++ body''
           in
           
           let anon_class = 
             mk_anon_class anon_str (l_use, ldef) body''
           in
           Common.push2 anon_class all_classes;

           (* finally just return the 'new anon_class($y, $this->x)' *)
           res
      | _ -> k v
     );
     Map_php.klvalue = (fun (k, _) v ->
      (* apply recursively first *)
      let res = k v in
      match res with
      | FunCallVar (qu_opt, var, args) ->

          (* do we care wether var is a regular var ? 
           * do something with qu_opt ?
           *)
          mk_call_user_func_call var args
      | _ -> res
     );
   }
   in
   let visitor = Map_php.mk_visitor hook in
   stmts +> List.map visitor.Map_php.vstmt_and_def



let main_action file = 
  let (ast2, _stat) = Parse_php.parse file in
  let ast = Parse_php.program_of_program2 ast2 in

  let hook = { Map_php.default_visitor with
    Map_php.kstmt_and_def = (fun (k, _) stmt ->
      let res =  transfo [] [stmt] in
      match res with
      | [stmt'] -> stmt'
      | _ -> raise Impossible
    );
    (* this is only for the toplevel elements which are 
     * a StmtList, not a stmt_and_def, but we still need
     * to transform them
     *)
    Map_php.kstmt = (fun (k,_) stmt ->
      let res = transfo [] [Stmt stmt] in
      match res with
      | [Stmt stmt'] -> stmt'
      | _ -> raise Impossible
    );
  }
  in
  
  let ast' = (Map_php.mk_visitor hook).Map_php.vprogram ast in

  let ast2' = Common.zip ast' (ast2 +> List.map snd) in
  let s = Unparse_php.string_of_program2 ast2' in
  pr s;

  !all_classes +> List.rev +> List.iter (fun (classdef) ->
    let top = ClassDef classdef in
    let s = Unparse_php.string_of_any (Toplevel top) in
    pr s;
  );
  ()
  
(*****************************************************************************)
(* Extra actions *)
(*****************************************************************************)

let unparse_without_type_hints file =

  (* steps:
   *  - parse with enabling the type hints grammar extension
   *  - visit ast and annotate type hints tokens with Remove
   *  - unparse ast using the tokens annotations
   *)

  Flag_parsing_php.type_hints_extension := true;

  let (ast2, _stat) = Parse_php.parse file in
  let ast = Parse_php.program_of_program2 ast2 in

  (* visit ast and annotate type hints *)
  let annotate_type_hint_tokens_as_remove type_hint =
    let token = 
      match type_hint with
      | Hint (ClassName name) -> Ast.info_of_name name
      | Hint (Self tok | Parent tok) -> tok
      | HintArray tok -> tok
      | Hint (LateStatic tok) -> raise Impossible
    in
    token.Parse_info.transfo <- Remove;
  in
  let v = V.mk_visitor { V.default_visitor with
    (* todo? we could keep some typehint such as Object or Array,
     * at certain places like in function parameters as they are already
     * accepted by vanilla PHP *)
    V.khint_type = (fun (k, _) ty ->
      annotate_type_hint_tokens_as_remove ty
    );
  }
  in
  (* modifying ast will also modify ast2 by side effect *)
  v (Program ast);

  (* this unparser knows about the Remove token annotation *)
  let s = Unparse_php.string_of_program2_using_transfo ast2 in
  pr s


(*---------------------------------------------------------------------------*)
(* XHP preprocessor and static optimizer *)
(*---------------------------------------------------------------------------*)

let xhp_preprocesor file =
  let _ast = Parse_php.parse_program file in
  raise Todo

(* I have some ideas on how we can make XHP faster via static analysis
 * in the rewriter, but I don't have time to work on it. It would probably
 * take a while to build since I think you would need to rewrite much
 * of the parser.
 * 
 * The idea is that given code like:
 * 
 * $foo = <div><span><a href={$href}>hello</a></span></div>;
 * ...
 * Currently we rewrite it to:
 * 
 * $foo=new xhp_div(array(), array(new xhp_span(array(), array(new xhp_a(array('href' => $this,), array('hello'))))));
 * 
 * But we could rewrite it to this instead:
 * 
 * $foo=new xhp_div(array(), array(HTML('<span><a href='.txt2html($href).'>hello</a></span>')));
 * 
 * You can't convert the outside div to a string, but you can convert 
 * the inside nodes. Since we mostly enforce that you can only append 
 * to XHP nodes this would be safe. If we have an intern or noob or 
 * someone who's interested in parsers and wants to work on XHP this 
 * might be a good project.
 *)

let xhp_optimizer file =
  let _ast = Parse_php.parse_program file in
  raise Todo

(*---------------------------------------------------------------------------*)
(* Extra actions *)
(*---------------------------------------------------------------------------*)

let extra_actions () = [
  "-unparse_without_type_hints", " <file>",
  Common.mk_action_1_arg (unparse_without_type_hints);

]

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () = 
 extra_actions () ++
 Test_parsing_php.actions()++
 []

let options () = 
  [
    "-verbose", Arg.Set verbose, 
    " ";
  ] ++
  Flag_parsing_php.cmdline_flags_pp () ++
  Common.options_of_actions action (all_actions()) ++
  Common.cmdline_flags_devel () ++
  Common.cmdline_flags_verbose () ++
  Common.cmdline_flags_other () ++
  [
  "-version",   Arg.Unit (fun () -> 
    Common.pr2 (spf "XXX version: %s" Config.version);
    exit 0;
  ), 
    "  guess what";

  (* this can not be factorized in Common *)
  "-date",   Arg.Unit (fun () -> 
    Common.pr2 "version: $Date: 2008/10/26 00:44:57 $";
    raise (Common.UnixExit 0)
    ), 
  "   guess what";
  ] ++
  []

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main () = 
  let usage_msg = 
    "Usage: " ^ Common.basename Sys.argv.(0) ^ 
      " [options] <file or dir> " ^ "\n" ^ "Options are:"
  in
  (* does side effect on many global flags *)
  let args = Common.parse_options (options()) usage_msg Sys.argv in

  (* must be done after Arg.parse, because Common.profile is set by it *)
  Common.profile_code "Main total" (fun () -> 

    (match args with
   
    (* --------------------------------------------------------- *)
    (* actions, useful to debug subpart *)
    (* --------------------------------------------------------- *)
    | xs when List.mem !action (Common.action_list (all_actions())) -> 
        Common.do_action !action xs (all_actions())

    | _ when not (Common.null_string !action) -> 
        failwith ("unrecognized action or wrong params: " ^ !action)

    (* --------------------------------------------------------- *)
    (* main entry *)
    (* --------------------------------------------------------- *)
    | [x] -> 
        main_action x

    (* --------------------------------------------------------- *)
    (* empty entry *)
    (* --------------------------------------------------------- *)
    | [] -> 
        Common.usage usage_msg (options()); 
        failwith "too few arguments"
    | x::y::zs -> 
        Common.usage usage_msg (options()); 
        failwith "too many arguments"
    )
  )



(*****************************************************************************)
let _ =
  Common.main_boilerplate (fun () -> 
      main ();
  )
