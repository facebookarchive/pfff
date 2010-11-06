(*s: ast_php.mli *)
open Common

(*****************************************************************************)
(* The AST related types *)
(*****************************************************************************)
(* ------------------------------------------------------------------------- *)
(* Token/info *)
(* ------------------------------------------------------------------------- *)
(*s: AST info *)
(*s: type pinfo *)
type pinfo = Parse_info.token
  (*s: pinfo constructors *)
  (*x: pinfo constructors *)
  (*x: pinfo constructors *)
  (*e: pinfo constructors *)
  (*s: tarzan annotation *)
   (* with tarzan *)
  (*e: tarzan annotation *)
(*e: type pinfo *)

type info = { 
  (* contains among other things the position of the token through
   * the Common.parse_info embedded inside the pinfo type.
   *)
  mutable pinfo : pinfo; 
  (*s: type info hook *)
  (*TODO*)
  comments: unit;
  (*x: type info hook *)
  mutable transfo: transformation;
  (*e: type info hook *)
  }
and tok = info
(*x: AST info *)
(* a shortcut to annotate some information with token/position information *)
and 'a wrap = 'a * info
(*x: AST info *)
and 'a paren   = tok * 'a * tok
and 'a brace   = tok * 'a * tok
and 'a bracket = tok * 'a * tok 
and 'a comma_list = ('a, tok (* the comma *)) Common.either list
(*x: AST info *)
and transformation = 
  | NoTransfo
  | Remove 
  | AddBefore of add
  | AddAfter of add
  | Replace of add
  and add = 
    | AddStr of string
    | AddNewlineAndIdent
 (*s: tarzan annotation *)
  (* with tarzan *)
 (*e: tarzan annotation *)
(*e: AST info *)
(* ------------------------------------------------------------------------- *)
(* Name *)
(* ------------------------------------------------------------------------- *)
(*s: AST name *)
 (*s: type name *)
 (* T_STRING, which are really just LABEL, see the lexer. *)
 type name = 
    | Name of string wrap 
    (*s: type name hook *)
    (* xhp: for :x:foo the list is ["x";"foo"] *)
    | XhpName of xhp_tag wrap
    (*e: type name hook *)
 (*e: type name *)
 (* for :x:foo the list is ["x";"foo"] *)
 and xhp_tag = string list

 (*s: type dname *)
 (* D for dollar. Was called T_VARIABLE in the original PHP parser/lexer.
  * The string does not contain the '$'. The info itself will usually 
  * contain it, but not always! Indeed if the variable we build comes 
  * from an encapsulated strings as in  echo "${x[foo]}" then the 'x' 
  * will be parsed as a T_STRING_VARNAME, and eventually lead to a DName, 
  * even if in the text it appears as a name.
  * So this token is kind of a FakeTok sometimes.
  * 
  * So if at some point you want to do some program transformation,
  * you may have to normalize this string wrap before moving it
  * in another context !!!
  *)
 and dname = 
   | DName of string wrap 
 (*e: type dname *)

 (*s: qualifiers *)
 and qualifier = 
   | Qualifier of fully_qualified_class_name * tok (* :: *)
   (* Could also transform at parsing time all occurences of self:: and
    * parent:: by their respective names. But I prefer to have all the 
    * PHP features somehow explicitely represented in the AST.
    *)
   | Self   of tok * tok (* :: *)
   | Parent of tok * tok (* :: *)

 and fully_qualified_class_name = name
 (*e: qualifiers *)

 (*s: tarzan annotation *)
  (* with tarzan *)
 (*e: tarzan annotation *)
(*e: AST name *)
(* ------------------------------------------------------------------------- *)
(* Type *)
(* ------------------------------------------------------------------------- *)
(*s: AST type *)
type ptype =
  | BoolTy 
  | IntTy 
  | DoubleTy (* float *)

  | StringTy 

  | ArrayTy 
  | ObjectTy

 (*s: tarzan annotation *)
  (* with tarzan *)
 (*e: tarzan annotation *)
(*e: AST type *)
(* ------------------------------------------------------------------------- *)
(* Expression *)
(* ------------------------------------------------------------------------- *)
(*s: AST expression *)
type expr = exprbis * exp_info
  (*s: type exp_info *)
  (* semantic: *)
  and exp_info = { 
     mutable t: Type_php.phptype;
  }
  (*e: type exp_info *)
  and exprbis =
  | Lv of lvalue

  (* start of expr_without_variable in original PHP lexer/parser terminology *)
  | Sc of scalar

  (*s: exprbis other constructors *)
  | Binary  of expr * binaryOp wrap * expr
  | Unary   of unaryOp wrap * expr
  (*x: exprbis other constructors *)
  (* should be a statement ... *)
  | Assign    of lvalue * tok (* = *) * expr
  | AssignOp  of lvalue * assignOp wrap * expr
  | Postfix of rw_variable   * fixOp wrap
  | Infix   of fixOp wrap    * rw_variable
  (*x: exprbis other constructors *)
  | CondExpr of expr * tok (* ? *) * expr * tok (* : *) * expr
  (*x: exprbis other constructors *)
  | AssignList  of tok (* list *)  * list_assign comma_list paren * 
        tok (* = *) * expr
  | ConsArray of tok (* array *) * array_pair  comma_list paren
  (*x: exprbis other constructors *)
  | New of tok * class_name_reference * argument comma_list paren option
  | Clone of tok * expr
  (*x: exprbis other constructors *)
  | AssignRef of lvalue * tok (* = *) * tok (* & *) * lvalue
  | AssignNew of lvalue * tok (* = *) * tok (* & *) * tok (* new *) * 
        class_name_reference * 
        argument comma_list paren option
  (*x: exprbis other constructors *)
  | Cast of castOp wrap * expr 
  | CastUnset of tok * expr (* ??? *)
  (*x: exprbis other constructors *)
  | InstanceOf of expr * tok * class_name_reference
  (*x: exprbis other constructors *)
  (* !The evil eval! *)
  | Eval of tok * expr paren
  (*x: exprbis other constructors *)
  (* Woohoo, viva PHP 5.3 *)
  | Lambda of lambda_def
  (*x: exprbis other constructors *)
  (* should be a statement ... *)
  | Exit of tok * (expr option paren) option
  | At of tok (* @ *) * expr
  | Print of tok * expr 
  (*x: exprbis other constructors *)
  | BackQuote of tok * encaps list * tok
  (*x: exprbis other constructors *)
  (* should be at toplevel *)
  | Include     of tok * expr
  | IncludeOnce of tok * expr
  | Require     of tok * expr
  | RequireOnce of tok * expr
  (*x: exprbis other constructors *)
  | Empty of tok * lvalue paren
  | Isset of tok * lvalue comma_list paren
  (*e: exprbis other constructors *)

  | XhpHtml of xhp_html

  (*s: type exprbis hook *)
  | EDots of info
  (*x: type exprbis hook *)
  (* unparser: *)
  | ParenExpr of expr paren
  (*e: type exprbis hook *)

  (*s: type scalar and constant and encaps *)
    and scalar = 
      | C of constant
      | ClassConstant of (qualifier * name)
        
      | Guil    of tok (* '"' or b'"' *) * encaps list * tok (* '"' *)
      | HereDoc of 
          tok (* < < < EOF, or b < < < EOF *) * 
          encaps list * 
          tok  (* EOF; *)
      (* | StringVarName??? *)

   (*s: type constant *)
       and constant = 
       (*s: constant constructors *)
        | Int of string wrap
        | Double of string wrap
       (*x: constant constructors *)
        (* see also Guil for interpolated strings 
         * The string does not contain the enclosing '"' or "'".
         * It does not contain either the possible 'b' prefix
         *)
        | String of string wrap 
       (*x: constant constructors *)
        | CName of name (* true, false, null,  or defined constant *)
       (*x: constant constructors *)
        | PreProcess of cpp_directive wrap
       (*e: constant constructors *)
       (*s: type constant hook *)
        | XdebugClass of name * class_stmt list
        | XdebugResource (* TODO *)
       (*e: type constant hook *)
       (*s: constant rest *)
        (*s: type cpp_directive *)
          and cpp_directive = 
              | Line  | File 
              | ClassC  | MethodC  | FunctionC
        (*e: type cpp_directive *)
       (*e: constant rest *)
   (*e: type constant *)
   (*s: type encaps *)
       and encaps = 
         (*s: encaps constructors *)
          | EncapsString of string wrap
         (*x: encaps constructors *)
          | EncapsVar of lvalue
         (*x: encaps constructors *)
          (* for "xx {$beer}s" *)
          | EncapsCurly of tok * lvalue * tok
         (*x: encaps constructors *)
          (* for "xx ${beer}s" *)
          | EncapsDollarCurly of tok (* '${' *) * lvalue * tok
         (*x: encaps constructors *)
          | EncapsExpr of tok * expr * tok
         (*e: encaps constructors *)
   (*e: type encaps *)
  (*e: type scalar and constant and encaps *)

  (*s: AST expression operators *)

   and fixOp    = Dec | Inc 
   and binaryOp    = Arith of arithOp | Logical of logicalOp 
     (*s: php concat operator *)
      | BinaryConcat (* . *)
     (*e: php concat operator *)
         and arithOp   = 
           | Plus | Minus | Mul | Div | Mod
           | DecLeft | DecRight 
           | And | Or | Xor

         and logicalOp = 
           | Inf | Sup | InfEq | SupEq 
           | Eq | NotEq 
           (*s: php identity operators *)
           | Identical (* === *) | NotIdentical (* !== *)
           (*e: php identity operators *)
           | AndLog | OrLog | XorLog
           | AndBool | OrBool (* diff with AndLog ? short-circuit operators ? *)
   and assignOp = AssignOpArith of arithOp  
    (*s: php assign concat operator *)
     | AssignConcat (* .= *)
    (*e: php assign concat operator *)
   and unaryOp = 
     | UnPlus | UnMinus
     | UnBang | UnTilde

  (*x: AST expression operators *)
   and castOp = ptype
  (*e: AST expression operators *)

  (*s: AST expression rest *)
   and list_assign = 
     | ListVar of lvalue
     | ListList of tok * list_assign comma_list paren
     | ListEmpty
  (*x: AST expression rest *)
   and array_pair = 
     | ArrayExpr of expr
     | ArrayRef of tok (* & *) * lvalue
     | ArrayArrowExpr of expr * tok (* => *) * expr
     | ArrayArrowRef of expr * tok (* => *) * tok (* & *) * lvalue
  (*x: AST expression rest *)
   and class_name_reference = 
     | ClassNameRefStatic of name
     | ClassNameRefDynamic of (lvalue * obj_prop_access list)
     and obj_prop_access = tok (* -> *) * obj_property
  (*e: AST expression rest *)

 and xhp_html = 
   | Xhp of xhp_tag wrap * xhp_attribute list * tok (* > *) * 
       xhp_body list * xhp_tag option wrap
   | XhpSingleton of xhp_tag wrap * xhp_attribute list * tok (* /> *)

   and xhp_attribute = xhp_attr_name * tok (* = *) * xhp_attr_value
    and xhp_attr_name = string wrap (* e.g. task-bar *)
    and xhp_attr_value = 
      | XhpAttrString of tok (* '"' *) * encaps list * tok (* '"' *)
      | XhpAttrExpr of expr brace
   and xhp_body = 
     | XhpText of string wrap
     | XhpExpr of expr brace
     | XhpNested of xhp_html

(*e: AST expression *)
(* ------------------------------------------------------------------------- *)
(* Expression bis, lvalue *)
(* ------------------------------------------------------------------------- *)
(*s: AST lvalue *)
and lvalue = lvaluebis * lvalue_info
  (*s: type lvalue_info *)
    (* semantic: *)
    and lvalue_info = { 
      mutable tlval: Type_php.phptype;
    }
  (*e: type lvalue_info *)
  and lvaluebis =
  (*s: lvaluebis constructors *)
    | Var of dname * 
     (*s: scope_php annotation *)
     Scope_php.phpscope ref
     (*e: scope_php annotation *)
  (*x: lvaluebis constructors *)
    | This of tok
    (* xhp: normally we can not have a FunCall in the lvalue of VArrayAccess,
     * but with xhp we can.
     * 
     * TODO? a VArrayAccessSimple with Constant string in expr ? 
     *)
    | VArrayAccess of lvalue * expr option bracket
    | VArrayAccessXhp of expr * expr option bracket
  (*x: lvaluebis constructors *)
    | VBrace       of tok      * expr brace
    | VBraceAccess of lvalue * expr brace
  (*x: lvaluebis constructors *)
    (* on the left of var *)
    | Indirect  of lvalue * indirect 
  (*x: lvaluebis constructors *)
    | VQualifier of qualifier * lvalue
  (*x: lvaluebis constructors *)
    | FunCallSimple of name                      * argument comma_list paren
    | FunCallVar    of qualifier option * lvalue * argument comma_list paren
  (*x: lvaluebis constructors *)
    | StaticMethodCallSimple of qualifier * name * argument comma_list paren
    | MethodCallSimple of lvalue * tok * name    * argument comma_list paren
  (*x: lvaluebis constructors *)
    | ObjAccessSimple of lvalue * tok (* -> *) * name
    | ObjAccess of lvalue * obj_access
  (*e: lvaluebis constructors *)

  (*s: type lvalue aux *)
    and indirect = Dollar of tok
  (*x: type lvalue aux *)
    and argument = 
      | Arg    of expr
      | ArgRef of tok * w_variable
  (*x: type lvalue aux *)
    and obj_access = tok (* -> *) * obj_property * argument comma_list paren option

    and obj_property = 
      | ObjProp of obj_dim
      | ObjPropVar of lvalue (* was originally var_without_obj *)

      (* I would like to remove OName from here, as I inline most of them 
       * in the MethodCallSimple and ObjAccessSimple above, but they 
       * can also be mentionned in OArrayAccess in the obj_dim, so 
       * I keep it
       *)
      and obj_dim = 
        | OName of name
        | OBrace of expr brace
        | OArrayAccess of obj_dim * expr option bracket
        | OBraceAccess of obj_dim * expr brace
  (*e: type lvalue aux *)

(* semantic: those grammar rule names were used in the original PHP 
 * lexer/parser but not enforced. It's just comments. *)
and rw_variable = lvalue
and r_variable = lvalue
and w_variable = lvalue

(*e: AST lvalue *)
(* ------------------------------------------------------------------------- *)
(* Statement *)
(* ------------------------------------------------------------------------- *)
(*s: AST statement *)
(* by introducing Lambda, expr and stmt are now mutually recursive *)
and stmt = 
  (*s: stmt constructors *)
    | ExprStmt of expr * tok (* ; *)
    | EmptyStmt of tok  (* ; *)
  (*x: stmt constructors *)
    | Block of stmt_and_def list brace
  (*x: stmt constructors *)
    | If      of tok * expr paren * stmt * 
        (* elseif *) (tok * expr paren * stmt) list *
        (* else *) (tok * stmt) option
          (* if(){}{} *)
    (*s: ifcolon *)
    | IfColon of tok * expr paren * 
          tok * stmt_and_def list * new_elseif list * new_else option * 
          tok * tok
      (* if(cond):
       *   stmts; defs;
       * elseif(cond):
       *   stmts;..
       * else(cond):
       *   defs; stmst;
       * endif; *)
    (*e: ifcolon *)
    | While of tok * expr paren * colon_stmt
    | Do of tok * stmt * tok * expr paren * tok 
    | For of tok * tok * 
        for_expr * tok *
        for_expr * tok *
        for_expr *
        tok * 
        colon_stmt
    | Switch of tok * expr paren * switch_case_list
  (*x: stmt constructors *)
    (* if it's a expr_without_variable, the second arg must be a Right variable,
     * otherwise if it's a variable then it must be a foreach_variable
     *)
    | Foreach of tok * tok * expr * tok * 
        (foreach_variable, lvalue) Common.either * foreach_arrow option * tok * 
        colon_stmt
      (* example: foreach(expr as $lvalue) { colon_stmt }
       *          foreach(expr as $foreach_varialbe => $lvalue) { colon_stmt}
       *) 
  (*x: stmt constructors *)
    | Break    of tok * expr option * tok
    | Continue of tok * expr option * tok
    | Return of tok * expr option * tok 
  (*x: stmt constructors *)
    | Throw of tok * expr * tok
    | Try of tok * stmt_and_def list brace * catch * catch list
  (*x: stmt constructors *)
    | Echo of tok * expr comma_list * tok
  (*x: stmt constructors *)
    | Globals    of tok * global_var comma_list * tok
    | StaticVars of tok * static_var comma_list * tok
  (*x: stmt constructors *)
    | InlineHtml of string wrap
  (*x: stmt constructors *)
    | Use of tok * use_filename * tok
    | Unset of tok * lvalue comma_list paren * tok
    | Declare of tok * declare comma_list paren * colon_stmt
  (*e: stmt constructors *)
    (* static-php-ext: *)
    | TypedDeclaration of hint_type * lvalue * (tok * expr) option * tok


  (*s: AST statement rest *)
    and switch_case_list = 
      | CaseList      of 
          tok (* { *) * tok option (* ; *) * case list * tok (* } *)
      | CaseColonList of 
          tok (* : *) * tok option (* ; *) * case list * 
          tok (* endswitch *) * tok (* ; *)
      and case = 
        | Case    of tok * expr * tok * stmt_and_def list
        | Default of tok * tok * stmt_and_def list
  (*x: AST statement rest *)
    and for_expr = expr comma_list (* can be empty *)
    and foreach_arrow = tok * foreach_variable
    and foreach_variable = is_ref * lvalue
  (*x: AST statement rest *)
    and catch = 
      tok * (fully_qualified_class_name * dname) paren * stmt_and_def list brace
  (*x: AST statement rest *)
    and use_filename = 
      | UseDirect of string wrap
      | UseParen  of string wrap paren
  (*x: AST statement rest *)
    and declare = name * static_scalar_affect
  (*x: AST statement rest *)
    and colon_stmt = 
      | SingleStmt of stmt
      | ColonStmt of tok (* : *) * stmt_and_def list * tok (* endxxx *) * tok (* ; *)
  (*x: AST statement rest *)
    and new_elseif = tok * expr paren * tok * stmt_and_def list
    and new_else = tok * tok * stmt_and_def list
  (*e: AST statement rest *)
(*e: AST statement *)
(* ------------------------------------------------------------------------- *)
(* Function definition *)
(* ------------------------------------------------------------------------- *)
(*s: AST function definition *)
and func_def = {
  f_tok: tok; (* function *)
  f_ref: is_ref;
  f_name: name;
  f_params: parameter comma_list paren;
  f_return_type: hint_type option;
  f_body: stmt_and_def list brace;
  (*s: f_type mutable field *)
  (* semantic: *)
  mutable f_type: Type_php.phptype; 
  (*e: f_type mutable field *)
}
  (*s: AST function definition rest *)
    and parameter = {
      p_type: hint_type option;
      p_ref: is_ref;
      p_name: dname;
      p_default: static_scalar_affect option;
    }
  (*x: AST function definition rest *)
      and hint_type = 
        | Hint of name
        | HintArray  of tok
  (*x: AST function definition rest *)
    and is_ref = tok (* bool wrap ? *) option
  (*e: AST function definition rest *)
(*e: AST function definition *)
(*s: AST lambda definition *)
and lambda_def = {
 l_tok: tok; (* function *)
 l_ref: is_ref;
 (* no l_name, anonymous *)
 l_params: parameter comma_list paren;
 l_use:  lexical_vars option;
 l_body: stmt_and_def list brace;
}
  and lexical_vars = tok (* use *) * lexical_var comma_list paren 
  and lexical_var = 
    | LexicalVar of is_ref * dname

(*e: AST lambda definition *)
(* ------------------------------------------------------------------------- *)
(* Class definition *)
(* ------------------------------------------------------------------------- *)
(*s: AST class definition *)
and class_def = {
  c_type: class_type;
  c_name: name;
  c_extends: extend option;
  c_implements: interface option;
  c_body: class_stmt list brace;
}
  (*s: type class_type *)
    and class_type = 
      | ClassRegular  of tok (* class *)
      | ClassFinal    of tok * tok (* final class *)
      | ClassAbstract of tok * tok (* abstract class *)
  (*e: type class_type *)
  (*s: type extend *)
    and extend =    tok * fully_qualified_class_name
  (*e: type extend *)
  (*s: type interface *)
    and interface = tok * fully_qualified_class_name comma_list
  (*e: type interface *)
(*x: AST class definition *)
and interface_def = {
  i_tok: tok; (* interface *)
  i_name: name;
  i_extends: interface option;
  i_body: class_stmt list brace;
}
(*x: AST class definition *)
  and class_stmt = 
    | ClassConstants of tok (* const *) * class_constant comma_list * tok (*;*)
    | ClassVariables of 
        class_var_modifier * hint_type option *
        class_variable comma_list * tok (* ; *)
    | Method of method_def

    | XhpDecl of xhp_decl

    (*s: class_stmt types *)
        and class_constant = name * static_scalar_affect
    (*x: class_stmt types *)
        and class_variable = dname * static_scalar_affect option
    (*x: class_stmt types *)
        and class_var_modifier = 
          | NoModifiers of tok (* 'var' *)
          | VModifiers of modifier wrap list
    (*x: class_stmt types *)
        and method_def = {
          m_modifiers: modifier wrap list;
          m_tok: tok; (* function *)
          m_ref: is_ref;
          m_name: name;
          m_params: parameter comma_list paren;
          m_return_type: hint_type option;
          m_body: method_body;
        }
    (*x: class_stmt types *)
          and modifier = 
            | Public  | Private | Protected
            | Static  | Abstract | Final
    (*x: class_stmt types *)
          and method_body = 
            | AbstractMethod of tok
            | MethodBody of stmt_and_def list brace
    (*e: class_stmt types *)
 and xhp_decl = 
    | XhpAttributesDecl of 
        tok (* attribute *) * xhp_attribute_decl comma_list * tok (*;*)
    (* there is normally only one 'children' declaration in a class *)
    | XhpChildrenDecl of 
        tok (* children *) * xhp_children_decl * tok (*;*)
    | XhpCategoriesDecl of 
        tok (* category *) * xhp_category_decl comma_list * tok (*;*)

 and xhp_attribute_decl = 
   | XhpAttrInherit of xhp_tag wrap
   | XhpAttrDecl of xhp_attribute_type * xhp_attr_name * 
       xhp_value_affect option * tok option (* is required *)
   and xhp_attribute_type = 
     | XhpAttrType of name (* e.g. float, bool, var, array, :foo *)
     | XhpAttrEnum of tok (* enum *) * constant comma_list brace
  and xhp_value_affect = tok (* = *) * constant

 (* Regexp-like syntax. The grammar actually restricts what kinds of
  * regexps can be written. For instance pcdata must be nested. But
  * here I simplified the type.
  *)
 and xhp_children_decl = 
   | XhpChild of xhp_tag wrap (* :x:frag *)
   | XhpChildCategory of xhp_tag wrap (* %x:frag *)

   | XhpChildAny of tok
   | XhpChildEmpty of tok
   | XhpChildPcdata of tok

   | XhpChildSequence    of xhp_children_decl * tok (*,*) * xhp_children_decl
   | XhpChildAlternative of xhp_children_decl * tok (*|*) * xhp_children_decl

   | XhpChildMul    of xhp_children_decl * tok (* * *)
   | XhpChildOption of xhp_children_decl * tok (* ? *)
   | XhpChildPlus   of xhp_children_decl * tok (* + *)

   | XhpChildParen of xhp_children_decl paren

 and xhp_category_decl = xhp_tag wrap (* %x:frag *)

(*e: AST class definition *)
(* ------------------------------------------------------------------------- *)
(* Other declarations *)
(* ------------------------------------------------------------------------- *)
(*s: AST other declaration *)
and global_var = 
  | GlobalVar of dname
  | GlobalDollar of tok * r_variable
  | GlobalDollarExpr of tok * expr brace
(*x: AST other declaration *)
and static_var = dname * static_scalar_affect option
(*x: AST other declaration *)
  and static_scalar = 
    | StaticConstant of constant
    | StaticClassConstant of (qualifier * name) (* semantic ? *)
        
    | StaticPlus  of tok * static_scalar
    | StaticMinus of tok * static_scalar

    | StaticArray of tok (* array *) * static_array_pair comma_list paren
  (*s: type static_scalar hook *)
    | XdebugStaticDots
  (*e: type static_scalar hook *)
(*x: AST other declaration *)
   and static_scalar_affect = tok (* = *) * static_scalar
(*x: AST other declaration *)
    and static_array_pair = 
      | StaticArraySingle of static_scalar
      | StaticArrayArrow  of static_scalar * tok (* => *) * static_scalar
(*e: AST other declaration *)
(* ------------------------------------------------------------------------- *)
(* Stmt bis *)
(* ------------------------------------------------------------------------- *)
(*s: AST statement bis *)
(* Was originally called toplevel, but for parsing reasons and estet I think
 * it's better to differentiate nested func and top func. Also better to
 * group the toplevel statements together (StmtList below), so that
 * in the database later they share the same id.
 *)
and stmt_and_def = 
  | Stmt of stmt
  | FuncDefNested of func_def
  | ClassDefNested of class_def
  | InterfaceDefNested of interface_def

(*e: AST statement bis *)
(* ------------------------------------------------------------------------- *)
(* phpext: *)
(* ------------------------------------------------------------------------- *)
(*s: AST phpext *)
(*e: AST phpext *)
(* ------------------------------------------------------------------------- *)
(* The toplevels elements *)
(* ------------------------------------------------------------------------- *)
(*s: AST toplevel *)
and toplevel = 
  (*s: toplevel constructors *)
    | StmtList of stmt list
    | FuncDef of func_def
    | ClassDef of class_def
    | InterfaceDef of interface_def
  (*x: toplevel constructors *)
    | Halt of tok * unit paren * tok (* __halt__ ; *)
  (*x: toplevel constructors *)
    | NotParsedCorrectly of info list
  (*x: toplevel constructors *)
    | FinalDef of info (* EOF *)
  (*e: toplevel constructors *)

 and program = toplevel list

 (*s: tarzan annotation *)
  (* with tarzan *)
 (*e: tarzan annotation *)

(*e: AST toplevel *)

type any = 
  | Lvalue of lvalue
  | Expr of expr
  | Stmt2 of stmt
  | StmtAndDef of stmt_and_def
  | Toplevel of toplevel
  | Program of program

  | Argument of argument
  | Parameter of parameter
  | Parameters of parameter comma_list paren
  | Body of stmt_and_def list brace
  | StmtAndDefs of stmt_and_def list

  | ClassStmt of class_stmt
  | ClassConstant2 of class_constant
  | ClassVariable of class_variable
  | ListAssign of list_assign
  | ColonStmt2 of colon_stmt
 
  | XhpAttribute of xhp_attribute
  | XhpAttrValue of xhp_attr_value
  | XhpHtml2 of xhp_html

  | Info of info
  | InfoList of info list
  (* with tarzan *)

(*****************************************************************************)
(* AST helpers *)
(*****************************************************************************)
(*s: AST helpers interface *)
val parse_info_of_info : info -> Parse_info.parse_info
(*x: AST helpers interface *)
val pinfo_of_info : info -> pinfo
(*x: AST helpers interface *)
val pos_of_info : info -> int
val str_of_info : info -> string
val file_of_info : info -> Common.filename
val line_of_info : info -> int
val col_of_info : info -> int
(*x: AST helpers interface *)
val string_of_info : info -> string
(*x: AST helpers interface *)
val name : name -> string
val dname : dname -> string
(*x: AST helpers interface *)
val info_of_name : name -> info
val info_of_dname : dname -> info
(*x: AST helpers interface *)
val unwrap : 'a wrap -> 'a
(*x: AST helpers interface *)
val unparen : tok * 'a * tok -> 'a
val unbrace : tok * 'a * tok -> 'a
val unbracket : tok * 'a * tok -> 'a
val uncomma: 'a comma_list -> 'a list

val map_paren: ('a -> 'b) -> 'a paren -> 'b paren
val map_comma_list: ('a -> 'b) -> 'a comma_list -> 'b comma_list

val unarg: argument -> expr
val unmodifiers: class_var_modifier -> modifier list
(*x: AST helpers interface *)
val untype : 'a * 'b -> 'a
(*x: AST helpers interface *)
val get_type : expr -> Type_php.phptype
val set_type : expr -> Type_php.phptype -> unit
(*x: AST helpers interface *)
val rewrap_str : string -> info -> info
val rewrap_parse_info : Parse_info.parse_info -> info -> info
val is_origintok : info -> bool
val al_info : info -> info
val compare_pos : info -> info -> int
(*x: AST helpers interface *)
val noType : unit -> exp_info
val noTypeVar : unit -> lvalue_info
val noScope : unit -> Scope_php.phpscope ref
val noFtype : unit -> Type_php.phptype

val fakeInfo: ?next_to:(Parse_info.parse_info * int) option -> string -> info
(*e: AST helpers interface *)
(*e: ast_php.mli *)
