(*s: pil.mli *)

(* The goal is to be significantly smaller than ast_php.mli *)

type name = Ast_php.name
type dname = Ast_php.dname

type qualifier = Ast_php.qualifier
type indirect = Ast_php.indirect
type binaryOp = Ast_php.binaryOp
type unaryOp = Ast_php.unaryOp
type constant = Ast_php.constant
type class_name_reference = Ast_php.class_name_reference
type assignOp = Ast_php.assignOp
type castOp = Ast_php.castOp

type type_info = {
  mutable t: Type_php.phptype;
}

type var = 
  | Var of dname
  | This of Ast_php.tok


type lvalue = lvaluebis * type_info
 and lvaluebis = 
   | VVar of var
   | VQualifier of qualifier * var

   | ArrayAccess of var * expr option
   | ObjAccess of var * name
   | DynamicObjAccess of var * var
   | IndirectAccess of var * indirect

and expr = exprbis * type_info
 and exprbis =
  | Lv of lvalue
  | C of constant
  | ClassConstant of (qualifier * name)

  | Binary  of expr * binaryOp * expr
  | Unary   of unaryOp * expr
  (* We could transform into a If but this would require to
   * change the return types of linearize_expr; this is because we would
   * need to push a stmt rather than just an instr *)
  | CondExpr of expr * expr * expr

  | ConsArray of expr list (* array(expr1,..., exprn)*)
  | ConsHash  of (expr * expr) list (* array(key1 => expr1,...)
                                     also defined as an array in AST *)

  | Cast of castOp * expr
  | InstanceOf of expr * class_name_reference


type instr =
  | Assign of lvalue * assign_kind * expr
  | AssignRef of lvalue * lvalue
  (* Call(a,f,args) intuitively is an instruction a=f(args) *)
  | Call of lvalue * call_kind * argument list
  | Eval of expr

  and assign_kind = 
    | AssignEq
    | AssignOp of assignOp 

  and call_kind = 
    | SimpleCall        of name
    | StaticMethodCall  of qualifier * name
    | MethodCall        of var * name
    | DynamicCall       of qualifier option * var
    | DynamicMethodCall of var * var

    | New of class_name_reference 

   and argument = 
     | Arg of expr
     | ArgRef of lvalue

type stmt =
  | Instr of instr
  | Block of stmt list
  | EmptyStmt 
  | If of expr * stmt * stmt
  | While of expr * stmt
  | Break of expr option
  | Continue of expr option
  | Return of expr option
  | Throw of expr
  | Try of stmt * catch
  | Echo of expr list

 and catch = unit (* TODO *)

(* for debugging *)
val string_of_instr: ?show_all:bool -> instr -> string
val string_of_stmt: ?show_all:bool -> stmt -> string
val string_of_expr: ?show_all:bool -> expr -> string

(* meta *)
val vof_expr: expr -> Ocaml.v
val vof_instr: instr -> Ocaml.v
val vof_argument: argument -> Ocaml.v

(*e: pil.mli *)
