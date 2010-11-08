(*****************************************************************************)
(* PHP vs PHP *)
(*****************************************************************************)

module type PARAM = 
  sig 
    type tin
    type 'x tout
    type ('a, 'b) matcher = 'a -> 'b  -> tin -> ('a * 'b) tout
    val (>>=): 
      (tin -> ('a * 'b) tout)  -> 
      ('a * 'b -> (tin -> ('c * 'd) tout)) -> 
      (tin -> ('c * 'd) tout)

    val (>||>) : 
      (tin -> 'x tout) ->
      (tin -> 'x tout) -> 
      (tin -> 'x tout)

    (* The classical monad combinators *)
    val return : ('a * 'b) -> tin -> ('a *'b) tout
    val fail : tin -> ('a * 'b) tout

    (* -------------------------------------------------------------------- *)
    (* Tokens tagging *)
    (* -------------------------------------------------------------------- *)
    val tokenf :     (Ast_php.info,  Ast_php.info) matcher

    (* -------------------------------------------------------------------- *)
    (* Distr_f functions, to tag a range of tokens *)
    (* -------------------------------------------------------------------- *)

    (* -------------------------------------------------------------------- *)
    (* Environment manipulation. Extract info from tin, the "something" *)
    (* -------------------------------------------------------------------- *)
    val envf : (Metavars_php.mvar * Ast_php.any) ->
      (unit -> tin -> 'x tout) -> (tin -> 'x tout)


  end

(*****************************************************************************)
(* The functor itself *)
(*****************************************************************************)

module PHP_VS_PHP :
  functor (X : PARAM) -> 
    sig
      type ('a, 'b) matcher = 'a -> 'b -> X.tin -> ('a * 'b) X.tout

      val m_expr :     (Ast_php.expr,   Ast_php.expr)   matcher
      val m_variable : (Ast_php.lvalue, Ast_php.lvalue) matcher

      (* there are far more functions in this functor but they 
       * do not have to be exported 
       *)
    end
