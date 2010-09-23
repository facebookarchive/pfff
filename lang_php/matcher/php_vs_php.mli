
module type PARAM = 
  sig 
    type tin
    type 'x tout
    type ('a, 'b) matcher = 'a -> 'b  -> tin -> ('a * 'b) tout
    val (>>=): 
      (tin -> ('a * 'b) tout)  -> 
      ('a * 'b -> (tin -> ('c * 'd) tout)) -> 
      (tin -> ('c * 'd) tout)

    (* The classical monad combinators *)
    val return : ('a * 'b) -> tin -> ('a *'b) tout
    val fail : tin -> ('a * 'b) tout

    val envf : (Metavars_php.mvar * Metavars_php.binded_code) ->
      (unit -> tin -> 'x tout) -> (tin -> 'x tout)
  end

module PHP_VS_PHP :
  functor (X : PARAM) -> 
    sig
      type ('a, 'b) matcher = 'a -> 'b -> X.tin -> ('a * 'b) X.tout

      val m_expr :     (Ast_php.expr,   Ast_php.expr)   matcher
      val m_variable : (Ast_php.lvalue, Ast_php.lvalue) matcher
    end
