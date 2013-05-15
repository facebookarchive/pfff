
(*****************************************************************************)
(* Ast_fuzzy vs Ast_fuzzy *)
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
    val tokenf :     (Parse_info.info,  Parse_info.info) matcher

    (* -------------------------------------------------------------------- *)
    (* Distr_f functions, to tag a range of tokens *)
    (* -------------------------------------------------------------------- *)

    (* -------------------------------------------------------------------- *)
    (* Environment manipulation. Extract info from tin, the "something" *)
    (* -------------------------------------------------------------------- *)
    val envf : (Metavars_fuzzy.mvar * Parse_info.info, Ast_fuzzy.trees)
      matcher
  end

(*****************************************************************************)
(* The functor itself *)
(*****************************************************************************)

module X_VS_X :
  functor (X : PARAM) -> 
    sig
      type ('a, 'b) matcher = 'a -> 'b -> X.tin -> ('a * 'b) X.tout

      val m_trees: (Ast_fuzzy.trees, Ast_fuzzy.trees) matcher
    end
