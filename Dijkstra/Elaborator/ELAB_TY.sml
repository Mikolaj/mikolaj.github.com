(**********)
(* ElabTy *)
(**********)


signature ELAB_TY =
sig
    
    structure AbstractGrammar : ABSTRACT_GRAMMAR

    structure OkError : OK_ERROR

    structure Ttype : TTYPE

    val T : AbstractGrammar.type_exp -> Ttype.ttype OkError.ok

end