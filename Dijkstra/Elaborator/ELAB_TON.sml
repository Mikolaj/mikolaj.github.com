(***********)
(* ElabTon *)
(***********)


signature ELAB_TON =
sig
    
    structure AbstractGrammar : ABSTRACT_GRAMMAR

    structure OkError : OK_ERROR

    sharing OkError.CharInfo = AbstractGrammar.CharInfo

    structure ElaboratorError : ELABORATOR_ERROR

    sharing OkError.Error = ElaboratorError

    structure Ttype : TTYPE 

    sharing ElaboratorError.Ttype = Ttype 

    val V' : 
	(AbstractGrammar.type_exp -> Ttype.ttype OkError.ok) ->

	AbstractGrammar.type_or_not -> Ttype.ttype -> Ttype.ttype OkError.ok

end

