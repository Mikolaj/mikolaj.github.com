(**********)
(* ElabTy *)
(**********)


functor ElabTy
    
    ( structure AbstractGrammar' : ABSTRACT_GRAMMAR

      structure OkError' : OK_ERROR

      structure Ttype' : TTYPE ) : sig

	                               include ELAB_TY 

                                       sharing AbstractGrammar = AbstractGrammar'
				       sharing OkError = OkError'

				       sharing Ttype = Ttype'
				   
				   end = 

struct

    structure AbstractGrammar = AbstractGrammar'

    structure OkError = OkError'

    structure Ttype = Ttype'

    fun T(AbstractGrammar.INT(i)) = OkError.OK(Ttype.INT)
      | T(AbstractGrammar.BOOL(i)) = OkError.OK(Ttype.BOOL) 
      | T(AbstractGrammar.FUN(i, ty1, ty2)) = 
	(case T ty1
	   of OkError.OK(tau1) => 
	       (case T ty2
		  of OkError.OK(tau2) => OkError.OK(Ttype.FUN(tau1, tau2))
		   | OkError.ERROR(e) => OkError.ERROR(e))
	    | OkError.ERROR(e) => OkError.ERROR(e))


end
