(***********)
(* ElabFal *)
(***********)


functor ElabFal 

    ( structure AbstractGrammar' : ABSTRACT_GRAMMAR

      structure OkError' : OK_ERROR

      structure Falseval' : sig

			       type fal

			       val fal2bool : fal -> bool

			   end

      sharing AbstractGrammar'.Falseval = Falseval'

      structure Boolean' : sig 
			
			      type boolean
			   
			      val bool2boolean : bool -> boolean
		      
			  end ) : sig

	                              include ELAB_FAL

                                      sharing AbstractGrammar = AbstractGrammar'

				      sharing OkError = OkError'

				      sharing Falseval = Falseval'

				      sharing Boolean = Boolean'

				  end =

struct

    structure AbstractGrammar = AbstractGrammar'

    structure OkError = OkError'

    structure Falseval = Falseval'

    structure Boolean = Boolean'

    fun F(AbstractGrammar.FALSEVAL(i, f)) = OkError.OK(Boolean.bool2boolean (Falseval.fal2bool f))

end