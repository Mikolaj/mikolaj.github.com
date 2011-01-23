(***********)
(* ElabNum *)
(***********)


functor ElabNum 

    ( structure AbstractGrammar' : ABSTRACT_GRAMMAR

      structure OkError' : OK_ERROR

      structure Numeral' : sig 
			
			      type num
			   
			      val num2int : num -> int
		      
			  end

      sharing AbstractGrammar'.Numeral = Numeral'

      structure Integer' : sig 
			
			   type integer
			   
			   val int2integer : int -> integer
		      
		       end ) : sig

	                           include ELAB_NUM

                                   sharing AbstractGrammar = AbstractGrammar'

				   sharing OkError = OkError'

				   sharing Numeral = Numeral'

				   sharing Integer = Integer'

			       end =

struct

    structure AbstractGrammar = AbstractGrammar'

    structure OkError = OkError'

    structure Numeral = Numeral'

    structure Integer = Integer'

    fun N(AbstractGrammar.NUMERAL(i, n)) = OkError.OK(Integer.int2integer (Numeral.num2int n))

end