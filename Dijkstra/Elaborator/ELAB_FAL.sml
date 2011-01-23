(***********)
(* ElabFal *)
(***********)


signature ELAB_FAL = 
sig
    
    structure AbstractGrammar : ABSTRACT_GRAMMAR

    structure OkError : OK_ERROR

    structure Falseval : sig

			     type fal

			     val fal2bool : fal -> bool

			 end

    sharing AbstractGrammar.Falseval = Falseval

    structure Boolean : sig 
			
			    type boolean
			   
			    val bool2boolean : bool -> boolean
		      
			end
 
    val F : AbstractGrammar.falseval -> Boolean.boolean OkError.ok

end