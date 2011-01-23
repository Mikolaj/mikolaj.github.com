(***********)
(* ElabNum *)
(***********)


signature ELAB_NUM = 
sig
    
    structure AbstractGrammar : ABSTRACT_GRAMMAR

    structure OkError : OK_ERROR

    structure Numeral : sig 
			
			    type num
			   
			    val num2int : num -> int
		      
			end

    sharing AbstractGrammar.Numeral = Numeral

    structure Integer : sig 
			
			    type integer
			   
			    val int2integer : int -> integer
		      
			end

    val N : AbstractGrammar.numeral -> Integer.integer OkError.ok

end