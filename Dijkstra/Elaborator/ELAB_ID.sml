(**********)
(* ElabId *)
(**********)


signature ELAB_ID = 
sig

    structure AbstractGrammar : ABSTRACT_GRAMMAR

    structure OkError : OK_ERROR

    structure Identifier : sig

			       type id
   
			       val id2string : id -> string      

			   end

    sharing AbstractGrammar.Identifier = Identifier

    structure Name : sig 
	
			 type name 
			   
			 val string2name : string -> name 
		      
		     end

    val I : AbstractGrammar.identifier -> Name.name OkError.ok

end
