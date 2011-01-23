(**********)
(* ElabId *)
(**********)


functor ElabId 
    
    ( structure AbstractGrammar' : ABSTRACT_GRAMMAR

      structure OkError' : OK_ERROR

      structure Identifier' : sig

				 type id
   
				 val id2string : id -> string      

			     end

      sharing AbstractGrammar'.Identifier = Identifier'

      structure Name' : sig 
			
			   type name 
			   
			   val string2name : string -> name 
		      
		       end ) : sig

	                           include ELAB_ID 

                                   sharing AbstractGrammar = AbstractGrammar'

				   sharing OkError = OkError'

				   sharing Identifier = Identifier'

				   sharing Name = Name'

			       end = 

struct

    structure AbstractGrammar = AbstractGrammar'

    structure OkError = OkError'

    structure Identifier = Identifier'

    structure Name = Name'

    fun I(AbstractGrammar.IDENTIFIER(i, id)) = OkError.OK(Name.string2name(Identifier.id2string id))

end