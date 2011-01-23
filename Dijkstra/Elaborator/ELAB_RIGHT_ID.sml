(***************)
(* ElabRightId *)
(***************)


signature ELAB_RIGHT_ID =
sig
 
    structure AbstractGrammar : ABSTRACT_GRAMMAR

    structure OkError : OK_ERROR

    sharing OkError.CharInfo = AbstractGrammar.CharInfo

    structure ElaboratorError : ELABORATOR_ERROR

    sharing OkError.Error = ElaboratorError

    structure Post : POST
      
    structure Environment : 
	sig

	    structure Name : sig type name end 
    
	    structure Denotable : sig type denotable end 
 
	    type environment 
		
	    val access_env : Name.name -> environment -> Denotable.denotable

	end

    structure NameSet : sig
	  
			    structure Name : sig type name end

			    type name_set

			    val is_in_one : Name.name -> name_set -> bool
				   
			end

    sharing ElaboratorError.NameSet.Name = NameSet.Name

    sharing NameSet.Name = Environment.Name

    structure Denotable : DENOTABLE

    sharing Environment.Denotable = Denotable

    structure Store : sig

			  structure Location : sig type location end
    
			  structure Storable : sig type storable end

			  type store

			  val access : Location.location -> store -> Storable.storable
		      
		      end
		  
    sharing Denotable.Location = Store.Location 


    val R' : 
	(AbstractGrammar.identifier -> NameSet.Name.name OkError.ok) ->

	AbstractGrammar.identifier -> Environment.environment -> NameSet.name_set ->
	(Denotable.Ttype.ttype * (Store.store -> Store.Storable.storable Post.post)) OkError.ok

end