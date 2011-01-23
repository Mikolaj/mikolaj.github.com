(***************)
(* ElabRightId *)
(***************)


functor ElabRightId
 
    ( structure AbstractGrammar' : ABSTRACT_GRAMMAR

      structure OkError' : OK_ERROR

      sharing OkError'.CharInfo = AbstractGrammar'.CharInfo

      structure ElaboratorError' : ELABORATOR_ERROR

      sharing OkError'.Error = ElaboratorError'

      structure Post' : POST

      structure Environment' : 
	  sig

	      structure Name : sig type name end 
    
	      structure Denotable : sig type denotable end 
 
	      type environment 
		
	      val access_env : Name.name -> environment -> Denotable.denotable

	  end

      structure NameSet' : sig
	  
			       structure Name : sig type name end

			       type name_set

			       val is_in_one : Name.name -> name_set -> bool
				   
			   end

      sharing ElaboratorError'.NameSet.Name = NameSet'.Name

      sharing NameSet'.Name = Environment'.Name

      structure Denotable' : DENOTABLE

      sharing Environment'.Denotable = Denotable'

      structure Store' : sig

			    structure Location : sig type location end
    
			    structure Storable : sig type storable end

			    type store

			    val access : Location.location -> store -> Storable.storable
		      
			end

      sharing Denotable'.Location = Store'.Location ) : sig

	                                                    include ELAB_RIGHT_ID 

                                                            sharing AbstractGrammar = AbstractGrammar'

							    sharing OkError = OkError'

							    sharing ElaboratorError = ElaboratorError'

							    sharing Post = Post'

							    sharing Environment = Environment'

							    sharing NameSet = NameSet'

							    sharing Denotable = Denotable'

							    sharing Store = Store'

							end = 

struct

    structure AbstractGrammar = AbstractGrammar'

    structure OkError = OkError'

    structure ElaboratorError = ElaboratorError'

    structure Post = Post'

    structure Environment = Environment'

    structure NameSet = NameSet'

    structure Denotable = Denotable'

    structure Store = Store'

local

    open AbstractGrammar

    open OkError 

    open NameSet

    open ElaboratorError

in
 
    fun R' I =

  let
           
    fun R(IDENTIFIER(i, id)) e vir = (case I(IDENTIFIER(i, id)) 
					of OK(nam) => 
					    (if is_in_one nam vir then ERROR(i, VAR_IS_VIRGIN_RID(nam))
					     else case (Environment.access_env nam e)
						    of Denotable.VAR(tau, l) => 
							OK(tau, fn s => Post.GOING(Store.access l s))
						     | Denotable.CON(tau, l) => 
							OK(tau, fn s => Post.GOING(Store.access l s))
						     | Denotable.FREE => ERROR(i, VAR_NOT_KNOWN_RID(nam)))
					 | ERROR(e) => ERROR(e))

  in
      R
  end

end

end