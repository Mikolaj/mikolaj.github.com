(***********)
(* ElabGua *)
(***********)


signature ELAB_GUA =
sig

    structure AbstractGrammar : ABSTRACT_GRAMMAR

    structure OkError : OK_ERROR
 
    sharing OkError.CharInfo = AbstractGrammar.CharInfo

    structure ElaboratorError : ELABORATOR_ERROR

    sharing OkError.Error = ElaboratorError

    structure Post : POST

    sharing Post.CharInfo = AbstractGrammar.CharInfo

    structure Ttype : sig

			  eqtype ttype
			      
			  val ttype_bool : ttype

		      end

    sharing ElaboratorError.Ttype = Ttype

    structure Storable : sig

			     type storable

			     val is_storable_false : storable -> bool

			     val is_storable_true : storable -> bool
    
			 end

    structure Environment : sig type environment end

    structure Location : sig type location_tube end

    structure NameSet : NAME_SET

    sharing ElaboratorError.NameSet = NameSet

    structure Store : sig type store end

    val G' :
	(AbstractGrammar.expression -> Environment.environment -> 
	 Location.location_tube -> NameSet.name_set -> 
	 (Ttype.ttype * (Store.store -> Storable.storable Post.post)) OkError.ok) ->
	
	(AbstractGrammar.command -> Environment.environment -> 
	 Location.location_tube -> NameSet.name_set -> 
	 (NameSet.name_set * (Store.store -> Store.store Post.post)) OkError.ok) ->

	AbstractGrammar.guard -> Environment.environment -> 
	Location.location_tube -> NameSet.name_set -> 
	(NameSet.name_set * (Store.store -> Store.store Post.post Post.post)) OkError.ok

end
