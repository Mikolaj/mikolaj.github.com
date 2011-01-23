(***********)
(* ElabCom *)
(***********)


signature ELAB_COM =
sig

    structure AbstractGrammar : ABSTRACT_GRAMMAR

    structure OkError : OK_ERROR

    sharing OkError.CharInfo = AbstractGrammar.CharInfo

    structure ElaboratorError : ELABORATOR_ERROR

    sharing OkError.Error = ElaboratorError
     
    structure Post : POST

    sharing Post.CharInfo = AbstractGrammar.CharInfo

    structure NameSet : NAME_SET

    sharing ElaboratorError.NameSet = NameSet

    structure Environment : 
	sig

	    structure Name : sig type name end 
    
	    structure Denotable : sig type denotable end 
 
	    type environment 
		
	    val access_env : Name.name -> environment -> Denotable.denotable

	end

    sharing NameSet.Name = Environment.Name

    structure Denotable : DENOTABLE

    sharing ElaboratorError.Ttype = Denotable.Ttype

    sharing Environment.Denotable = Denotable

    structure Store : STORE

    sharing Denotable.Location = Store.Location

    structure Location : sig type location_tube end 

    val C' :
	(AbstractGrammar.identifier -> NameSet.Name.name OkError.ok) ->

	(AbstractGrammar.expression -> Environment.environment -> 
	 Location.location_tube -> NameSet.name_set -> 
	 (Denotable.Ttype.ttype * (Store.store -> Store.Storable.storable Post.post)) OkError.ok) ->
	
	(AbstractGrammar.guard -> Environment.environment -> 
	 Location.location_tube -> NameSet.name_set -> 
	 (NameSet.name_set * (Store.store -> Store.store Post.post Post.post)) OkError.ok) ->

	(AbstractGrammar.block -> Environment.environment -> 
	 Location.location_tube -> NameSet.name_set -> 
	 (NameSet.name_set * (Store.store -> Store.store Post.post)) OkError.ok) ->

	AbstractGrammar.command -> Environment.environment -> 
	Location.location_tube -> NameSet.name_set -> 
	(NameSet.name_set * (Store.store -> Store.store Post.post)) OkError.ok

end
