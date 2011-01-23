(**********)
(* ElabBl *)
(**********)


signature ELAB_BL =
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

    structure Environment : sig type environment end

    structure Location : sig type location_tube end

    structure Store : sig type store end

    val B' :
	(AbstractGrammar.declaration -> Environment.environment -> 
	 Location.location_tube -> NameSet.name_set -> 
	 (Environment.environment * Location.location_tube *
	  NameSet.name_set * NameSet.name_set * NameSet.name_set) OkError.ok) ->

	(AbstractGrammar.command -> Environment.environment -> 
	 Location.location_tube -> NameSet.name_set -> 
	 (NameSet.name_set * (Store.store -> Store.store Post.post)) OkError.ok) ->

	AbstractGrammar.block -> Environment.environment -> 
	Location.location_tube -> NameSet.name_set -> 
	(NameSet.name_set * (Store.store -> Store.store Post.post)) OkError.ok
	
end
