(***********)
(* ElabExp *)
(***********)


signature ELAB_EXP =
sig

    structure AbstractGrammar : ABSTRACT_GRAMMAR

    structure OkError : OK_ERROR

    sharing OkError.CharInfo = AbstractGrammar.CharInfo

    structure ElaboratorError : ELABORATOR_ERROR

    sharing OkError.Error = ElaboratorError

    structure Post : POST

    sharing Post.CharInfo = AbstractGrammar.CharInfo
 
    structure Environment : sig type environment end

    structure NameSet : sig type name_set end

    structure Store : sig type store end

    structure Ttype : TTYPE

    sharing ElaboratorError.Ttype = Ttype

    structure Storable : STORABLE

    sharing Storable.Post = Post

    structure Location : sig type location_tube end 
  
    val E' : 
	(AbstractGrammar.identifier -> Environment.environment -> NameSet.name_set ->
	(Ttype.ttype * (Store.store -> Storable.storable Post.post)) OkError.ok) ->

	(AbstractGrammar.block -> Environment.environment -> 
	 Location.location_tube -> NameSet.name_set ->
	 (Ttype.ttype * (Store.store -> Storable.storable Post.post)) OkError.ok) ->

	(AbstractGrammar.numeral -> Storable.Integer.integer OkError.ok) ->

	(AbstractGrammar.falseval -> Storable.Boolean.boolean OkError.ok) ->

	AbstractGrammar.expression -> Environment.environment -> 
	Location.location_tube -> NameSet.name_set -> 
	(Ttype.ttype * (Store.store -> Storable.storable Post.post)) OkError.ok
	
end