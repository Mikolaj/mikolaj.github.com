(***********)
(* ElabDec *)
(***********)


signature ELAB_DEC =
sig

    structure AbstractGrammar : ABSTRACT_GRAMMAR

    structure OkError : OK_ERROR

    sharing OkError.CharInfo = AbstractGrammar.CharInfo

    structure ElaboratorError : ELABORATOR_ERROR

    sharing OkError.Error = ElaboratorError

    structure Location : LOCATION

    structure Environment : ENVIRONMENT

    structure Denotable : DENOTABLE

    sharing ElaboratorError.Ttype = Denotable.Ttype

    sharing Denotable.Location = Location

    sharing Environment.Denotable = Denotable 

    structure NameSet : NAME_SET

    sharing ElaboratorError.NameSet = NameSet

    sharing Environment.Name = NameSet.Name

    val D' : 
	(AbstractGrammar.identifier -> NameSet.Name.name OkError.ok) ->

	(AbstractGrammar.type_exp -> Denotable.Ttype.ttype OkError.ok) ->

	(AbstractGrammar.type_or_not -> Denotable.Ttype.ttype -> Denotable.Ttype.ttype OkError.ok) ->

	AbstractGrammar.declaration -> Environment.environment -> 
	Location.location_tube -> NameSet.name_set -> 
	(Environment.environment * Location.location_tube *
	 NameSet.name_set * NameSet.name_set * NameSet.name_set) OkError.ok

end 