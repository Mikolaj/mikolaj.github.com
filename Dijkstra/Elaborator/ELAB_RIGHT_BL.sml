(***************)
(* ElabRightBl *)
(***************)


signature ELAB_RIGHT_BL =
sig

    structure AbstractGrammar : ABSTRACT_GRAMMAR

    structure Identifier : sig

			       type id
   
			       val string2id : string -> id

			   end

    sharing AbstractGrammar.Identifier = Identifier

    structure OkError : OK_ERROR

    sharing OkError.CharInfo = AbstractGrammar.CharInfo

    structure ElaboratorError : ELABORATOR_ERROR

    sharing OkError.Error = ElaboratorError

    structure NameSet : NAME_SET

    sharing ElaboratorError.NameSet = NameSet

    structure Environment : ENVIRONMENT

    sharing Environment.Name = NameSet.Name

    structure Denotable : DENOTABLE

    sharing Environment.Denotable = Denotable

    structure Location : LOCATION

    sharing Denotable.Location = Location

    structure Ttype : TTYPE 

    sharing Denotable.Ttype = Ttype

    structure Store : STORE

    sharing Store.Location = Location

    structure Storable : sig
			     
			     structure Post : sig type 'a post end
			 
			     type storable

			     val functional2sto : (storable -> storable Post.post) -> storable

			 end

    sharing Store.Storable = Storable
 
    structure Post : POST 

    sharing Storable.Post = Post

    val A' :
	(AbstractGrammar.identifier -> NameSet.Name.name OkError.ok) ->

	(AbstractGrammar.declaration -> Environment.environment -> 
	 Location.location_tube -> NameSet.name_set -> 
	 (Environment.environment * Location.location_tube *
	  NameSet.name_set * NameSet.name_set * NameSet.name_set) OkError.ok) ->

	(AbstractGrammar.block -> Environment.environment -> 
	 Location.location_tube -> NameSet.name_set -> 
	 (NameSet.name_set * (Store.store -> Store.store Post.post)) OkError.ok) ->

	AbstractGrammar.block -> Environment.environment -> 
	Location.location_tube -> NameSet.name_set -> 
	(Ttype.ttype * (Store.store -> Storable.storable Post.post)) OkError.ok

end