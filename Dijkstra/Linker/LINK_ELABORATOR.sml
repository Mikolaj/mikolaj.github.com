(******************)
(* LinkElaborator *)
(******************)


signature LINK_ELABORATOR =
sig
    
    structure CharInfo : CHAR_INFO

    structure Identifier : IDENTIFIER
	  
    structure Numeral : NUMERAL
	
    structure Falseval : FALSEVAL

    structure AbstractGrammar : ABSTRACT_GRAMMAR

    sharing AbstractGrammar.CharInfo = CharInfo

    sharing AbstractGrammar.Identifier = Identifier

    sharing AbstractGrammar.Numeral = Numeral

    sharing AbstractGrammar.Falseval = Falseval

    structure Ttype : TTYPE

    structure Name : NAME

    structure NameSet : NAME_SET

    sharing NameSet.Name = Name

    structure ElaboratorError : ELABORATOR_ERROR 

    sharing ElaboratorError.Ttype = Ttype

    sharing ElaboratorError.NameSet = NameSet

    structure OkError : OK_ERROR 

    sharing OkError.CharInfo = CharInfo

    sharing OkError.Error = ElaboratorError

    structure Boolean : BOOLEAN

    structure Integer : INTEGER

    structure Post : POST

    sharing Post.CharInfo = CharInfo

    structure Storable : STORABLE

    sharing Storable.Boolean = Boolean

    sharing Storable.Integer = Integer

    sharing Storable.Post = Post

    val P : 
	AbstractGrammar.program -> 
	(Ttype.ttype * (Storable.storable -> Storable.storable Post.post)) OkError.ok

end