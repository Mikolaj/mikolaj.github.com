(******************)
(* LinkElaborator *)
(******************)


functor LinkElaborator 

    ( structure CharInfo' : CHAR_INFO

      structure Identifier' : IDENTIFIER
	  
      structure Numeral' : NUMERAL
	  
      structure Falseval' : FALSEVAL

      structure AbstractGrammar' : ABSTRACT_GRAMMAR

      sharing AbstractGrammar'.CharInfo = CharInfo'

      sharing AbstractGrammar'.Identifier = Identifier'

      sharing AbstractGrammar'.Numeral = Numeral'

      sharing AbstractGrammar'.Falseval = Falseval' ) : sig

	                                                    include LINK_ELABORATOR

                                                            sharing CharInfo = CharInfo'

							    sharing Identifier = Identifier'

							    sharing Numeral  = Numeral'

							    sharing Falseval = Falseval'

							    sharing AbstractGrammar = AbstractGrammar'

							end =

struct

    structure CharInfo = CharInfo'

    structure Identifier = Identifier'

    structure Numeral  = Numeral'

    structure Falseval = Falseval'

    structure AbstractGrammar = AbstractGrammar'

    structure Ttype = Ttype ()

    structure Name = Name ()

    structure NameSet = NameSet ( structure Name' = Name )

    structure ElaboratorError = ElaboratorError ( structure NameSet' = NameSet

						  structure Ttype' = Ttype )
						 
    structure OkError = OkError ( structure CharInfo' = CharInfo

				  structure Error' = ElaboratorError )

    structure Boolean = Boolean ()

    structure Integer = Integer ()

    structure Post = Post ( structure CharInfo' = CharInfo )

    structure Storable = Storable ( structure Integer' = Integer

				    structure Boolean' = Boolean

				    structure Post' = Post )

    structure Location = Location ()

    structure Store = Store ( structure Location' = Location
    
			      structure Storable' = Storable )

    structure Denotable = Denotable ( structure Ttype' = Ttype

				      structure Location' = Location )

    structure Environment = Environment ( structure Name' = Name

					  structure Denotable' = Denotable

					  structure Location' = Location )

    structure ElabId = ElabId ( structure AbstractGrammar' = AbstractGrammar

				structure OkError' = OkError

				structure Identifier' = Identifier

				structure Name' = Name )

    structure ElabRightId = ElabRightId ( structure AbstractGrammar' = AbstractGrammar

					  structure OkError' = OkError

					  structure ElaboratorError' = ElaboratorError

					  structure Post' = Post

					  structure Environment' = Environment

					  structure NameSet' = NameSet

					  structure Denotable' = Denotable

					  structure Store' = Store )

    structure ElabNum = ElabNum ( structure AbstractGrammar' = AbstractGrammar

				  structure OkError' = OkError

				  structure Numeral' = Numeral

				  structure Integer' = Integer )

    structure ElabFal = ElabFal ( structure AbstractGrammar' = AbstractGrammar

				  structure OkError' = OkError

				  structure Falseval' = Falseval

				  structure Boolean' = Boolean )

    structure ElabRightBl = ElabRightBl ( structure AbstractGrammar' = AbstractGrammar

					  structure Identifier' = Identifier

					  structure OkError' = OkError

					  structure ElaboratorError' = ElaboratorError

					  structure NameSet' = NameSet

					  structure Environment' = Environment

					  structure Denotable' = Denotable

					  structure Location' = Location

					  structure Ttype' = Ttype

					  structure Store' = Store

					  structure Storable' = Storable

					  structure Post' = Post )

    structure ElabExp = ElabExp ( structure AbstractGrammar' = AbstractGrammar

				  structure OkError' = OkError

				  structure ElaboratorError' = ElaboratorError

				  structure Post' = Post

				  structure Environment' = Environment

				  structure NameSet' = NameSet

				  structure Store' = Store

				  structure Ttype' = Ttype

				  structure Storable' = Storable

				  structure Location' = Location )

    structure ElabTy = ElabTy ( structure AbstractGrammar' = AbstractGrammar

				structure OkError' = OkError

				structure Ttype' = Ttype )

    structure ElabTon = ElabTon ( structure AbstractGrammar' = AbstractGrammar
				 
				  structure OkError' = OkError

				  structure ElaboratorError' = ElaboratorError

				  structure Ttype' = Ttype )

    structure ElabDec = ElabDec ( structure AbstractGrammar' = AbstractGrammar

				  structure OkError' = OkError

				  structure ElaboratorError' = ElaboratorError

				  structure Location' = Location

				  structure Environment' = Environment

				  structure Denotable' = Denotable

				  structure NameSet' = NameSet )

    structure ElabGua = ElabGua ( structure AbstractGrammar' = AbstractGrammar

				  structure OkError' = OkError

				  structure ElaboratorError' = ElaboratorError

				  structure Post' = Post

				  structure Ttype' = Ttype

				  structure Storable' = Storable

				  structure Environment' = Environment

				  structure Location' = Location

				  structure NameSet' = NameSet
   
				  structure Store' = Store )

    structure ElabBl = ElabBl ( structure AbstractGrammar' = AbstractGrammar

				structure OkError' = OkError
	
				structure ElaboratorError' = ElaboratorError

				structure Post' = Post
		            
				structure NameSet' = NameSet

				structure Environment' = Environment

				structure Location' = Location
   
				structure Store' = Store )


    structure ElabCom = ElabCom ( structure AbstractGrammar' = AbstractGrammar

				  structure OkError' = OkError

				  structure ElaboratorError' = ElaboratorError

				  structure Post' = Post

				  structure NameSet' = NameSet

				  structure Environment' = Environment

				  structure Denotable' = Denotable

				  structure Store' = Store

				  structure Location' = Location )

    structure ElabProg = ElabProg ( structure AbstractGrammar' = AbstractGrammar
				   
				    structure OkError' = OkError

				    structure NameSet' = NameSet

				    structure Environment' = Environment

				    structure Denotable' = Denotable

				    structure Location' = Location

				    structure Ttype' = Ttype
 
				    structure Post' = Post

				    structure Storable' = Storable

				    structure Store' = Store )

    val I = ElabId.I

    val R = ElabRightId.R' I

    val N = ElabNum.N

    val F = ElabFal.F

    val T = ElabTy.T

    val V = ElabTon.V' T

    val D = ElabDec.D' I T V

    fun A x = ElabRightBl.A' I D B x

    and E x = ElabExp.E' R A N F x

    and G x = ElabGua.G' E C x

    and B x = ElabBl.B' D C x

    and C x = ElabCom.C' I E G B x

    val P = ElabProg.P' A

end


 

	