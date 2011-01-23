(******************)
(* LinkAbstractor *)
(******************)


functor LinkAbstractor

    ( structure CharInfo' : CHAR_INFO

      structure Identifier' : IDENTIFIER
	  
      structure Numeral' : NUMERAL
	  
      structure Falseval' : FALSEVAL
	
      structure LL1Grammar' : LL1_GRAMMAR
	
      sharing LL1Grammar'.CharInfo = CharInfo'

      sharing LL1Grammar'.Identifier = Identifier'

      sharing LL1Grammar'.Numeral = Numeral'

      sharing LL1Grammar'.Falseval = Falseval' ) : sig

	                                              include LINK_ABSTRACTOR

                                                      sharing CharInfo = CharInfo'

						      sharing Identifier = Identifier'
	  
						      sharing Numeral = Numeral'
	  
						      sharing Falseval = Falseval'

                                                      sharing LL1Grammar = LL1Grammar'
								       
						  end =

struct

    structure CharInfo = CharInfo'

    structure Identifier = Identifier'
	  
    structure Numeral = Numeral'
	  
    structure Falseval = Falseval'
	
    structure LL1Grammar = LL1Grammar'

    structure AbstractGrammar = AbstractGrammar ( structure CharInfo' = CharInfo

						  structure Identifier' = Identifier

						  structure Numeral' = Numeral

						  structure Falseval' = Falseval )

    structure Abstractor = Abstractor ( structure LL1Grammar' = LL1Grammar

					structure AbstractGrammar' = AbstractGrammar )

    val abs_program = Abstractor.abs_program

end