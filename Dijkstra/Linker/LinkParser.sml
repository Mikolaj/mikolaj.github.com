(**************)
(* LinkParser *)
(**************)


functor LinkParser

    ( structure CharInfo' : CHAR_INFO

      structure Tokens' : TOKENS
	  
      structure InfoAndTokenList' : INFO_AND_TOKEN_LIST

      sharing InfoAndTokenList'.CharInfo = CharInfo'

      sharing InfoAndTokenList'.Tokens = Tokens'
 
      structure Identifier' : IDENTIFIER

      sharing Tokens'.Identifier = Identifier'

      structure Numeral' : NUMERAL
 
      sharing Tokens'.Numeral = Numeral'
	  
      structure Falseval' : FALSEVAL
	
      sharing Tokens'.Falseval = Falseval' ) : sig

	                                           include LINK_PARSER

                                                   sharing CharInfo = CharInfo'

						   sharing Tokens = Tokens'
						       
						   sharing InfoAndTokenList = InfoAndTokenList'
 
						   sharing Identifier = Identifier'

						   sharing Numeral = Numeral'
						       
						   sharing Falseval = Falseval'
						     
						end =

struct

    structure CharInfo = CharInfo'

    structure Tokens = Tokens'
	  
    structure InfoAndTokenList = InfoAndTokenList'
 
    structure Identifier = Identifier'

    structure Numeral = Numeral'
	  
    structure Falseval = Falseval'

    structure LL1Grammar = LL1Grammar ( structure CharInfo' = CharInfo

					structure Identifier' = Identifier

					structure Numeral' = Numeral

					structure Falseval' = Falseval )

    structure ParserError = ParserError ( structure Tokens' = Tokens )

    structure OkError = OkError ( structure CharInfo' = CharInfo

				  structure Error' = ParserError )

    structure Parser = Parser ( structure LL1Grammar' = LL1Grammar

				structure OkError' = OkError

				structure ParserError' = ParserError

				structure InfoAndTokenList' = InfoAndTokenList

				structure Tokens' = Tokens )

    val get_program = Parser.get_program

end

