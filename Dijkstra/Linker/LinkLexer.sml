(*************)
(* LinkLexer *)
(*************)


functor LinkLexer 

    ( structure CharList' : CHAR_LIST

      structure MultiChar' : MULTI_CHAR

      sharing CharList'.MultiChar = MultiChar' ) : sig

	                                               include LINK_LEXER

						       sharing CharList = CharList' 

						       sharing MultiChar = MultiChar'

						   end =

struct

    structure CharList = CharList' 

    structure MultiChar = MultiChar'

    structure Identifier = Identifier ()
    
    structure Numeral = Numeral ()

    structure Falseval = Falseval ()
				      
    structure Tokens = Tokens ( structure Identifier' = Identifier

				structure Numeral' = Numeral

				structure Falseval' = Falseval )

    structure CharInfo = CharInfo ( structure MultiChar' = MultiChar )

    structure InfoAndTokenList = InfoAndTokenList ( structure Tokens' = Tokens
						   
						    structure CharInfo' = CharInfo )
		   
    structure Word = Word ( structure MultiChar' = MultiChar )

    structure Number = Number ( structure MultiChar' = MultiChar )

    structure Counter = Counter ( structure MultiChar' = MultiChar

				  structure Number' = Number
				
				  structure Tokens' = Tokens

				  structure Numeral' = Numeral )

    structure Recognizer = Recognizer ( structure MultiChar' = MultiChar

					structure Word' = Word
				
					structure Tokens' = Tokens

					structure Identifier' = Identifier

					structure Falseval' = Falseval )

    structure Cutter = Cutter ( structure MultiChar' = MultiChar

				structure CharList' = CharList

				structure Number' = Number

				structure Word' = Word
	
				structure CharInfo' = CharInfo )

	
    structure Lexer = Lexer ( structure Cutter' = Cutter
				
			      structure Tokens' = Tokens

			      structure Counter' = Counter

			      structure Recognizer' = Recognizer

			      structure InfoAndTokenList' = InfoAndTokenList )


    val lex = Lexer.lex 

end

