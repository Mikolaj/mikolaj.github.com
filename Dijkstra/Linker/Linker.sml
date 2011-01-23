(**********)
(* Linker *)
(**********)


functor Linker 

    ( structure CharList' : CHAR_LIST

      structure MultiChar' : MULTI_CHAR
	  
      sharing CharList'.MultiChar = MultiChar' ) : sig

	                                               include LINKER

						       sharing CharList = CharList' 

						       sharing MultiChar = MultiChar'

						   end =
struct

    structure CharList = CharList' 

    structure MultiChar = MultiChar'

    structure LinkLexer = LinkLexer ( structure CharList' = CharList 

				      structure MultiChar' = MultiChar )
 
    structure LinkParser = LinkParser ( structure CharInfo' = LinkLexer.CharInfo

					structure Tokens' = LinkLexer.Tokens
	  
					structure InfoAndTokenList' = LinkLexer.InfoAndTokenList
 
					structure Identifier' = LinkLexer.Identifier

					structure Numeral' = LinkLexer.Numeral
	  
					structure Falseval' = LinkLexer.Falseval ) 

    structure LinkAbstractor = LinkAbstractor ( structure CharInfo' = LinkParser.CharInfo

						structure Identifier' = LinkParser.Identifier
	  
						structure Numeral' = LinkParser.Numeral
						   
						structure Falseval' = LinkParser.Falseval
							  
						structure LL1Grammar' = LinkParser.LL1Grammar )

    structure LinkElaborator = LinkElaborator ( structure CharInfo' = LinkAbstractor.CharInfo

						structure Identifier' = LinkAbstractor.Identifier
	  
						structure Numeral' = LinkAbstractor.Numeral
	  
						structure Falseval' = LinkAbstractor.Falseval

						structure AbstractGrammar' = LinkAbstractor.AbstractGrammar )

    structure OkError_elab = LinkElaborator.OkError

    structure CharInfo = LinkElaborator.CharInfo

    structure ElaboratorError = LinkElaborator.ElaboratorError

    structure NameSet = LinkElaborator.NameSet

    structure Ttype = LinkElaborator.Ttype

    structure OkError_parse = LinkParser.OkError

    structure ParserError = LinkParser.ParserError

    structure Tokens = LinkParser.Tokens

    structure Post = LinkElaborator.Post 

    structure Storable = LinkElaborator.Storable

    fun compile char_list = 
	let
      	    val info_and_token_list = LinkLexer.lex char_list
	    val ll1_program_ok = LinkParser.get_program info_and_token_list
	in
	    case ll1_program_ok 
	      of LinkParser.OkError.OK(ll1_program) =>
		  let		     
		      val abstract_program = LinkAbstractor.abs_program ll1_program
		      val ttype_and_fun_ok = LinkElaborator.P abstract_program
		  in
		      LinkParser.OkError.OK(ttype_and_fun_ok)
		  end
	       | LinkParser.OkError.ERROR(e) => LinkParser.OkError.ERROR(e)
	end

end


