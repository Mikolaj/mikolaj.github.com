(**************)
(* LinkParser *)
(**************)


signature LINK_PARSER =
sig

    structure CharInfo : CHAR_INFO

    structure Tokens : TOKENS
	  
    structure InfoAndTokenList : INFO_AND_TOKEN_LIST

    sharing InfoAndTokenList.CharInfo = CharInfo
	
    sharing InfoAndTokenList.Tokens = Tokens
	  
    structure Identifier : IDENTIFIER
	  
    sharing Tokens.Identifier = Identifier

    structure Numeral : NUMERAL
 
    sharing Tokens.Numeral = Numeral
	  
    structure Falseval : FALSEVAL
	
    sharing Tokens.Falseval = Falseval
    
    structure LL1Grammar : LL1_GRAMMAR

    sharing LL1Grammar.CharInfo = InfoAndTokenList.CharInfo

    structure OkError : OK_ERROR

    sharing OkError.CharInfo = InfoAndTokenList.CharInfo

    structure ParserError : PARSER_ERROR

    sharing OkError.Error = ParserError

    sharing LL1Grammar.Identifier = Identifier
 
    sharing LL1Grammar.Numeral = Numeral

    sharing LL1Grammar.Falseval = Falseval 

    sharing ParserError.Tokens = Tokens 

    val get_program : InfoAndTokenList.info_and_token_list -> LL1Grammar.program OkError.ok

end

