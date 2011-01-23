(*************)
(* LinkLexer *)
(*************)


signature LINK_LEXER =
sig

    structure CharList : CHAR_LIST

    structure MultiChar : MULTI_CHAR

    sharing CharList.MultiChar = MultiChar

    structure CharInfo : CHAR_INFO

    sharing MultiChar = CharInfo.MultiChar

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

    val lex : CharList.char_list -> InfoAndTokenList.info_and_token_list

end

