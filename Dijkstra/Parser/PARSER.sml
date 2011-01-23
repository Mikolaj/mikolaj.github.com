(**********)
(* Parser *)
(**********)


signature PARSER =
sig

    structure LL1Grammar : LL1_GRAMMAR

    structure OkError : OK_ERROR

    structure ParserError : PARSER_ERROR

    sharing OkError.Error = ParserError

    structure InfoAndTokenList : INFO_AND_TOKEN_LIST

    sharing LL1Grammar.CharInfo = InfoAndTokenList.CharInfo

    sharing OkError.CharInfo = InfoAndTokenList.CharInfo

    structure Tokens : TOKENS

    sharing ParserError.Tokens = Tokens

    sharing InfoAndTokenList.Tokens = Tokens 

    val get_program : InfoAndTokenList.info_and_token_list -> LL1Grammar.program OkError.ok

end