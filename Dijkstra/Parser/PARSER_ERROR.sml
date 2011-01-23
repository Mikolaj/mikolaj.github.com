(***************)
(* ParserError *) 
(***************)


signature PARSER_ERROR =
sig

    structure Tokens : sig type token end

    datatype error = 
	PROGRAM_TOO_SHORT
      | TOKEN_EXPECTED of Tokens.token
      | COMMAND_UNKNOWN of Tokens.token
      | ASSIGNMENT_UNKNOWN of Tokens.token
      | BLOCK_UNKNOWN of Tokens.token
      | DECLARATION_UNKNOWN of Tokens.token
      | TYPE0_ATOM_UNKNOWN of Tokens.token
      | A0_ATOM_UNKNOWN of Tokens.token
      | IDENTIFIER_UNKNOWN of Tokens.token
      | NUMERAL_UNKNOWN of Tokens.token
      | FALSEVAL_UNKNOWN of Tokens.token
      | PROGRAM_TOO_LONG

end 



