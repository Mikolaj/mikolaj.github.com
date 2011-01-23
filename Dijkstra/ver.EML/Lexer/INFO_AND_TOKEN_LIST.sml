(********************)
(* InfoAndTokenList *) 
(********************)


signature INFO_AND_TOKEN_LIST = 
sig

    structure Tokens :  sig type token end 

    structure CharInfo : sig eqtype info end 

    datatype info_and_token_list = 
	NIL of CharInfo.info
      | CONS of CharInfo.info * Tokens.token * info_and_token_list
	      
end
