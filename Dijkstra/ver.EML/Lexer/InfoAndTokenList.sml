(********************)
(* InfoAndTokenList *) 
(********************)


functor InfoAndTokenList
 
    ( structure Tokens' :  sig type token end 

      structure CharInfo' : sig eqtype info end ) : sig

	                                             include INFO_AND_TOKEN_LIST

                                                     sharing Tokens = Tokens'

						     sharing CharInfo = CharInfo'

						  end =

struct
    
    structure Tokens = Tokens'

    structure CharInfo = CharInfo'

    datatype info_and_token_list = 
	NIL of CharInfo.info
      | CONS of CharInfo.info * Tokens.token * info_and_token_list

end
