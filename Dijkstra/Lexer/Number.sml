(**********)
(* Number *) 
(**********)


functor Number  
 
    ( structure MultiChar' : sig eqtype digit end ) : sig

	                                               include NUMBER

                                                       sharing MultiChar = MultiChar'

						   end =

struct

    structure MultiChar = MultiChar'

    datatype number =
	NUMBER of MultiChar.digit * mono
    and mono =
	STRICT
      | WORSE of MultiChar.digit * mono

end 