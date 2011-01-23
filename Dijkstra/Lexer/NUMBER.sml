(**********)
(* Number *) 
(**********)


signature NUMBER = 
sig 
				
    structure MultiChar : sig eqtype digit end

    datatype number =
	NUMBER of MultiChar.digit * mono
    and mono =
	STRICT
      | WORSE of MultiChar.digit * mono

end 