(********)
(* Word *) 
(********)


functor Word
 
    ( structure MultiChar' : sig 
				
				eqtype digit 

				eqtype letter 

			    end ) : sig

	                                include WORD

                                        sharing MultiChar = MultiChar'

				    end =

struct

    structure MultiChar = MultiChar'

    datatype word =
	WORD of MultiChar.letter * mix
    and mix = 
	NOT_REALLY
      | SURELY of MultiChar.digit * mix
      | MAYBE of MultiChar.letter * mix

end 
