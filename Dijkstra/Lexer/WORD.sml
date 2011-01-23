(********)
(* Word *) 
(********)


signature WORD = 
sig 
	
    structure MultiChar : sig 

			      eqtype digit 

			      eqtype letter 

			  end

    datatype word =
	WORD of MultiChar.letter * mix
    and mix = 
	NOT_REALLY
      | SURELY of MultiChar.digit * mix
      | MAYBE of MultiChar.letter * mix

end 
