(************)
(* CharList *) 
(************)


signature CHAR_LIST = 
sig
 
    structure MultiChar : sig 

			      eqtype blank 
								
			      eqtype digit 

			      eqtype letter 

			      eqtype other   

			  end

    datatype char_list =
	NIL
      | BLANK of MultiChar.blank * char_list
      | DIGIT of MultiChar.digit * char_list
      | LETTER of MultiChar.letter * char_list
      | OTHER of MultiChar.other * char_list

end