(************)
(* CharList *) 
(************)


functor CharList  
 
    ( structure MultiChar' : sig 

				eqtype blank 
								
				eqtype digit 

				eqtype letter 

				eqtype other   

			    end ) : sig

	                                include CHAR_LIST

                                        sharing MultiChar = MultiChar'

				    end =

struct

    structure MultiChar = MultiChar'

    datatype char_list =
	NIL
      | BLANK of MultiChar.blank * char_list
      | DIGIT of MultiChar.digit * char_list
      | LETTER of MultiChar.letter * char_list
      | OTHER of MultiChar.other * char_list

end