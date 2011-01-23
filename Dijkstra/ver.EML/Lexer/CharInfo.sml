(************)
(* CharInfo *) 
(************)


functor CharInfo  
 
    ( structure MultiChar' : sig 

				datatype blank = 
				    TAB
				  | SPC
				  | RET				
								
				type digit 

				type letter 

				type other   

			    end ) : sig

                                        include CHAR_INFO

                                        sharing MultiChar = MultiChar'

				    end =

struct

    structure MultiChar = MultiChar'

    datatype info = POSITION of int * int

    fun exam_blank (MultiChar.TAB, POSITION(r, c)) = POSITION(r, c + 8)
      | exam_blank (MultiChar.SPC, POSITION(r, c)) = POSITION(r, c + 1)
      | exam_blank (MultiChar.RET, POSITION(r, c)) = POSITION(r + 1, 0)

    fun exam_digit (d, POSITION(r, c)) = POSITION(r, c + 1)

    fun exam_letter (a, POSITION(r, c)) = POSITION(r, c + 1)

    fun exam_other (ot, POSITION(r, c)) = POSITION(r, c + 1)

    val initial_info = POSITION(1, 0)

end 
