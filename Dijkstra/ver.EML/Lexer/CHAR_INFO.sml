(************)
(* CharInfo *) 
(************)


signature CHAR_INFO = 
sig 
	

    structure MultiChar : sig 

			      datatype blank = 
				  TAB
				| SPC
				| RET	
								
			      type digit 

			      type letter 

			      type other   

			  end

    datatype info = POSITION of int * int

    val exam_blank : MultiChar.blank * info -> info

(* EML *)			       

    axiom 
        let
	    fun exam_blank' (MultiChar.TAB, POSITION(r, c)) = POSITION(r, c + 8)
	      | exam_blank' (MultiChar.SPC, POSITION(r, c)) = POSITION(r, c + 1)
	      | exam_blank' (MultiChar.RET, POSITION(r, c)) = POSITION(r + 1, 0)
	in
	    forall (b, gi) => exam_blank (b, gi) = exam_blank' (b, gi)
	end

(* enough EML *)   
    
    val exam_digit : MultiChar.digit * info -> info

(* EML *)

    axiom forall (d, POSITION(r, c)) => 
	(exam_digit (d, POSITION(r, c))) proper andalso
	exam_digit (d, POSITION(r, c)) = POSITION(r, c + 1)

(* enough EML *)   

    val exam_letter : MultiChar.letter * info -> info

(* EML *)

    axiom forall (a, POSITION(r, c)) => 
	(exam_letter (a, POSITION(r, c))) proper andalso
	exam_letter (a, POSITION(r, c)) = POSITION(r, c + 1)

(* enough EML *)   

    val exam_other : MultiChar.other * info -> info

(* EML *)

    axiom forall (ot, POSITION(r, c)) => 
	(exam_other (ot, POSITION(r, c))) proper andalso
	exam_other (ot, POSITION(r, c)) = POSITION(r, c + 1)

(* enough EML *)   

    val initial_info : info

(* EML *)

    axiom initial_info = POSITION(1, 0)

(* enough EML *)

end 
