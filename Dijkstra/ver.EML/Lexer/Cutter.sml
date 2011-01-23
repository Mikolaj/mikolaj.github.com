(**********)
(* Cutter *)
(**********)


functor Cutter
 
    ( structure MultiChar' : sig 

				type blank 
								
				type digit 

				type letter 

				type other   

			    end

      structure CharList' : CHAR_LIST

      sharing CharList'.MultiChar = MultiChar'

      structure Number' : NUMBER
 
      sharing Number'.MultiChar = MultiChar'

      structure Word' : WORD
 
      sharing Word'.MultiChar = MultiChar'

      structure CharInfo' : sig

			       structure MultiChar : sig 

							 type blank 
								
							 type digit 

							 type letter 

							 type other   

						     end

			       eqtype info

			       val exam_blank : MultiChar.blank * info -> info
(* EML *)			       
			       axiom forall (b, i) => (exam_blank (b, i)) proper
(* enough EML *)   
			       val exam_digit : MultiChar.digit * info -> info
(* EML *)
			       axiom forall (d, i) => (exam_digit (d, i)) proper
(* enough EML *)   
			       val exam_letter : MultiChar.letter * info -> info
(* EML *)
			       axiom forall (a, i) => (exam_letter (a, i)) proper
(* enough EML *)   
			       val exam_other : MultiChar.other * info -> info
(* EML *)
			       axiom forall (ot, i) => (exam_other (ot, i)) proper
(* enough EML *)   
			       val initial_info : info
(* Here this
			       axiom initial_info proper
   would have no effect. *)
	
			   end 

      sharing CharInfo'.MultiChar = MultiChar' ) : sig

	                                             include CUTTER

                                                     sharing MultiChar = MultiChar'

						     sharing CharList = CharList'

						     sharing Number = Number'

						     sharing Word = Word'
	
						     sharing CharInfo = CharInfo'

						 end =

struct

    structure MultiChar = MultiChar'

    structure CharList = CharList'

    structure Number = Number'

    structure Word = Word'
	
    structure CharInfo = CharInfo'


local                     
                                      
    open CharList Number Word CharInfo

in             

    datatype sentence =
	END of info
      | CONCRETE of info * number * sentence
      | POETIC of info * word * sentence

    fun cut_mono (DIGIT(d, l1), i) = 
	let
	    val (moo, rest, new_i) = cut_mono (l1, exam_digit (d, i))
	in
	    (WORSE(d, moo), rest, new_i)
	end
      | cut_mono (l, i) = (STRICT, l, i)

    fun cut_mix (DIGIT(d, l1), i) =
	let
	    val (mii, rest, new_i) = cut_mix (l1, exam_digit (d, i))
	in
	    (SURELY(d, mii), rest, new_i)
	end
      | cut_mix (LETTER(a, l1), i) =
	let
	    val (mii, rest, new_i) = cut_mix (l1, exam_letter (a, i))
	in
	    (MAYBE(a, mii), rest, new_i)
	end
      | cut_mix (l, i) = (NOT_REALLY, l , i)

    fun cut_text (NIL, i) = END(i)
      | cut_text (DIGIT(d, l), i) = 
	let
	    val (moo, rest, end_i) = cut_mono (l, exam_digit (d, i))
	in
	    CONCRETE(i, NUMBER(d, moo), cut_text (rest, end_i))
	end
      | cut_text (LETTER(a, l), i) = 
	let
	    val (mii, rest, end_i) = cut_mix (l, exam_letter (a, i))
	in
	    POETIC(i, WORD(a, mii), cut_text (rest, end_i))
	end
      | cut_text (BLANK(b, l), i) = cut_text (l, exam_blank (b, i))
      | cut_text (OTHER(ot, l), i) = cut_text (l, exam_other (ot, i))

    fun cut l = cut_text (l, initial_info)

end            
	
end

