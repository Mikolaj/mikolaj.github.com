(**********)
(* Cutter *)
(**********)


signature CUTTER =  
sig

    structure MultiChar : sig 

			      type blank 
								
			      type digit 

			      type letter 

			      type other   

			  end

    structure CharList : CHAR_LIST

    sharing CharList.MultiChar = MultiChar

    structure Number : NUMBER
 
    sharing Number.MultiChar = MultiChar

    structure Word : WORD
 
    sharing Word.MultiChar = MultiChar

    structure CharInfo : sig

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

    sharing CharInfo.MultiChar = MultiChar


local                     
                                      
    open CharList Number Word CharInfo

in             

    datatype sentence =
	END of info
      | CONCRETE of info * number * sentence
      | POETIC of info * word * sentence

    val cut_mono : char_list * info -> mono * char_list * info 

(* EML *)

    axiom forall (l, i) =>
(* Here this
	(cut_mono (l, i)) proper andalso
   would have no effect. *)
	(case (cut_mono (l, i))
	   of (STRICT, rest, new_i) =>
	       not (exists (l1, d) => l == DIGIT(d, l1)) andalso 
	       rest == l andalso new_i == i
	    | (WORSE(d, moo), rest, new_i) => 
	       (exists l1 => 
		   l == DIGIT(d, l1) andalso
		   (moo, rest, new_i) == cut_mono (l1, exam_digit (d, i))))

(* enough EML *)

    val cut_mix : char_list * info -> mix * char_list * info

(* EML *)

    axiom forall (l, i) =>
	(case (cut_mix (l, i))
	   of (NOT_REALLY, rest, new_i) => 
	       not (exists (l1, d) => l == DIGIT(d, l1)) andalso 
	       not (exists (l1, a) => l == LETTER(a, l1)) andalso 
	       rest == l andalso new_i == i
	    | (SURELY(d, mii), rest, new_i) => 
	       (exists l1 => 
		   l == DIGIT(d, l1) andalso
		   (mii, rest, new_i) == cut_mix (l1, exam_digit (d, i)))
	    | (MAYBE(a, mii), rest, new_i) => 
	       (exists l1 => 
		   l == LETTER(a, l1) andalso
		   (mii, rest, new_i) == cut_mix (l1, exam_letter (a, i))))

(* enough EML *)

    val cut_text : char_list * info -> sentence

(* EML *)

    axiom 
	let

	    fun append (NIL, l2) = l2
	      | append (BLANK(b, l), l2) = BLANK(b, append (l, l2))
	      | append (DIGIT(d, l), l2) = DIGIT(d, append (l, l2))
	      | append (LETTER(a, l), l2) = LETTER(a, append (l, l2))
	      | append (OTHER(ot, l), l2) = OTHER(ot, append (l, l2))

	    fun is_trash NIL = true
	      | is_trash (BLANK(b, l)) = is_trash l
	      | is_trash (OTHER(ot, l)) = is_trash l
	      | is_trash _ = false

	    exception NotTrash

	    fun exam_trash (NIL, i) = i
	      | exam_trash (BLANK(b, l), i) = exam_trash (l, exam_blank (b, i)) 
	      | exam_trash (OTHER(ot, l), i) = exam_trash (l, exam_other (ot, i))
	      | exam_trash _ = raise NotTrash

	in

	    forall (l, i) =>
		(case (cut_text (l, i))
		   of END(end_i) => l == NIL andalso end_i == i
		    | CONCRETE(i, n, s) => 
		       (exists (l1, DIGIT(d, l2)) =>
			   l == (append (l1, DIGIT(d, l2))) andalso
			   (is_trash l1) andalso
			   let
			       val start_i = exam_trash (l1, i)
			       val (moo, rest, end_i) = cut_mono (l2, exam_digit (d, start_i))
			   in
			       n == NUMBER(d, moo) andalso
			       i == start_i andalso
			       s == cut_text (rest, end_i)
			   end)
		    | POETIC(i, w, s) => 
		       (exists (l1, LETTER(a, l2)) =>
			   l == (append (l1, LETTER(a, l2))) andalso
			   (is_trash l1) andalso
			   let
			       val start_i = exam_trash (l1, i)
			       val (mii, rest, end_i) = cut_mix (l2, exam_letter (a, start_i))
			   in
			       w == WORD(a, mii) andalso
			       i == start_i andalso
			       s == cut_text (rest, end_i)
			   end))

	end

(* enough EML *)

    val cut : char_list -> sentence

(* EML *)

    axiom forall l => cut l == cut_text (l, initial_info)

(* enough EML *)


end            
	
end

