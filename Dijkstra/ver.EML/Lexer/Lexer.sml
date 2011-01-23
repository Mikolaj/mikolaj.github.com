(*********)
(* Lexer *)
(*********)


functor Lexer
 
    ( structure Cutter' : sig
	  
			     structure CharList :  sig type char_list end 	

			     structure Number : sig type number end

			     structure Word : sig type word end

			     structure CharInfo : sig eqtype info end 
 
			     datatype sentence =
				 END of CharInfo.info
			       | CONCRETE of CharInfo.info * Number.number * sentence
			       | POETIC of CharInfo.info * Word.word * sentence

			     val cut : CharList.char_list -> sentence
(* EML *)
                             axiom forall l => (cut l) proper
(* enough EML *)
			 end 

      structure Tokens' : sig type token end

      structure Counter' : sig

			      structure Number : sig type number end

			      structure Tokens : sig type token end 
	
			      val count : Number.number -> Tokens.token
(* EML *)
                              axiom forall n => (count n) proper
(* enough EML *)
			  end

      sharing Counter'.Number = Cutter'.Number

      sharing Counter'.Tokens = Tokens'
      
      structure Recognizer' : sig

				 structure Word : sig type word end

				 structure Tokens : sig type token end 
	
				 val recognize : Word.word -> Tokens.token
(* EML *)
                                 axiom forall w => (recognize w) proper
(* enough EML *)
			     end 

      sharing Recognizer'.Word = Cutter'.Word

      sharing Recognizer'.Tokens = Tokens'

      structure InfoAndTokenList' : INFO_AND_TOKEN_LIST

      sharing InfoAndTokenList'.Tokens = Tokens'

      sharing InfoAndTokenList'.CharInfo = Cutter'.CharInfo ) : sig

	                                                          include LEXER

                                                                  sharing Cutter = Cutter'
				
								  sharing Tokens = Tokens'

								  sharing Counter = Counter'

								  sharing Recognizer = Recognizer'

								 sharing InfoAndTokenList = InfoAndTokenList'

							      end =

struct

    structure Cutter = Cutter'
				
    structure Tokens = Tokens'

    structure Counter = Counter'

    structure Recognizer = Recognizer'

    structure InfoAndTokenList = InfoAndTokenList'

local

    open Cutter InfoAndTokenList

in

    fun map (END(gi), nf, wf) = NIL(gi)
      | map (CONCRETE(i, n, s), nf, wf) = CONS(i, (nf n), map(s, nf, wf))
      | map (POETIC(i, w, s), nf, wf) = CONS(i, (wf w), map(s, nf, wf))

    fun lex l = map (Cutter.cut l, Counter.count, Recognizer.recognize)

end

end