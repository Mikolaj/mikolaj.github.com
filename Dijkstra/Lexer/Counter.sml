(***********)
(* Counter *)
(***********)


functor Counter
 
    ( structure MultiChar' : sig 

				datatype digit =
				    ZERO
				  | ONE
				  | TWO
				  | TREE
				  | FOUR
				  | FIVE
				  | SIX
				  | SEVEN
				  | EIGHT
				  | NINE

			    end 

      structure Number' : NUMBER
 
      sharing Number'.MultiChar = MultiChar'

      structure Tokens' : TOKENS 

      structure Numeral' : sig 
			
			      type num
			   
			      val int2num : int -> num
		      
			  end

      sharing Tokens'.Numeral = Numeral' ) : sig

	                                       include COUNTER

                                               sharing MultiChar = MultiChar'

					       sharing Number = Number'
				
					       sharing Tokens = Tokens'

					       sharing Numeral = Numeral'

					   end =

struct

    structure MultiChar = MultiChar'

    structure Number = Number'
				
    structure Tokens = Tokens'

    structure Numeral = Numeral'

    local

	open MultiChar

	val digit2int = fn ZERO  => 0
                         | ONE   => 1
			 | TWO   => 2
			 | TREE  => 3
			 | FOUR  => 4
			 | FIVE  => 5
			 | SIX   => 6
			 | SEVEN => 7
			 | EIGHT => 8
			 | NINE  => 9
	    
	fun number2int (Number.NUMBER(d, moo)) = mono2int (moo, digit2int d)
	and mono2int (Number.STRICT, mem) = mem
	  | mono2int (Number.WORSE(d, moo), mem) = mono2int (moo, digit2int d + 10 * mem)
				
    in
				
	fun count n = Tokens.T_NUM(Numeral.int2num (number2int n))

    end


end
