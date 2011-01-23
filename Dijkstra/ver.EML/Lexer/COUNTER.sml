(***********)
(* Counter *)
(***********)


signature COUNTER =
sig

    structure MultiChar : sig 

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

    structure Number : NUMBER 
 
    sharing Number.MultiChar = MultiChar

    structure Tokens : TOKENS

    structure Numeral : sig 
			
			    type num
			   
			    val int2num : int -> num
		      
			end

    sharing Tokens.Numeral = Numeral 

    val count : Number.number -> Tokens.token

(* EML *)

    axiom forall n =>  
	let

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
				
	    forall n => count n == Tokens.T_NUM(Numeral.int2num (number2int n))

	end

(* enough EML *)

end
