(********************)
(* String2MultiChar *)
(********************)


functor String2MultiChar 

    ( structure MultiChar' : MULTI_CHAR 

      structure CharList' : CHAR_LIST

      sharing CharList'.MultiChar = MultiChar' ) : sig

                                                       include STRING2MULTI_CHAR

                                                       sharing MultiChar = MultiChar'

						       sharing CharList = CharList'

						   end =

struct

    structure MultiChar = MultiChar'

    structure CharList = CharList'
	
    local

	local

	    open MultiChar 
	    
	    open CharList

	in  

	    nonfix o

	    fun stringl2mchar nil       = NIL 
	      | stringl2mchar ("!"::sl)  = LETTER(SHOUT     , stringl2mchar sl)    
	      | stringl2mchar ("\""::sl) = LETTER(D_QUOTE   , stringl2mchar sl)   
	      | stringl2mchar ("#"::sl)  = LETTER(HASH      , stringl2mchar sl)
	      | stringl2mchar ("$"::sl)  = LETTER(DOLLAR    , stringl2mchar sl)
	      | stringl2mchar ("%"::sl)  = LETTER(PERCENT   , stringl2mchar sl)
	      | stringl2mchar ("&"::sl)  = LETTER(ET	    , stringl2mchar sl)
	      | stringl2mchar ("'"::sl)  = LETTER(QUOTE     , stringl2mchar sl)
	      | stringl2mchar ("("::sl)  = LETTER(L_PAREN   , stringl2mchar sl)
	      | stringl2mchar (")"::sl)  = LETTER(R_PAREN   , stringl2mchar sl)
	      | stringl2mchar ("*"::sl)  = LETTER(ASTERISK  , stringl2mchar sl)
	      | stringl2mchar ("+"::sl)  = LETTER(PLUS      , stringl2mchar sl)
	      | stringl2mchar (","::sl)  = LETTER(COMA      , stringl2mchar sl)
	      | stringl2mchar ("-"::sl)  = LETTER(MINUS     , stringl2mchar sl)
	      | stringl2mchar ("."::sl)  = LETTER(DOT	    , stringl2mchar sl)
	      | stringl2mchar ("/"::sl)  = LETTER(SLASH     , stringl2mchar sl)
	       			           	  		
	      | stringl2mchar (":"::sl)  = LETTER(COLON     , stringl2mchar sl)
	      | stringl2mchar (";"::sl)  = LETTER(SEMICOLON , stringl2mchar sl)
	      | stringl2mchar ("<"::sl)  = LETTER(LESS      , stringl2mchar sl)
	      | stringl2mchar ("="::sl)  = LETTER(EQUAL     , stringl2mchar sl)
	      | stringl2mchar (">"::sl)  = LETTER(GREATER   , stringl2mchar sl)
	      | stringl2mchar ("?"::sl)  = LETTER(QUESTION  , stringl2mchar sl)
	      | stringl2mchar ("@"::sl)  = LETTER(MONKEY    , stringl2mchar sl)
	       			                     		
	      | stringl2mchar ("A"::sl)  = LETTER(A         , stringl2mchar sl)
	      | stringl2mchar ("B"::sl)  = LETTER(B         , stringl2mchar sl)
	      | stringl2mchar ("C"::sl)  = LETTER(C         , stringl2mchar sl)
	      | stringl2mchar ("D"::sl)  = LETTER(D         , stringl2mchar sl)
	      | stringl2mchar ("E"::sl)  = LETTER(E         , stringl2mchar sl)
	      | stringl2mchar ("F"::sl)  = LETTER(F         , stringl2mchar sl)
	      | stringl2mchar ("G"::sl)  = LETTER(G         , stringl2mchar sl)
	      | stringl2mchar ("H"::sl)  = LETTER(H         , stringl2mchar sl)
	      | stringl2mchar ("I"::sl)  = LETTER(I         , stringl2mchar sl)
	      | stringl2mchar ("J"::sl)  = LETTER(J         , stringl2mchar sl)
	      | stringl2mchar ("K"::sl)  = LETTER(K         , stringl2mchar sl)
	      | stringl2mchar ("L"::sl)  = LETTER(L         , stringl2mchar sl)
	      | stringl2mchar ("M"::sl)  = LETTER(M         , stringl2mchar sl)
	      | stringl2mchar ("N"::sl)  = LETTER(N         , stringl2mchar sl)
	      | stringl2mchar ("O"::sl)  = LETTER(O         , stringl2mchar sl)
	      | stringl2mchar ("P"::sl)  = LETTER(P         , stringl2mchar sl)
	      | stringl2mchar ("Q"::sl)  = LETTER(Q         , stringl2mchar sl)
	      | stringl2mchar ("R"::sl)  = LETTER(R         , stringl2mchar sl)
	      | stringl2mchar ("S"::sl)  = LETTER(S         , stringl2mchar sl)
	      | stringl2mchar ("T"::sl)  = LETTER(T         , stringl2mchar sl)
	      | stringl2mchar ("U"::sl)  = LETTER(U         , stringl2mchar sl)
	      | stringl2mchar ("V"::sl)  = LETTER(V         , stringl2mchar sl)
	      | stringl2mchar ("W"::sl)  = LETTER(W         , stringl2mchar sl)
	      | stringl2mchar ("X"::sl)  = LETTER(X         , stringl2mchar sl)
	      | stringl2mchar ("Y"::sl)  = LETTER(Y         , stringl2mchar sl)
	      | stringl2mchar ("Z"::sl)  = LETTER(Z         , stringl2mchar sl)
	       			                     		
	      | stringl2mchar ("["::sl)  = LETTER(L_SQUARE  , stringl2mchar sl)
	      | stringl2mchar ("\\"::sl) = LETTER(BACKSLASH , stringl2mchar sl)
	      | stringl2mchar ("]"::sl)  = LETTER(R_SQUARE  , stringl2mchar sl)
	      | stringl2mchar ("^"::sl)  = LETTER(POWER     , stringl2mchar sl)
	      | stringl2mchar ("_"::sl)  = LETTER(UNDERSCORE, stringl2mchar sl)
	      | stringl2mchar ("`"::sl)  = LETTER(BACKQUOTE , stringl2mchar sl)
	       			                     		
	      | stringl2mchar ("a"::sl)  = LETTER(a         , stringl2mchar sl)
	      | stringl2mchar ("b"::sl)  = LETTER(b         , stringl2mchar sl)
	      | stringl2mchar ("c"::sl)  = LETTER(c         , stringl2mchar sl)
	      | stringl2mchar ("d"::sl)  = LETTER(d         , stringl2mchar sl)
	      | stringl2mchar ("e"::sl)  = LETTER(e         , stringl2mchar sl)
	      | stringl2mchar ("f"::sl)  = LETTER(f         , stringl2mchar sl)
	      | stringl2mchar ("g"::sl)  = LETTER(g         , stringl2mchar sl)
	      | stringl2mchar ("h"::sl)  = LETTER(h         , stringl2mchar sl)
	      | stringl2mchar ("i"::sl)  = LETTER(i         , stringl2mchar sl)
	      | stringl2mchar ("j"::sl)  = LETTER(j         , stringl2mchar sl)
	      | stringl2mchar ("k"::sl)  = LETTER(k         , stringl2mchar sl)
	      | stringl2mchar ("l"::sl)  = LETTER(l         , stringl2mchar sl)
	      | stringl2mchar ("m"::sl)  = LETTER(m         , stringl2mchar sl)
	      | stringl2mchar ("n"::sl)  = LETTER(n         , stringl2mchar sl)
	      | stringl2mchar ("o"::sl)  = LETTER(o         , stringl2mchar sl)
	      | stringl2mchar ("p"::sl)  = LETTER(p         , stringl2mchar sl)
	      | stringl2mchar ("q"::sl)  = LETTER(q         , stringl2mchar sl)
	      | stringl2mchar ("r"::sl)  = LETTER(r         , stringl2mchar sl)
	      | stringl2mchar ("s"::sl)  = LETTER(s         , stringl2mchar sl)
	      | stringl2mchar ("t"::sl)  = LETTER(t         , stringl2mchar sl)
	      | stringl2mchar ("u"::sl)  = LETTER(u         , stringl2mchar sl)
	      | stringl2mchar ("v"::sl)  = LETTER(v         , stringl2mchar sl)
	      | stringl2mchar ("w"::sl)  = LETTER(w         , stringl2mchar sl)
	      | stringl2mchar ("x"::sl)  = LETTER(x         , stringl2mchar sl)
	      | stringl2mchar ("y"::sl)  = LETTER(y         , stringl2mchar sl)
	      | stringl2mchar ("z"::sl)  = LETTER(z         , stringl2mchar sl)
	       			                     		
	      | stringl2mchar ("{"::sl)  = LETTER(L_CURLY   , stringl2mchar sl)
	      | stringl2mchar ("|"::sl)  = LETTER(BAR       , stringl2mchar sl)
	      | stringl2mchar ("}"::sl)  = LETTER(R_CURLY   , stringl2mchar sl)
	      | stringl2mchar ("~"::sl)  = LETTER(TILDE     , stringl2mchar sl)
	       			  				
	      | stringl2mchar ("0"::sl)  = DIGIT(ZERO       , stringl2mchar sl)
	      | stringl2mchar ("1"::sl)  = DIGIT(ONE  	    , stringl2mchar sl)
	      | stringl2mchar ("2"::sl)  = DIGIT(TWO        , stringl2mchar sl)
	      | stringl2mchar ("3"::sl)  = DIGIT(TREE 	    , stringl2mchar sl)
	      | stringl2mchar ("4"::sl)  = DIGIT(FOUR 	    , stringl2mchar sl)
	      | stringl2mchar ("5"::sl)  = DIGIT(FIVE 	    , stringl2mchar sl)
	      | stringl2mchar ("6"::sl)  = DIGIT(SIX  	    , stringl2mchar sl)
	      | stringl2mchar ("7"::sl)  = DIGIT(SEVEN	    , stringl2mchar sl)
	      | stringl2mchar ("8"::sl)  = DIGIT(EIGHT	    , stringl2mchar sl)
	      | stringl2mchar ("9"::sl)  = DIGIT(NINE 	    , stringl2mchar sl)
	       			  				
	      | stringl2mchar ("\t"::sl) = BLANK(TAB	    , stringl2mchar sl)
	      | stringl2mchar (" "::sl)  = BLANK(SPC	    , stringl2mchar sl)
	      | stringl2mchar ("\n"::sl) = BLANK(RET	    , stringl2mchar sl)
	       			  				
	      | stringl2mchar (_::sl)    = OTHER(OOOTHER    , stringl2mchar sl)

	end							 
 						 
(* SML		

	val standard_explode = explode

   enough SML *)

	fun standard_explode s = map str (explode s) 		

    in

	fun string2mchar s = stringl2mchar (standard_explode s)

    end

end

