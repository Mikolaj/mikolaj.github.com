(**************)
(* Recognizer *)
(**************)


signature RECOGNIZER =
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

			      datatype letter =
				  SHOUT
				| D_QUOTE
				| HASH
				| DOLLAR
				| PERCENT
				| ET
				| QUOTE
				| L_PAREN
				| R_PAREN
				| ASTERISK
				| PLUS
				| COMA
				| MINUS
				| DOT
				| SLASH
     
				| COLON
				| SEMICOLON
				| LESS
				| EQUAL
				| GREATER
				| QUESTION
				| MONKEY

				| A
				| B
				| C
				| D
				| E
				| F
				| G
				| H
				| I
				| J
				| K
				| L
				| M
				| N
				| O
				| P
				| Q
				| R
				| S
				| T
				| U
				| V
				| W
				| X
				| Y
				| Z

				| L_SQUARE
				| BACKSLASH
				| R_SQUARE
				| POWER
				| UNDERSCORE
				| BACKQUOTE

				| a 
				| b
				| c
				| d
				| e
				| f
				| g
				| h
				| i
				| j
				| k
				| l
				| m
				| n
				| o
				| p
				| q
				| r
				| s
				| t
				| u
				| v
				| w
				| x
				| y
				| z

				| L_CURLY
				| BAR
				| R_CURLY
				| TILDE

			  end 

    structure Word : WORD

    sharing Word.MultiChar = MultiChar

    structure Tokens : TOKENS

    structure Identifier : sig 
			
			       type id
			 
			       val string2id : string -> id
		      
			   end

    sharing Tokens.Identifier = Identifier

    structure Falseval : sig 
			
			       type fal
			 
			       val bool2fal : bool -> fal
		      
			   end

    sharing Tokens.Falseval = Falseval

    val recognize : Word.word -> Tokens.token

(* EML

    axiom 
	 
    let

	local
	    
	    open MultiChar
	in  

	    nonfix o

	    val letter2string = (fn SHOUT      => "!"
	                          | D_QUOTE    => "\""
				  | HASH       => "#"
				  | DOLLAR     => "$"  
				  | PERCENT    => "%"
				  | ET	       => "&"
				  | QUOTE      => "'"
				  | L_PAREN    => "("
				  | R_PAREN    => ")"
				  | ASTERISK   => "*"
				  | PLUS       => "+"
				  | COMA       => ","
				  | MINUS      => "-"
				  | DOT	       => "."
				  | SLASH      => "/"
				 
	                          | COLON      => ":"
	      			  | SEMICOLON  => ";"
	      			  | LESS       => "<"
	        		  | EQUAL      => "="
	      			  | GREATER    => ">"
	      			  | QUESTION   => "?"
	      			  | MONKEY     => "@"
	      			                     
	       			  | A          => "A"
	      			  | B          => "B"
	      			  | C          => "C"
	      			  | D          => "D"
	      			  | E          => "E"
	      			  | F          => "F"
	      			  | G          => "G"
	      			  | H          => "H"
	      			  | I          => "I"
	      			  | J          => "J"
	      			  | K          => "K"
	      			  | L          => "L"
	      			  | M          => "M"
	      			  | N          => "N"
	        		  | O          => "O"
	      			  | P          => "P"
	      			  | Q          => "Q"
	      			  | R          => "R"
	      			  | S          => "S"
	      			  | T          => "T"
	      			  | U          => "U"
	      			  | V          => "V"
	      			  | W          => "W"
	      			  | X          => "X"
	      			  | Y          => "Y"
	      			  | Z          => "Z"
	      			                     
	      			  | L_SQUARE   => "["
	      			  | BACKSLASH  => "\\"
	      			  | R_SQUARE   => "]"
	      			  | POWER      => "^"
	      			  | UNDERSCORE => "_"
	      			  | BACKQUOTE  => "`"
	      			                     
	      			  | a          => "a"
	      			  | b          => "b"
	      			  | c          => "c"
	      			  | d          => "d"
	      			  | e          => "e"
	      			  | f          => "f"
	      			  | g          => "g"
	       			  | h          => "h"
	      			  | i          => "i"
	      			  | j          => "j"
	      			  | k          => "k"
	      			  | l          => "l"
	      			  | m          => "m"
	      			  | n          => "n"
	      			  | o          => "o"
	      			  | p          => "p"
	      			  | q          => "q"
	      			  | r          => "r"
	      			  | s          => "s"
	      			  | t          => "t"
	      			  | u          => "u"
	      			  | v          => "v"
	      			  | w          => "w"
	      			  | x          => "x"
	      			  | y          => "y"
	      			  | z          => "z"
	      			                     
	      			  | L_CURLY    => "{"
	      			  | BAR        => "|"
	      			  | R_CURLY    => "}"
	      			  | TILDE      => "~")

	    val digit2string = (fn ZERO  => "0"
	                         | ONE   => "1"
				 | TWO   => "2"
				 | TREE  => "3"
				 | FOUR  => "4"
				 | FIVE  => "5"
				 | SIX   => "6"
				 | SEVEN => "7"
				 | EIGHT => "8"
				 | NINE  => "9")

	end	

	fun word2string (Word.WORD(l, mii)) = (letter2string l)^(mix2string mii)
        and mix2string Word.NOT_REALLY = ""
	  | mix2string (Word.SURELY(d, mii)) = (digit2string d)^(mix2string mii)
	  | mix2string (Word.MAYBE(l, mii)) = (letter2string l)^(mix2string mii)

    in 
	
	let
	    
	    open Identifier

	    open Falseval

	    open Tokens

	in
    
	    forall w => recognize w == (fn ";"      => T_SEMICOLON       
		                         | "skip"   => T_SKIP              
					 | "abort"  => T_ABORT
					 | "if"     => T_IF                
					 | "fi"     => T_FI                
					 | "do"     => T_DO                
					 | "od"     => T_OD                
					 | ":vir="  => T_VIR               
					 | ":="     => T_ASSIGN            
					 | "->"     => T_ARROW            
					 | "[]"     => T_TACT               
					 | "begin"  => T_BEGIN             
					 | "end"    => T_END               
					 | "privar" => T_PRIVAR            
					 | "pricon" => T_PRICON            
					 | "virvar" => T_VIRVAR            
					 | "vircon" => T_VIRCON            
					 | "glovar" => T_GLOVAR            
					 | "glocon" => T_GLOCON
					 | ":"      => T_COLON
					 | "int"    => T_TYPE_INT
					 | "bool"   => T_TYPE_BOOL            
					 | "true"   => T_FAL(bool2fal true)       
					 | "false"  => T_FAL(bool2fal false)          
					 | "~"      => T_NEG               
					 | "not"    => T_NOT  
					 | "."      => T_DOT             
					 | "("      => T_LPAREN            
					 | ")"      => T_RPAREN            
					 | "div"    => T_DIV               
					 | "mod"    => T_MOD               
					 | "*"      => T_TIMES             
					 | "+"      => T_PLUS              
					 | "-"      => T_MINUS             
					 | "="      => T_EQUAL             
					 | "<>"     => T_DIFF              
					 | "<"      => T_LESS              
					 | ">"      => T_GREAT             
					 | "<="     => T_EQLESS            
					 | ">="     => T_EQGREAT           
					 | "and"    => T_AND          
					 | "or"     => T_OR            
					 | s        => T_ID(string2id s)) (word2string w)
		
	end

    end

   enough EML *)

end
