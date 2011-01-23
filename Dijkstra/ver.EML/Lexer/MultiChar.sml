(*************)
(* MultiChar *)
(*************)


functor MultiChar () : MULTI_CHAR = 
struct

    datatype blank = 
	TAB
      | SPC
      | RET

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

    datatype other =
	OOOTHER

end
