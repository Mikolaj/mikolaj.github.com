(******************)
(* LinkAbstractor *)
(******************)


signature LINK_ABSTRACTOR =
sig

    structure CharInfo : CHAR_INFO

    structure Identifier : IDENTIFIER
	  
    structure Numeral : NUMERAL
	  
    structure Falseval : FALSEVAL

    structure LL1Grammar : LL1_GRAMMAR

    sharing LL1Grammar.CharInfo = CharInfo

    sharing LL1Grammar.Identifier = Identifier

    sharing LL1Grammar.Numeral = Numeral

    sharing LL1Grammar.Falseval = Falseval

    structure AbstractGrammar : ABSTRACT_GRAMMAR

    sharing AbstractGrammar.CharInfo = CharInfo

    sharing AbstractGrammar.Identifier = Identifier

    sharing AbstractGrammar.Numeral = Numeral

    sharing AbstractGrammar.Falseval = Falseval

    val abs_program : LL1Grammar.program -> AbstractGrammar.program

end
