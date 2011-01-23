(**************)
(* Abstractor *)
(**************)


signature ABSTRACTOR =
sig

    structure LL1Grammar : LL1_GRAMMAR

    structure AbstractGrammar : ABSTRACT_GRAMMAR

    sharing LL1Grammar.CharInfo = AbstractGrammar.CharInfo

    val abs_program : LL1Grammar.program -> AbstractGrammar.program 

end