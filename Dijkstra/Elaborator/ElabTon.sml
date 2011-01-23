(***********)
(* ElabTon *)
(***********)


functor ElabTon
    
    ( structure AbstractGrammar' : ABSTRACT_GRAMMAR

      structure OkError' : OK_ERROR

      sharing OkError'.CharInfo = AbstractGrammar'.CharInfo

      structure ElaboratorError' : ELABORATOR_ERROR

      sharing OkError'.Error = ElaboratorError'

      structure Ttype' : TTYPE

      sharing ElaboratorError'.Ttype = Ttype' ) : sig

                                  	              include ELAB_TON 

                                                      sharing AbstractGrammar = AbstractGrammar'

						      sharing OkError = OkError'

						      sharing ElaboratorError = ElaboratorError'

						      sharing Ttype = Ttype'

						  end = 

struct

    structure AbstractGrammar = AbstractGrammar'

    structure OkError = OkError'

    structure ElaboratorError = ElaboratorError'

    structure Ttype = Ttype'

local

    open AbstractGrammar 

    open OkError

    open Ttype

    open ElaboratorError

in

    fun V' T =

  let
         
    fun V(EPSton) tau1 = OK(tau1)
      | V(COLON(i, type_exp)) tau1 = 
	case T(type_exp)
	  of OK(tau2) => (case (tau1, tau2)
			    of (UNKNOWN, tau2) => OK(tau2)
			     | (tau1, UNKNOWN)  => OK(tau1)
			     | _ =>  (if tau1 = tau2 then OK(tau1) 
				      else ERROR(i, TYPE_CLASH_TON(tau1, tau2))))
	   | ERROR(e) => ERROR(e)

  in
      V
  end

end

end

