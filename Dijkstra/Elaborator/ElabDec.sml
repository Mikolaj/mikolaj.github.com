(***********)
(* ElabDec *)
(***********)


functor ElabDec

    ( structure AbstractGrammar' : ABSTRACT_GRAMMAR

      structure OkError' : OK_ERROR

      sharing OkError'.CharInfo = AbstractGrammar'.CharInfo

      structure ElaboratorError' : ELABORATOR_ERROR

      sharing OkError'.Error = ElaboratorError'

      structure Location' : LOCATION

      structure Environment' : ENVIRONMENT

      structure Denotable' : DENOTABLE

      sharing ElaboratorError'.Ttype = Denotable'.Ttype

      sharing Denotable'.Location = Location'

      sharing Environment'.Denotable = Denotable'

      structure NameSet' : NAME_SET

      sharing ElaboratorError'.NameSet = NameSet'

      sharing Environment'.Name = NameSet'.Name ) : sig

	                                                include ELAB_DEC 

                                                        sharing AbstractGrammar = AbstractGrammar'

							sharing OkError = OkError'

							sharing ElaboratorError = ElaboratorError'

							sharing Location = Location'

							sharing Environment = Environment'

							sharing Denotable = Denotable'

							sharing NameSet = NameSet'

						    end = 

struct

    structure AbstractGrammar = AbstractGrammar'

    structure OkError = OkError'

    structure ElaboratorError = ElaboratorError'

    structure Location = Location'

    structure Environment = Environment'

    structure Denotable = Denotable'

    structure NameSet = NameSet'

local

    open AbstractGrammar 

    open OkError

    open Location

    open Denotable

    open Environment

    open NameSet

    open ElaboratorError

in

    fun single_env nam v = update_env nam v (empty_env FREE)

    fun D' I T V =

  let
         
    fun D(PRIVAR(i, id, ty)) e (TUBE(l, t)) vir = 
	(case I(id)
	  of OK(nam) =>	
	      (case T(ty)
		 of OK(tau) => OK(single_env nam (VAR(tau, l)), t(), 
				  singleton nam, empty, empty)
		  | ERROR(e) => ERROR(e))				   
	   | ERROR(e) => ERROR(e))				   
      | D(PRICON(i, id, ty)) e (TUBE(l, t)) vir = 
	(case I(id)
	  of OK(nam) =>	
	      (case T(ty)
		 of OK(tau) => OK(single_env nam (CON(tau, l)), t(),
				  singleton nam, empty, empty)
		  | ERROR(e) => ERROR(e))				   
	   | ERROR(e) => ERROR(e))				   
      | D(VIRVAR(i, id, type_or_not)) e t vir = 
	(case I(id)
	  of OK(nam) =>	 
	      if not (is_in_one nam vir) then ERROR(i, VAR_IS_NOT_VIRGIN_DEC(nam))
	      else (case access_env nam e
		      of VAR(tau, l) => 
			  (case V(type_or_not) tau
			     of OK(tau1) => OK(single_env nam (VAR(tau1, l)), t, 
					       empty, singleton nam, empty)
			      | ERROR(e) => ERROR(e))
		       | CON _ => ERROR(i, VAR_IS_CONSTANT_DEC(nam)) 		
		       | FREE => ERROR(i, VAR_NOT_KNOWN_DEC(nam)))
	   | ERROR(e) => ERROR(e))
      | D(VIRCON(i, id, type_or_not)) e t vir = 
	(case I(id)
	  of OK(nam) =>	 
	      if not (is_in_one nam vir) then ERROR(i, VAR_IS_NOT_VIRGIN_DEC(nam))
	      else (case access_env nam e
		      of VAR(tau, l) => 
			  (case V(type_or_not) tau
			     of OK(tau1) => OK(single_env nam (CON(tau1, l)), t, 
					       empty, singleton nam, empty)
			      | ERROR(e) => ERROR(e))
		       | CON(tau, l) =>  
			  (case V(type_or_not) tau
			     of OK(tau1) => OK(single_env nam (CON(tau1, l)), t, 
					       empty, singleton nam, empty)
			      | ERROR(e) => ERROR(e))
		       | FREE => ERROR(i, VAR_NOT_KNOWN_DEC(nam)))
	   | ERROR(e) => ERROR(e))
      | D(GLOVAR(i, id, type_or_not)) e t vir = 
	(case I(id)
	  of OK(nam) =>	 
	      if is_in_one nam vir then ERROR(i, VAR_IS_VIRGIN_DEC(nam))
	      else (case access_env nam e
		      of VAR(tau, l) => 
			  (case V(type_or_not) tau
			     of OK(tau1) => OK(single_env nam (VAR(tau1, l)), t, 
					       empty, empty, singleton nam)
			      | ERROR(e) => ERROR(e))
		       | CON _ => ERROR(i, VAR_IS_CONSTANT_DEC(nam)) 		
		       | FREE => ERROR(i, VAR_NOT_KNOWN_DEC(nam)))
	   | ERROR(e) => ERROR(e))
      | D(GLOCON(i, id, type_or_not)) e t vir = 
	(case I(id)
	  of OK(nam) =>	 
	      if is_in_one nam vir then ERROR(i, VAR_IS_VIRGIN_DEC(nam))
	      else (case access_env nam e
		      of VAR(tau, l) => 
			  (case V(type_or_not) tau
			     of OK(tau1) => OK(single_env nam (CON(tau1, l)), t, 
					       empty, empty, singleton nam)
			      | ERROR(e) => ERROR(e))
		       | CON(tau, l) =>  
			  (case V(type_or_not) tau
			     of OK(tau1) => OK(single_env nam (CON(tau1, l)), t, 
					       empty, empty, singleton nam)
			      | ERROR(e) => ERROR(e))
		       | FREE => ERROR(i, VAR_NOT_KNOWN_DEC(nam)))
	   | ERROR(e) => ERROR(e))	   
      | D(COMPOUND(i, dec1, dec2)) e t vir =
	(case D(dec1) e t vir 
	  of OK(e1, t1, pri1, vir1, glo1) =>
	      (case D(dec2) e t1 vir 
		 of OK(e2, t2, pri2, vir2, glo2) =>
		     let
			 val new2 = plus (pri2, plus(vir2, glo2))
		     in
			 OK(plus_env FREE e1 e2, t2,
			    plus (minus (pri1, new2), pri2), 
			    plus (minus (vir1, new2), vir2), 
			    plus (minus (glo1, new2), glo2))
		     end
		  | ERROR(e) => ERROR(e))
	   | ERROR(e) => ERROR(e))  
	
  in
      D
  end

end

end