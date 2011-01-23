(***********)
(* ElabCom *)
(***********)


functor ElabCom

    ( structure AbstractGrammar' : ABSTRACT_GRAMMAR

      structure OkError' : OK_ERROR

      sharing OkError'.CharInfo = AbstractGrammar'.CharInfo

      structure ElaboratorError' : ELABORATOR_ERROR

      sharing OkError'.Error = ElaboratorError'
     
      structure Post' : POST

      sharing Post'.CharInfo = AbstractGrammar'.CharInfo

      structure NameSet' : NAME_SET

      sharing ElaboratorError'.NameSet = NameSet'

      structure Environment' : 
	  sig

	      structure Name : sig type name end 
    
	      structure Denotable : sig type denotable end 
 
	      type environment 
		
	      val access_env : Name.name -> environment -> Denotable.denotable

	  end

      sharing NameSet'.Name = Environment'.Name

      structure Denotable' : DENOTABLE

      sharing ElaboratorError'.Ttype = Denotable'.Ttype

      sharing Environment'.Denotable = Denotable'

      structure Store' : STORE

      sharing Denotable'.Location = Store'.Location

      structure Location' : sig type location_tube end  ) : sig

                                                                include ELAB_COM 

                                                                sharing AbstractGrammar = AbstractGrammar'

								sharing OkError = OkError'

								sharing ElaboratorError = ElaboratorError'

								sharing Post = Post'

								sharing NameSet = NameSet'

								sharing Environment = Environment'

								sharing Denotable = Denotable'

								sharing Store = Store'

								sharing Location = Location'

							    end =

struct

    structure AbstractGrammar = AbstractGrammar'

    structure OkError = OkError'

    structure ElaboratorError = ElaboratorError'

    structure Post = Post'

    structure NameSet = NameSet'

    structure Environment = Environment'

    structure Denotable = Denotable'

    structure Store = Store'

    structure Location = Location'
 
local

    open AbstractGrammar

    open OkError

    open Post

    open NameSet

    open Denotable

    open ElaboratorError

in

    fun C' I E G B =

  let
                          
      fun C(SKIP(i)) e t vir = OK(vir, (fn s => GOING(s)))
        | C(ABORT(i)) e t vir = OK(vir, (fn s => BROKEN(i)))
	| C(FOLLOW(i, com1, com2)) e t vir = (case (C(com1) e t vir)
						of OK(vir1, f1) => 
						    (case (C(com2) e t vir1)
						       of OK(vir2, f2) => OK(vir2, (check f2) o f1)
							| ERROR(e) => ERROR(e))
						 | ERROR(e) => ERROR(e))
	| C(VIR(i, id, exp)) e t vir = 
	  (case I(id) 
	     of OK(nam) => 
		 if not (is_in_one nam vir) 
		     then ERROR(i, VAR_IS_NOT_VIRGIN(nam))
		 else (case E(exp) e t vir
			 of OK(tau2, f) => 
			     (case Environment.access_env nam e
				of VAR(tau1, l) =>	
				    if tau1 <> tau2 
					then ERROR(i, TYPE_CLASH(tau1, tau2))
				    else OK(minus_one nam vir, 
					    fn s => check (fn sble => GOING(Store.update l sble s)) (f s))
				 | CON(tau1, l) => 
				    if tau1 <> tau2 
					then ERROR(i, TYPE_CLASH(tau1, tau2))
				    else OK(minus_one nam vir, 
					    fn s => check (fn sble => GOING(Store.update l sble s)) (f s))
				 | FREE => ERROR(i, VAR_NOT_KNOWN(nam)))
			  | ERROR(e) => ERROR(e))
	      | ERROR(e) => ERROR(e))
	| C(ASSIGN(i, id, exp)) e t vir = 
	  (case I(id) 
	     of OK(nam) => 
		 if is_in_one nam vir then ERROR(i, VAR_IS_VIRGIN(nam))
		 else (case E(exp) e t vir
			 of OK(tau2, f) => 
			     (case Environment.access_env nam e
				of VAR(tau1, l) =>	
				    if tau1 <> tau2 
					then ERROR(i, TYPE_CLASH(tau1, tau2))
				    else OK(vir, 
					    fn s => check (fn sble => GOING(Store.update l sble s)) (f s)) 
				 | CON _ => ERROR(i, VAR_IS_CONSTANT(nam))
				 | FREE => ERROR(i, VAR_NOT_KNOWN(nam)))
			  | ERROR(e) => ERROR(e))
	      | ERROR(e) => ERROR(e))
	| C(IFFI(i, guard)) e t vir = (case (G(guard) e t vir) 
					 of OK(vir1, ff) => 
					     OK(vir1, fn s => (case (ff s)
								 of GOING(p) => p
								  | BROKEN(_) => BROKEN(i)))
					  | ERROR(e) => ERROR(e)) 			     
	| C(DOOD(i, guard)) e t vir = (case (G(guard) e t vir) 
					 of OK(vir1, ff) => 
					     let
						 val init = minus (vir, vir1) 
					     in
						 if not (is_empty init) 
						     then ERROR(i, VARS_ARE_INITIALIZED(init))
						 else OK(vir, 
							 let
							     fun dood s = 
								 case (ff s)
								   of GOING(p) => check dood p
								    | BROKEN(i) => GOING(s)
							 in 
							     dood
							 end)
					     end
					  | ERROR(e) => ERROR(e)) 		 
	| C(BLOCK(i, bl)) e t vir = (case (B(bl) e t vir)
				       of OK(b) => OK(b)
					| ERROR(e) => ERROR(e))

  in
      C
  end
		
end

end
