(***********)
(* ElabGua *)
(***********)


functor ElabGua

    ( structure AbstractGrammar' : ABSTRACT_GRAMMAR

      structure OkError' : OK_ERROR
 
      sharing OkError'.CharInfo = AbstractGrammar'.CharInfo

      structure ElaboratorError' : ELABORATOR_ERROR

      sharing OkError'.Error = ElaboratorError'

      structure Post' : POST

      sharing Post'.CharInfo = AbstractGrammar'.CharInfo

      structure Ttype' : sig

			    eqtype ttype

			    val ttype_bool : ttype

			end

      sharing ElaboratorError'.Ttype = Ttype'

      structure Storable' : sig

			     type storable

			     val is_storable_false : storable -> bool

			     val is_storable_true : storable -> bool
    
			 end

      structure Environment' : sig type environment end

      structure Location' : sig type location_tube end

      structure NameSet' : NAME_SET

      sharing ElaboratorError'.NameSet = NameSet'

      structure Store' : sig type store end ) : sig

	                                            include ELAB_GUA

                                                    sharing AbstractGrammar = AbstractGrammar'

						    sharing OkError = OkError'

						    sharing Post = Post'

						    sharing ElaboratorError = ElaboratorError'

						    sharing Ttype = Ttype'

						    sharing Storable = Storable'

						    sharing Environment = Environment'

						    sharing Location = Location'

						    sharing NameSet = NameSet'
							
						    sharing Store = Store'

						end =

struct

    structure AbstractGrammar = AbstractGrammar'

    structure OkError = OkError'

    structure Post = Post'

    structure ElaboratorError = ElaboratorError'

    structure Ttype = Ttype'

    structure Storable = Storable'

    structure Environment = Environment'

    structure Location = Location'

    structure NameSet = NameSet'
   
    structure Store = Store'

local

    open AbstractGrammar

    open OkError

    open Post

    open NameSet

    open Ttype

    open ElaboratorError

in

    fun G' E C =

  let

    fun G(ARROW(i, exp, com)) e t vir = (case E(exp) e t vir
					   of OK(tau, fe) =>
					       if tau <> ttype_bool 
						   then ERROR(i, GUARD_IS_NOT_BOOL(tau))
					       else (case C(com) e t vir
						       of OK(vir1, fc) => 
							   OK(vir1, fn s => 
							      (case fe s
								 of GOING(sble) => 
								     if Storable.is_storable_true sble
									 then GOING(fc s) 
								     else BROKEN(i)
								  | BROKEN(i) => GOING(BROKEN(i))))
							| ERROR(e) => ERROR(e))
					    | ERROR(e) => ERROR(e))	  
      | G(TACT(i, guard1, guard2)) e t vir = (case G(guard1) e t vir
						of OK(vir1, ff1) => 
						    (case G(guard2) e t vir
						       of OK(vir2, ff2) =>
							   let
							       val init1 = minus (vir, vir1) 
							       val init2 = minus (vir, vir2)
							   in
							       if not (is_in (init1, init2) andalso is_in (init2, init1))
								   then ERROR(i, INITIALIZED_VARS_DIFFER
									      (init1, init2))     
							       else OK(vir1, fn s =>
								       (case ff1 s
									  of GOING(p) => GOING(p)
									   | BROKEN(_) => ff2 s))
							   end
							| ERROR(e) => ERROR(e))
						 | ERROR(e) => ERROR(e))

  in
      G
  end
		
end

end
