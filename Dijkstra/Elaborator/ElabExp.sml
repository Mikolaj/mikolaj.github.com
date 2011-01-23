(***********)
(* ElabExp *)
(***********)


functor ElabExp 
  
    ( structure AbstractGrammar' : ABSTRACT_GRAMMAR

      structure OkError' : OK_ERROR

      sharing OkError'.CharInfo = AbstractGrammar'.CharInfo

      structure ElaboratorError' : ELABORATOR_ERROR

      sharing OkError'.Error = ElaboratorError'

      structure Post' : POST

      sharing Post'.CharInfo = AbstractGrammar'.CharInfo

      structure Environment' : sig type environment end

      structure NameSet' : sig type name_set end

      structure Store' : sig type store end

      structure Ttype' : TTYPE

      sharing ElaboratorError'.Ttype = Ttype'

      structure Storable' : STORABLE

      sharing Storable'.Post = Post'

      structure Location' : sig type location_tube end ) : sig

	                                                       include ELAB_EXP

                                                               sharing AbstractGrammar = AbstractGrammar'

							       sharing OkError = OkError'

							       sharing Post = Post'

							       sharing ElaboratorError = ElaboratorError'
	
							       sharing Environment = Environment'

							       sharing NameSet = NameSet'

							       sharing Store = Store'

							       sharing Ttype = Ttype'

							       sharing Storable = Storable'

							       sharing Location = Location'

							   end = 

struct

    structure AbstractGrammar = AbstractGrammar'

    structure OkError = OkError'

    structure Post = Post'

    structure ElaboratorError = ElaboratorError'
	
    structure Environment = Environment'

    structure NameSet = NameSet'

    structure Store = Store'

    structure Ttype = Ttype'

    structure Storable = Storable'

    structure Location = Location'

local

    open AbstractGrammar

    open OkError

    open Post

    open Ttype

    open Storable

    open ElaboratorError

in

    fun E' R A N F =

  let
                          
    fun E(ID(i, id)) e t vir = R(id) e vir 
      | E(BLOCKexp(i, block)) e t vir = A(block) e t vir
      | E(NUM(i, n)) e t vir = (case N(n)
				  of OK(i) => OK(ttype_int, fn s => GOING(integer2sto i))
				   | ERROR(e) => ERROR(e))
      | E(FAL(i, f)) e t vir = (case F(f)
				  of OK(b) => OK(ttype_bool, fn s => GOING(boolean2sto b))
				   | ERROR(e) => ERROR(e))
      | E(APP(i, exp1, exp2)) e t vir = (case (E(exp1) e t vir)
					   of OK(FUN(tau1, tau2), f1) => 
					       (case (E(exp2) e t vir)
						  of OK(tau3, f2) => 
						      (if tau1 = tau3 then
							   OK(tau2, fn s => 
							      check (fn sble1 => 
								     check (fn sble2 => 
									    sapp (sble1, sble2))
								     (f2 s)) 
							      (f1 s))
						       else ERROR(i, TYPE_CLASH_EXP(tau1, tau3)))
						   | ERROR(e) => ERROR(e))
					    | OK(tau, _) => ERROR(i, TYPE_NOT_FUNCTIONAL(tau))
					    | ERROR(e) => ERROR(e))

      | E(NEG(i)) e t vir = OK(FUN(ttype_int, ttype_int), fn s => GOING(sneg))
      | E(NOT(i)) e t vir = OK(FUN(ttype_bool, ttype_bool), fn s => GOING(snot)) 
      | E(DIV(i)) e t vir = OK(FUN(ttype_int, FUN(ttype_int, ttype_int)), fn s => GOING(sdiv))
      | E(MOD(i)) e t vir = OK(FUN(ttype_int, FUN(ttype_int, ttype_int)), fn s => GOING(smod))
      | E(TIMES(i)) e t vir = OK(FUN(ttype_int, FUN(ttype_int, ttype_int)), fn s => GOING(stimes))
      | E(PLUS(i)) e t vir = OK(FUN(ttype_int, FUN(ttype_int, ttype_int)), fn s => GOING(splus))
      | E(MINUS(i)) e t vir = OK(FUN(ttype_int, FUN(ttype_int, ttype_int)), fn s => GOING(sminus))
      | E(EQUAL(i)) e t vir = OK(FUN(ttype_int, FUN(ttype_int, ttype_bool)), fn s => GOING(sequal))
      | E(DIFF(i)) e t vir = OK(FUN(ttype_int, FUN(ttype_int, ttype_bool)), fn s => GOING(sdiff))
      | E(LESS(i)) e t vir = OK(FUN(ttype_int, FUN(ttype_int, ttype_bool)), fn s => GOING(sless))
      | E(GREAT(i)) e t vir = OK(FUN(ttype_int, FUN(ttype_int, ttype_bool)), fn s => GOING(sgreat))
      | E(EQLESS(i)) e t vir = OK(FUN(ttype_int, FUN(ttype_int, ttype_bool)), fn s => GOING(seqless))
      | E(EQGREAT(i)) e t vir = OK(FUN(ttype_int, FUN(ttype_int, ttype_bool)), fn s => GOING(seqgreat))
      | E(AND(i)) e t vir = OK(FUN(ttype_bool, FUN(ttype_bool, ttype_bool)), fn s => GOING(sand))
      | E(OR(i)) e t vir = OK(FUN(ttype_bool, FUN(ttype_bool, ttype_bool)), fn s => GOING(sor))

  in
      E
  end

end

end 