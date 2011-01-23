(**********)
(* ElabBl *)
(**********)


functor ElabBl

    ( structure AbstractGrammar' : ABSTRACT_GRAMMAR

      structure OkError' : OK_ERROR

      sharing OkError'.CharInfo = AbstractGrammar'.CharInfo

      structure ElaboratorError' : ELABORATOR_ERROR

      sharing OkError'.Error = ElaboratorError'

      structure Post' : POST

      sharing Post'.CharInfo = AbstractGrammar'.CharInfo

      structure NameSet' : NAME_SET

      sharing ElaboratorError'.NameSet = NameSet'

      structure Environment' : sig type environment end

      structure Location' : sig type location_tube end

      structure Store' : sig type store end ) : sig

	                                            include ELAB_BL

                                                    sharing AbstractGrammar = AbstractGrammar'

						    sharing OkError = OkError'

						    sharing ElaboratorError = ElaboratorError'

						    sharing Post = Post'

						    sharing NameSet = NameSet'

						    sharing Environment = Environment'
							
						    sharing Location = Location'
	
						    sharing Store = Store'

						end =

struct

    structure AbstractGrammar = AbstractGrammar'

    structure OkError = OkError'

    structure ElaboratorError = ElaboratorError'

    structure Post = Post'

    structure NameSet = NameSet'

    structure Environment = Environment'

    structure Location = Location'
   
    structure Store = Store'

local

    open AbstractGrammar

    open OkError

    open NameSet

    open ElaboratorError

in

    fun B' D C =

  let
                          
    fun B(BEGIN(i, dec, com)) e t vir = 
	(case (D(dec) e t vir)
	   of OK(e1, t1, pri1, vir1, glo1) => 
	       (case C(com) e1 t1 (plus (pri1, vir1))
		  of OK(vir2, f) =>
		      let
			  val uninit = minus (vir2, pri1) 
		      in
			  if not (is_empty uninit) then ERROR(i, VARS_NOT_INITIALIZED(uninit))
			  else OK(minus (vir, vir1), f)
		      end
		   | ERROR(e) => ERROR(e))	     
	    | ERROR(e) => ERROR(e))
	     
  in
      B
  end
		
end

end
