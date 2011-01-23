(*************)
(* Denotable *)
(*************)


functor Denotable

    ( structure Ttype' : sig eqtype ttype end

      structure Location' : sig eqtype location end ) : sig

	                                                    include DENOTABLE 

                                                            sharing Ttype = Ttype'

							    sharing Location = Location'

							end = 

struct

    structure Ttype = Ttype'

    structure Location = Location'

    datatype denotable = 
	VAR of Ttype.ttype * Location.location
      | CON of Ttype.ttype * Location.location
      | FREE

end