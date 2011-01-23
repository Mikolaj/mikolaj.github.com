(*************)
(* Denotable *)
(*************)


signature DENOTABLE = 
sig

    structure Ttype : sig eqtype ttype end

    structure Location : sig eqtype location end

    datatype denotable = 
	VAR of Ttype.ttype * Location.location
      | CON of Ttype.ttype * Location.location
      | FREE

end