(********)
(* Post *)
(********)


signature POST =
sig

    structure CharInfo : sig eqtype info end

    datatype 'a post =
	GOING of 'a
      | BROKEN of CharInfo.info

    val check : ( 'a -> 'b post ) -> 'a post -> 'b post 
			      
end

