(********)
(* Post *)
(********)


functor Post

    ( structure CharInfo' : sig eqtype info end ) : sig

	                                              include POST

                                                      sharing CharInfo = CharInfo'

						  end =

struct

    structure CharInfo = CharInfo'

    datatype 'a post =
	GOING of 'a
      | BROKEN of CharInfo.info

    fun check f p = case p
		      of GOING(s) => f s 
		       | BROKEN(i) => BROKEN(i)
			      
end

