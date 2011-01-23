(***********)
(* OkError *)
(***********)


functor OkError
    
    ( structure CharInfo' : sig eqtype info end

      structure Error' : sig type error end ) : sig

	                                            include OK_ERROR

                                                    sharing CharInfo = CharInfo'

						    sharing Error = Error'

						end =

struct

    structure CharInfo = CharInfo'

    structure Error = Error'

    datatype 'a ok =
	OK of 'a
      | ERROR of CharInfo.info * Error.error

end 
