
(***********)
(* OkError *)
(***********)

signature OK_ERROR =
sig

    structure CharInfo : sig eqtype info end

    structure Error : sig type error end 

    datatype 'a ok =
	OK of 'a
      | ERROR of CharInfo.info * Error.error

end 
