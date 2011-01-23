(************)
(* Location *)
(************)


functor Location () : LOCATION =
struct

    datatype location = 
	ZERO
      | SUCC of location
 
    datatype location_tube =
	TUBE of location * ( unit -> location_tube )

    val tube = let
		   fun blow l = TUBE(l, fn () => blow (SUCC(l)))
	       in
		   blow ZERO
	       end

end