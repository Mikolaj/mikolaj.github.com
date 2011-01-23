(************)
(* Location *)
(************)


signature LOCATION =
sig

    eqtype location

    datatype location_tube =
	TUBE of location * ( unit -> location_tube )

    val tube : location_tube

(* EML *)

    axiom let 
	      structure Nat : sig
	    
				  datatype nat = 
				      ZERO
				    | SUCC of nat
				
			      end = ?
	  in
	      let
		  fun squeeze (TUBE(l, t)) Nat.ZERO = l
		    | squeeze (TUBE(l, t)) (Nat.SUCC(n)) = squeeze (t()) n
	      in
		  forall n => 
		      forall m => 
			  ( n <> m) implies ( squeeze tube n <> squeeze tube m )
	      end
	  end

(* enough EML *)

end