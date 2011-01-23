(************)
(* Falseval *)
(************)


functor Falseval () : FALSEVAL = 
struct

    type fal = bool

    fun fal2bool f = f

    fun bool2fal b = b

end