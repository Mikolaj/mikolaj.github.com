(************)
(* Falseval *)
(************)


signature FALSEVAL = 
sig

    type fal

    val fal2bool : fal -> bool

    val bool2fal : bool -> fal

end