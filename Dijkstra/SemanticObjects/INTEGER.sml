(***********)
(* Integer *)
(***********)


signature INTEGER =
sig

    eqtype integer

    val ~ : integer -> integer
    val div : integer * integer -> integer
    val mod : integer * integer -> integer
    val * : integer * integer -> integer
    val + : integer * integer -> integer
    val - : integer * integer -> integer
    val <  : integer * integer -> bool
    val >  : integer * integer -> bool
    val <= : integer * integer -> bool
    val >= : integer * integer -> bool

    val int2integer : int -> integer

end