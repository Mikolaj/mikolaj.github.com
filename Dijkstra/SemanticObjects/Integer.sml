(***********)
(* Integer *)
(***********)


functor Integer () : INTEGER =
struct

    type integer = int

    nonfix div mod * + - < > <= >=

    val ~ : int -> int = ~
    val div : int * int -> int = div
    val mod : int * int -> int = mod
    val * : int * int -> int = *
    val + : int * int -> int = +
    val - : int * int -> int = -
    val <  : int * int -> bool = <
    val >  : int * int -> bool = >
    val <= : int * int -> bool = <=
    val >= : int * int -> bool = >=

    fun int2integer i = i

end