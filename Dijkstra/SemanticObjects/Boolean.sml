(***********)
(* Boolean *)
(***********)


functor Boolean () : BOOLEAN = 
struct

    type boolean = bool

    val ffalse = false

    val ttrue = true

    val not = not

    fun aand (b1, b2) = b1 andalso b2

    fun oor (b1, b2) = b1 orelse b2

    fun bool2boolean b = b

end