(***********)
(* Boolean *)
(***********)


signature BOOLEAN = 
sig

    eqtype boolean

    val ffalse : boolean

    val ttrue : boolean

    val not : boolean -> boolean

    val aand : boolean * boolean -> boolean

    val oor : boolean * boolean -> boolean

    val bool2boolean : bool -> boolean

end