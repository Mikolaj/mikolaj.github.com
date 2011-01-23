(*********)
(* Ttype *)
(*********)


signature TTYPE = 
sig

    datatype ttype = 
	INT
      | BOOL
      | FUN of ttype * ttype
      | UNKNOWN

    val ttype_bool : ttype

    val ttype_int : ttype

end