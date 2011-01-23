(*********)
(* Ttype *)
(*********)


functor Ttype () : TTYPE = 
struct

    datatype ttype = 
	INT
      | BOOL
      | FUN of ttype * ttype
      | UNKNOWN

    val ttype_bool = BOOL

    val ttype_int = INT

end