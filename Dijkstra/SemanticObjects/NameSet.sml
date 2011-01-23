(***********)
(* NameSet *)
(***********)


functor NameSet

    ( structure Name' : sig eqtype name end) : sig

	                                           include NAME_SET 

                                                   sharing Name = Name'

					       end = 

struct

    structure Name = Name'

    datatype name_set =
	NIL
      | CONS of Name.name * name_set

    val empty = NIL

    fun is_empty s = (s = empty)

    fun plus_one nam s = CONS(nam, s)

    fun singleton nam = plus_one nam empty

    fun plus (s, NIL) = s
      | plus (s, CONS(nam, rest)) = plus (plus_one nam s, rest)

    fun minus_one nam NIL = NIL
      | minus_one nam (CONS(nam1, rest)) = if nam1 = nam then rest
					   else CONS(nam1, minus_one nam rest)

    fun minus (s, NIL) = s
      | minus (s, CONS(nam, rest)) = minus (minus_one nam s, rest)

    fun is_in_one nam s = (minus_one nam s) <> s

    fun is_in (NIL, s) = true
      | is_in (CONS(nam, rest), s) = is_in_one nam s andalso is_in (rest, s)

end