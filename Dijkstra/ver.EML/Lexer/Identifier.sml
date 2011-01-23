(**************)
(* Identifier *)
(**************)


functor Identifier () : IDENTIFIER = 
struct

    type id = string

    fun id2string i = i

    fun string2id s = s

end