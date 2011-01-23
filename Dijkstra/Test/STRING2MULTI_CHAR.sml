(********************)
(* String2MultiChar *)
(********************)


signature STRING2MULTI_CHAR =
sig

    structure CharList : CHAR_LIST 

    structure MultiChar : MULTI_CHAR

    sharing CharList.MultiChar = MultiChar

    val string2mchar : string -> CharList.char_list

end
