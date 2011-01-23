(***********)
(* NameSet *)
(***********)


signature NAME_SET = 
sig

    structure Name : sig eqtype name end

    type name_set

    val empty : name_set

    val is_empty : name_set -> bool

    val plus_one : Name.name -> name_set -> name_set

    val singleton : Name.name -> name_set

    val plus : name_set * name_set -> name_set

    val minus_one : Name.name -> name_set -> name_set

    val minus : name_set * name_set -> name_set

    val is_in_one : Name.name -> name_set -> bool

    val is_in : name_set * name_set -> bool

end