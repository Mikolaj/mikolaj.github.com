(**************)
(* Identifier *)
(**************)


signature IDENTIFIER = 
sig

    type id
   
    val id2string : id -> string      

    val string2id : string -> id

end