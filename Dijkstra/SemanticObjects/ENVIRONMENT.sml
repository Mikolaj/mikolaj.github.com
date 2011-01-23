(***************)
(* Environment *)
(***************)


signature ENVIRONMENT = 
sig

    structure Name : sig eqtype name end 
    
    structure Denotable : sig eqtype denotable end 

    type environment

    val access_env : Name.name -> environment -> Denotable.denotable

    val update_env : Name.name -> Denotable.denotable -> environment -> environment

    val empty_env : Denotable.denotable -> environment

    val plus_env  : Denotable.denotable -> environment -> environment -> environment

end