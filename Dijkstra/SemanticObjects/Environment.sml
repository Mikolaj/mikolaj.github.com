(***************)
(* Environment *)
(***************)


functor Environment

    ( structure Name' : sig eqtype name end 

      structure Denotable' : sig eqtype denotable end ) : sig

	                                                      include ENVIRONMENT 

                                                              sharing Name = Name'

							      sharing Denotable = Denotable'

							  end = 

struct

    structure Name = Name'

    structure Denotable = Denotable'
    
    datatype environment = 
	ENV of ( Name.name -> Denotable.denotable )

    fun access_env nam (ENV(f)) = f nam

    fun update_env nam v e = 
	ENV(fn nam1 => if nam1 = nam then v else access_env nam1 e)

    fun empty_env free = ENV(fn nam1 => free)

    fun plus_env free e1 e2 = 
	ENV(fn nam => (let 
			   val d = access_env nam e2
		       in
			   if d = free
			       then access_env nam e1
			   else d
		       end))

end