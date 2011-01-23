(************)
(* Storable *)
(************)


functor Storable

    ( structure Integer' : INTEGER
  
      structure Boolean' : BOOLEAN

      structure Post' : POST ) : sig

	                             include STORABLE

                                     sharing Integer = Integer'

				     sharing Boolean = Boolean'

				     sharing Post = Post'

				 end = 

struct

    structure Integer = Integer'

    structure Boolean = Boolean'

    structure Post = Post'

    datatype storable = 
	INTEGER of Integer.integer
      | BOOLEAN of Boolean.boolean
      | FUNCTIONAL of storable -> storable Post.post

    exception StorableError

    fun integer2sto i = INTEGER(i)

    fun boolean2sto b = BOOLEAN(b)

    fun functional2sto f = FUNCTIONAL(f)

    exception StorableError

    fun sto2functional (FUNCTIONAL(f)) = f
      | sto2functional _ = raise StorableError

    fun is_storable_false (BOOLEAN(b)) = b = Boolean.ffalse
      | is_storable_false _ = raise StorableError

    fun is_storable_true (BOOLEAN(b)) = b = Boolean.ttrue
      | is_storable_true _ = raise StorableError

    val storable_false = BOOLEAN(Boolean.ffalse)

    val storable_true = BOOLEAN(Boolean.ttrue)

    fun sapp (FUNCTIONAL(f), s) = f s
      | sapp _ = raise StorableError

    val sneg = FUNCTIONAL(fn INTEGER(i) => Post.GOING(INTEGER(Integer.~ i))
                           | _ => raise StorableError)
    val snot = FUNCTIONAL(fn BOOLEAN(b) => Post.GOING(BOOLEAN(Boolean.not b))
                           | _ => raise StorableError)

    val sdiv = FUNCTIONAL(fn INTEGER(i1) => 
			  Post.GOING(FUNCTIONAL(fn INTEGER(i2) => 
						Post.GOING(INTEGER(Integer.div (i1, i2)))
			                         | _ => raise StorableError))
                           | _ => raise StorableError)

    val smod = FUNCTIONAL(fn INTEGER(i1) => 
			  Post.GOING(FUNCTIONAL(fn INTEGER(i2) => 
						Post.GOING(INTEGER(Integer.mod (i1, i2)))
			                         | _ => raise StorableError))
                           | _ => raise StorableError)

    val stimes = FUNCTIONAL(fn INTEGER(i1) => 
		  	    Post.GOING(FUNCTIONAL(fn INTEGER(i2) => 
						  Post.GOING(INTEGER(Integer.* (i1, i2)))
			                           | _ => raise StorableError))
                             | _ => raise StorableError)

    val splus = FUNCTIONAL(fn INTEGER(i1) => 
			   Post.GOING(FUNCTIONAL(fn INTEGER(i2) => 
						 Post.GOING(INTEGER(Integer.+ (i1, i2)))
			                          | _ => raise StorableError))
                            | _ => raise StorableError)

    val sminus = FUNCTIONAL(fn INTEGER(i1) => 
			    Post.GOING(FUNCTIONAL(fn INTEGER(i2) => 
						  Post.GOING(INTEGER(Integer.- (i1, i2)))
			                           | _ => raise StorableError))
                             | _ => raise StorableError)

    val sequal = FUNCTIONAL(fn INTEGER(i1) => 
			    Post.GOING(FUNCTIONAL(fn INTEGER(i2) => 
						  Post.GOING(if i1 = i2 then storable_true 
							     else storable_false)
			                           | _ => raise StorableError))
                             | _ => raise StorableError)

    val sdiff = FUNCTIONAL(fn INTEGER(i1) => 
			   Post.GOING(FUNCTIONAL(fn INTEGER(i2) => 
						 Post.GOING(if i1 <> i2 then storable_true 
							    else storable_false)
			                          | _ => raise StorableError))
                            | _ => raise StorableError)

    val sless = FUNCTIONAL(fn INTEGER(i1) => 
			   Post.GOING(FUNCTIONAL(fn INTEGER(i2) => 
						 Post.GOING(if Integer.< (i1, i2) then storable_true 
							    else storable_false)
			                          | _ => raise StorableError))
                            | _ => raise StorableError)

    val sgreat = FUNCTIONAL(fn INTEGER(i1) => 
			    Post.GOING(FUNCTIONAL(fn INTEGER(i2) => 
						  Post.GOING(if Integer.> (i1, i2) then storable_true 
							     else storable_false)
			                           | _ => raise StorableError))
                             | _ => raise StorableError)

    val seqless = FUNCTIONAL(fn INTEGER(i1) => 
		 	     Post.GOING(FUNCTIONAL(fn INTEGER(i2) => 
						   Post.GOING(if Integer.<= (i1, i2) then storable_true 
							      else storable_false)
			                            | _ => raise StorableError))
                              | _ => raise StorableError)

    val seqgreat = FUNCTIONAL(fn INTEGER(i1) => 
			      Post.GOING(FUNCTIONAL(fn INTEGER(i2) => 
						    Post.GOING(if Integer.>= (i1, i2) then storable_true 
							       else storable_false)
			                             | _ => raise StorableError))
                               | _ => raise StorableError)

    val sand = FUNCTIONAL(fn BOOLEAN(b1) => 
			  Post.GOING(FUNCTIONAL(fn BOOLEAN(b2) => 
						Post.GOING(BOOLEAN(Boolean.aand (b1, b2)))
			                         | _ => raise StorableError))
                           | _ => raise StorableError)

    val sor = FUNCTIONAL(fn BOOLEAN(b1) => 
			 Post.GOING(FUNCTIONAL(fn BOOLEAN(b2) => 
					       Post.GOING(BOOLEAN(Boolean.oor (b1, b2)))
			                        | _ => raise StorableError))
                          | _ => raise StorableError)
	
end


