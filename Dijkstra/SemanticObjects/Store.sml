(*********)
(* Store *)
(*********)


functor Store 

    ( structure Location' : sig eqtype location end

      structure Storable' : sig type storable end ) : sig 

	                                                  include STORE 

                                                          sharing Location = Location'
    
							  sharing Storable = Storable'

						      end =

struct

    structure Location = Location'
    
    structure Storable = Storable'

    datatype store = STORE of Location.location -> Storable.storable

    fun access l (STORE(f)) = f l

    fun update l v s = STORE(fn l1 => if l1 = l then v else access l1 s)

    exception StoreError

    val empty_store = STORE(fn l => raise StoreError)
		      
end

