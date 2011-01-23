(************)
(* ElabProg *)
(************)


functor ElabProg

    ( structure AbstractGrammar' : ABSTRACT_GRAMMAR

      structure OkError' : OK_ERROR

      structure NameSet' : sig 

			      type name_set

			      val empty : name_set

			  end

      structure Environment' : sig

				  structure Denotable : sig type denotable end

				  type environment

				  val empty_env : Denotable.denotable -> environment

			      end

      structure Denotable' : DENOTABLE

      sharing Environment'.Denotable = Denotable'

      structure Location' : sig

			       type location_tube

			       val tube : location_tube

			   end

      structure Ttype' : sig type ttype end

      structure Post' : POST

      structure Storable' : sig

			       structure Post : sig type 'a post end

			       type storable

			       val sto2functional : storable -> (storable -> storable Post.post) (* unsafe *)

			   end

      sharing Storable'.Post = Post'

      structure Store' : sig

			    type store

			    val empty_store : store

			end ) : sig

	                            include ELAB_PROG 

                                    sharing AbstractGrammar = AbstractGrammar'
	
				    sharing OkError = OkError'

				    sharing NameSet = NameSet'

				    sharing Environment = Environment'

				    sharing Denotable = Denotable'

				    sharing Location = Location'

				    sharing Ttype = Ttype'
	
				    sharing Post = Post'
	
				    sharing Storable = Storable'

				    sharing Store = Store'

				end =

struct

    structure AbstractGrammar = AbstractGrammar'
	
    structure OkError = OkError'

    structure NameSet = NameSet'

    structure Environment = Environment'

    structure Denotable = Denotable'

    structure Location = Location'

    structure Ttype = Ttype'
 
    structure Post = Post'

    structure Storable = Storable'

    structure Store = Store'

    fun P' A =

  let

    fun P(AbstractGrammar.BLOCKprog(i, bl)) = 
	let

	    val initial_env = Environment.empty_env Denotable.FREE
	    val initial_tube = Location.tube
	    val initial_vir = NameSet.empty
	    val initial_store = Store.empty_store 
	    exception ElabProgError

	in

	    case A(bl) initial_env initial_tube initial_vir
	      of OkError.OK(tau, f) => 
		  OkError.OK(tau, (case f initial_store
				     of Post.GOING(sble) => Storable.sto2functional sble
				      | Post.BROKEN(i) => raise ElabProgError))
	       | OkError.ERROR(e) => OkError.ERROR(e)

	end

  in	
      P
  end

end