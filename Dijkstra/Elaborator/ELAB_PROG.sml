(************)
(* ElabProg *)
(************)


signature ELAB_PROG =
sig
    
    structure AbstractGrammar : ABSTRACT_GRAMMAR

    structure OkError : OK_ERROR

    structure NameSet : sig 

			    type name_set

			    val empty : name_set

			end

    structure Environment : sig

				structure Denotable : sig type denotable end

				type environment

				val empty_env : Denotable.denotable -> environment

			    end

    structure Denotable : DENOTABLE

    sharing Environment.Denotable = Denotable

    structure Location : sig

			     type location_tube

			     val tube : location_tube

			 end

    structure Ttype : sig type ttype end

    structure Post : POST

    structure Storable : sig

			     structure Post : sig type 'a post end

			     type storable

			     val sto2functional : storable -> (storable -> storable Post.post) (* unsafe *)

			 end

    sharing Storable.Post = Post

    structure Store : sig

			  type store

			  val empty_store : store

		      end

    val P' :
	(AbstractGrammar.block -> Environment.environment -> 
	 Location.location_tube -> NameSet.name_set ->
	 (Ttype.ttype * (Store.store -> Storable.storable Post.post)) OkError.ok) ->

	AbstractGrammar.program -> 
	(Ttype.ttype * (Storable.storable -> Storable.storable Post.post)) OkError.ok

end

   






   
