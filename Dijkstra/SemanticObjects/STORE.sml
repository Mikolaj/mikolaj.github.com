(*********)
(* Store *)
(*********)

signature STORE =
sig

    structure Location : sig eqtype location end
    
    structure Storable : sig type storable end

    type store

    val access : Location.location -> store -> Storable.storable

    val update : Location.location -> Storable.storable -> store -> store

    val	empty_store : store
		      
end
