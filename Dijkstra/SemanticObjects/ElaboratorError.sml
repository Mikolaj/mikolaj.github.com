(*******************)
(* ElaboratorError *) 
(*******************)


functor ElaboratorError

    ( structure NameSet' : sig 
			    
			    structure Name : sig type name end

			    type name_set 

			end 

      structure Ttype' : sig type ttype end ) : sig

                                                    include ELABORATOR_ERROR 

                                                    sharing NameSet = NameSet'

						    sharing Ttype = Ttype'

						end =

struct

    structure NameSet = NameSet'

    structure Ttype = Ttype'

    datatype error = 
	VARS_NOT_INITIALIZED of NameSet.name_set (* bl *)

      | VAR_IS_NOT_VIRGIN of NameSet.Name.name (* com *)
      | VAR_IS_VIRGIN of NameSet.Name.name
      | TYPE_CLASH of Ttype.ttype * Ttype.ttype
      | VAR_IS_CONSTANT of NameSet.Name.name
      | VAR_NOT_KNOWN of NameSet.Name.name
      | VARS_ARE_INITIALIZED of NameSet.name_set

      | GUARD_IS_NOT_BOOL of Ttype.ttype (* gua *)
      | INITIALIZED_VARS_DIFFER of NameSet.name_set * NameSet.name_set

      | VAR_IS_NOT_VIRGIN_DEC of NameSet.Name.name (* dec *)
      | VAR_IS_VIRGIN_DEC of NameSet.Name.name
      | VAR_IS_CONSTANT_DEC of NameSet.Name.name
      | VAR_NOT_KNOWN_DEC of NameSet.Name.name

      | TYPE_CLASH_TON of Ttype.ttype * Ttype.ttype (* ton *)
 
      | TYPE_CLASH_EXP of Ttype.ttype * Ttype.ttype (* exp *)
      | TYPE_NOT_FUNCTIONAL of Ttype.ttype 

      | VAR_IS_VIRGIN_RID of NameSet.Name.name (* right id *)
      | VAR_NOT_KNOWN_RID of NameSet.Name.name

      | INPUT_NOT_DECLARED (* right bl *)
      | OUTPUT_NOT_DECLARED
      | INPUT_TYPE_NOT_KNOWN
      | OUTPUT_TYPE_NOT_KNOWN
      | NO_OUTPUT


end 
