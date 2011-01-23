(***************)
(* ElabRightBl *)
(***************)


functor ElabRightBl

    ( structure AbstractGrammar' : ABSTRACT_GRAMMAR

      structure Identifier' : sig

				 type id
   
				 val string2id : string -> id

			     end

      sharing AbstractGrammar'.Identifier = Identifier'

      structure OkError' : OK_ERROR

      sharing OkError'.CharInfo = AbstractGrammar'.CharInfo

      structure ElaboratorError' : ELABORATOR_ERROR

      sharing OkError'.Error = ElaboratorError'

      structure NameSet' : NAME_SET

      sharing ElaboratorError'.NameSet = NameSet'

      structure Environment' : ENVIRONMENT

      sharing Environment'.Name = NameSet'.Name

      structure Denotable' : DENOTABLE

      sharing Environment'.Denotable = Denotable'

      structure Location' : LOCATION

      sharing Denotable'.Location = Location'

      structure Ttype' : TTYPE 

      sharing Denotable'.Ttype = Ttype'

      structure Store' : STORE

      sharing Store'.Location = Location'

      structure Storable' : sig
			     
			       structure Post : sig type 'a post end

			       type storable

			       val functional2sto : (storable -> storable Post.post) -> storable

			   end

      sharing Store'.Storable = Storable'
 
      structure Post' : POST 

      sharing Storable'.Post = Post' ) : sig

	                                     include ELAB_RIGHT_BL

                                             sharing AbstractGrammar = AbstractGrammar'

					     sharing Identifier = Identifier'

					     sharing OkError = OkError'

					     sharing ElaboratorError = ElaboratorError'

					     sharing NameSet = NameSet'

					     sharing Environment = Environment'

					     sharing Denotable = Denotable'

					     sharing Location = Location'

					     sharing Ttype = Ttype'

					     sharing Store = Store'

					     sharing Storable = Storable'

					     sharing Post = Post'

					 end =

struct

    structure AbstractGrammar = AbstractGrammar'

    structure Identifier = Identifier'

    structure OkError = OkError'

    structure ElaboratorError = ElaboratorError'

    structure NameSet = NameSet'

    structure Environment = Environment'

    structure Denotable = Denotable'

    structure Location = Location'

    structure Ttype = Ttype'

    structure Store = Store'

    structure Storable = Storable'

    structure Post = Post'

local

    open Identifier

    open AbstractGrammar

    open OkError

    open Location

    open Ttype

    open Denotable

    open Environment

    open NameSet

    open ElaboratorError

in

    fun A' I D B =

  let
                          
    fun A(bl as BEGIN(i, dec, com)) e t vir = 
	let
	    exception ElabRightBlError
	    val initial_env0 = e

	    val TUBE(l1, t1) = t
	    val input_name = (case I(IDENTIFIER(i, string2id "input"))
				of OK(nam) => nam
				 | ERROR(e) => raise ElabRightBlError) 
	    val initial_env1 = update_env input_name 
		(CON(UNKNOWN, l1)) initial_env0

	    val TUBE(l2, t2) = t1()
	    val output_name = (case I(IDENTIFIER(i, string2id "output"))
				of OK(nam) => nam
				 | ERROR(e) => raise ElabRightBlError)
	    val initial_env2 = update_env output_name 
		(CON(UNKNOWN, l2)) initial_env1

	    val initial_env = initial_env2
	    val initial_tube = t2()
	    val initial_vir = plus_one output_name (minus_one output_name vir)
	in
	    (case (D(dec) initial_env initial_tube initial_vir)
	       of OK(e1, t1, pri1, vir1, glo1) => 
		   (if not (is_in_one input_name glo1) then ERROR(i, INPUT_NOT_DECLARED)
		    else if not (is_in_one output_name vir1) then ERROR(i, OUTPUT_NOT_DECLARED)
			 else (case access_env input_name e1
				 of CON(UNKNOWN, _) => ERROR(i, INPUT_TYPE_NOT_KNOWN)
				  | CON(tau1, l1) =>
				     (case access_env output_name e1
					of CON(UNKNOWN, _) => ERROR(i, OUTPUT_TYPE_NOT_KNOWN)
					 | CON(tau2, l2) =>
					    (case B(bl) initial_env initial_tube initial_vir		 
					       of OK(vir, f) => 
						   (if is_in_one output_name vir then ERROR(i, NO_OUTPUT)
						    else OK(FUN(tau1, tau2),
							    fn s => 
							    Post.GOING
							    (Storable.functional2sto
							     (fn sble =>
							      let
								  val initial_store = 
								      Store.update l1 sble s
							      in
								  case (f initial_store)
								    of Post.GOING(s) => 
									Post.GOING(Store.access l2 s)
								     | Post.BROKEN(i) => Post.BROKEN(i)
							      end))))
						| ERROR(e) => ERROR(e))
					 | _ => raise ElabRightBlError)
				  | _ => raise ElabRightBlError))
		| ERROR(e) => ERROR(e))
	end
	     
  in
      A
  end
		
end

end