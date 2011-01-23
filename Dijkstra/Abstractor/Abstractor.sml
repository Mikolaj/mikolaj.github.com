(**************)
(* Abstractor *)
(**************)


functor Abstractor

    ( structure LL1Grammar' : LL1_GRAMMAR

      structure AbstractGrammar' : ABSTRACT_GRAMMAR

      sharing LL1Grammar'.CharInfo = AbstractGrammar'.CharInfo

      sharing LL1Grammar'.Identifier = AbstractGrammar'.Identifier

      sharing LL1Grammar'.Numeral = AbstractGrammar'.Numeral

      sharing LL1Grammar'.Falseval = AbstractGrammar'.Falseval ) : sig

	                                                               include ABSTRACTOR

                                                                       sharing LL1Grammar = LL1Grammar'

								       and AbstractGrammar = AbstractGrammar'

								   end =

struct

    structure LL1Grammar = LL1Grammar'

    structure AbstractGrammar = AbstractGrammar'

local

    open LL1Grammar

    structure A = AbstractGrammar

in

    fun abs_program (BLOCKprog(i, block)) = A.BLOCKprog(i, abs_block block)

    and abs_command_list (COMMAND_LIST(i, command, rest_of_command_list)) =
	abs_rest_of_command_list (abs_command command) rest_of_command_list

    and abs_rest_of_command_list a EPScom = a
      | abs_rest_of_command_list a (SEMIcom(i, command_list)) = 
	A.FOLLOW(i, a, abs_command_list command_list)

    and abs_command (SKIP(i)) = A.SKIP(i)
      | abs_command (ABORT(i)) = A.ABORT(i) 
      | abs_command (ASSIGN(i, identifier, vir_or_not, expression)) =
	abs_vir_or_not (abs_identifier identifier, abs_expression expression) vir_or_not
      | abs_command (IFFI(i, guard_list)) = A.IFFI(i, abs_guard_list guard_list)
      | abs_command (DOOD(i, guard_list)) = A.DOOD(i, abs_guard_list guard_list)
      | abs_command (BLOCK(i, block)) = A.BLOCK(i, abs_block block)

    and abs_vir_or_not (a1, a2) (VIR(i)) = A.VIR(i, a1, a2)
      | abs_vir_or_not (a1, a2) (ASS(i)) = A.ASSIGN(i, a1, a2)

    and abs_guard_list (GUARD_LIST(i, guard, rest_of_guard_list)) =
	abs_rest_of_guard_list (abs_guard guard) rest_of_guard_list

    and abs_rest_of_guard_list a EPSgua = a
      | abs_rest_of_guard_list a (TACTgua(i, guard_list)) = 
	A.TACT(i, a, abs_guard_list guard_list)
	
    and abs_guard (ARROW(i, expression, command_list)) = 
	A.ARROW(i, abs_expression expression, abs_command_list command_list)

    and abs_block (BEGIN(i, declaration_list, command_list)) = 
	A.BEGIN(i, abs_declaration_list declaration_list, abs_command_list command_list)
	
    and abs_declaration_list (DECLARATION_LIST(i, declaration, rest_of_declaration_list)) =
	abs_rest_of_declaration_list (abs_declaration declaration) rest_of_declaration_list

    and abs_rest_of_declaration_list a EPSdec = a
      | abs_rest_of_declaration_list a (SEMIdec(i, declaration_list)) = 
	A.COMPOUND(i, a, abs_declaration_list declaration_list)
	
    and abs_declaration (PRIVAR(i, identifier, type_exp)) = 
	A.PRIVAR(i, abs_identifier identifier, abs_type_exp type_exp)
      | abs_declaration (PRICON(i, identifier, type_exp)) = 
	A.PRICON(i, abs_identifier identifier, abs_type_exp type_exp)
      | abs_declaration (VIRVAR(i, identifier, type_or_not)) = 
	A.VIRVAR(i, abs_identifier identifier, abs_type_or_not type_or_not)
      | abs_declaration (VIRCON(i, identifier, type_or_not)) = 
	A.VIRCON(i, abs_identifier identifier, abs_type_or_not type_or_not)
      | abs_declaration (GLOVAR(i, identifier, type_or_not)) = 
	A.GLOVAR(i, abs_identifier identifier, abs_type_or_not type_or_not)
      | abs_declaration (GLOCON(i, identifier, type_or_not)) = 
	A.GLOCON(i, abs_identifier identifier, abs_type_or_not type_or_not)
	
    and abs_type_or_not (EPSton) = A.EPSton
      | abs_type_or_not (COLON(i, type_exp)) = A.COLON(i, abs_type_exp type_exp)

    and abs_type0_atom (INT(i)) = A.INT(i)
      | abs_type0_atom (BOOL(i)) = A.BOOL(i)
      | abs_type0_atom (PARtyp(i, type_exp)) = abs_type_exp type_exp

    and abs_type1_atom (TYPE1_ATOM(i, type0_atom, type1_atom')) =
	abs_type1_atom' (abs_type0_atom type0_atom) type1_atom'

    and abs_type1_atom' a EPS1typ = a
      | abs_type1_atom' a (ARROWtyp(i, type_exp)) = A.FUN(i, a, abs_type_exp type_exp)

    and abs_type_exp (TYPE_EXP(i, type1_atom)) = abs_type1_atom type1_atom
	
    and abs_a0_atom (ID(i, identifier)) = A.ID(i, abs_identifier identifier)
      | abs_a0_atom (BLOCKexp(i, block)) = A.BLOCKexp(i, abs_block block)
      | abs_a0_atom (NUM(i, numeral)) = A.NUM(i, abs_numeral numeral)
      | abs_a0_atom (FAL(i, falseval)) = A.FAL(i, abs_falseval falseval)
      | abs_a0_atom (NEG(i, a0_atom)) = A.APP(i, A.NEG(i), abs_a0_atom a0_atom)
      | abs_a0_atom (NOT(i, a0_atom)) = A.APP(i, A.NOT(i), abs_a0_atom a0_atom)
      | abs_a0_atom (PAR(i, expression)) = abs_expression expression

    and abs_a1_atom (A1_ATOM(i, a0_atom, a1_atom')) =
	     abs_a1_atom' (abs_a0_atom a0_atom) a1_atom'

    and abs_a1_atom' a EPS1 = a
      | abs_a1_atom' a (APP(i, a0_atom, a1_atom')) = 
	     abs_a1_atom' (A.APP(i, a, abs_a0_atom a0_atom)) a1_atom'
  
    and abs_a2_atom (A2_ATOM(i, a1_atom, a2_atom')) =
	     abs_a2_atom' (abs_a1_atom a1_atom) a2_atom'

    and abs_a2_atom' a EPS2 = a
      | abs_a2_atom' a (DIV(i, a1_atom, a2_atom')) = 
	     abs_a2_atom' (A.APP(i, A.APP(i, A.DIV(i), a), abs_a1_atom a1_atom)) a2_atom'
      | abs_a2_atom' a (MOD(i, a1_atom, a2_atom')) = 
	     abs_a2_atom' (A.APP(i, A.APP(i, A.MOD(i), a), abs_a1_atom a1_atom)) a2_atom'
      | abs_a2_atom' a (TIMES(i, a1_atom, a2_atom')) = 
	     abs_a2_atom' (A.APP(i, A.APP(i, A.TIMES(i), a), abs_a1_atom a1_atom)) a2_atom'

    and abs_a3_atom (A3_ATOM(i, a2_atom, a3_atom')) =
	     abs_a3_atom' (abs_a2_atom a2_atom) a3_atom'

    and abs_a3_atom' a EPS3 = a
      | abs_a3_atom' a (PLUS(i, a2_atom, a3_atom')) = 
	     abs_a3_atom' (A.APP(i, A.APP(i, A.PLUS(i), a), abs_a2_atom a2_atom)) a3_atom'
      | abs_a3_atom' a (MINUS(i, a2_atom, a3_atom')) = 
	     abs_a3_atom' (A.APP(i, A.APP(i, A.MINUS(i), a), abs_a2_atom a2_atom)) a3_atom'

    and abs_a4_atom (A4_ATOM(i, a3_atom, a4_atom')) =
	abs_a4_atom' (abs_a3_atom a3_atom) a4_atom'

    and abs_a4_atom' a EPS4 = a
      | abs_a4_atom' a (EQUAL(i, a3_atom, a4_atom')) = 
	     abs_a4_atom' (A.APP(i, A.APP(i, A.EQUAL(i), a), abs_a3_atom a3_atom)) a4_atom'
      | abs_a4_atom' a (DIFF(i, a3_atom, a4_atom')) = 
	     abs_a4_atom' (A.APP(i, A.APP(i, A.DIFF(i), a), abs_a3_atom a3_atom)) a4_atom'
      | abs_a4_atom' a (LESS(i, a3_atom, a4_atom')) = 
	     abs_a4_atom' (A.APP(i, A.APP(i, A.LESS(i), a), abs_a3_atom a3_atom)) a4_atom'
      | abs_a4_atom' a (GREAT(i, a3_atom, a4_atom')) = 
	     abs_a4_atom' (A.APP(i, A.APP(i, A.GREAT(i), a), abs_a3_atom a3_atom)) a4_atom'
      | abs_a4_atom' a (EQLESS(i, a3_atom, a4_atom')) = 
	     abs_a4_atom' (A.APP(i, A.APP(i, A.EQLESS(i), a), abs_a3_atom a3_atom)) a4_atom'
      | abs_a4_atom' a (EQGREAT(i, a3_atom, a4_atom')) = 
	     abs_a4_atom' (A.APP(i, A.APP(i, A.EQGREAT(i), a), abs_a3_atom a3_atom)) a4_atom'

    and abs_a5_atom (A5_ATOM(i, a4_atom, a5_atom')) =
	abs_a5_atom' (abs_a4_atom a4_atom) a5_atom'

    and abs_a5_atom' a EPS5 = a
      | abs_a5_atom' a (AND(i, a4_atom, a5_atom')) = 
	     abs_a5_atom' (A.APP(i, A.APP(i, A.AND(i), a), abs_a4_atom a4_atom)) a5_atom'
      | abs_a5_atom' a (OR(i, a4_atom, a5_atom')) = 
	     abs_a5_atom' (A.APP(i, A.APP(i, A.OR(i), a), abs_a4_atom a4_atom)) a5_atom'

    and abs_expression (EXPRESSION(i, a5_atom)) = abs_a5_atom a5_atom

    and abs_identifier (IDENTIFIER(i, id)) = A.IDENTIFIER(i, id)

    and abs_numeral (NUMERAL(i, num)) = A.NUMERAL(i, num)

    and abs_falseval (FALSEVAL(i, fal)) = A.FALSEVAL(i, fal)

end

end
