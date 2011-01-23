(**********)
(* Parser *)
(**********)


functor Parser

    ( structure LL1Grammar' : LL1_GRAMMAR

      structure OkError' : OK_ERROR

      structure ParserError' : PARSER_ERROR

      sharing OkError'.Error = ParserError'

      structure InfoAndTokenList' : INFO_AND_TOKEN_LIST

      sharing LL1Grammar'.CharInfo = InfoAndTokenList'.CharInfo

      sharing OkError'.CharInfo = InfoAndTokenList'.CharInfo

      structure Tokens' : TOKENS

      sharing ParserError'.Tokens = Tokens'

      sharing LL1Grammar'.Identifier = Tokens'.Identifier

      sharing LL1Grammar'.Numeral = Tokens'.Numeral

      sharing LL1Grammar'.Falseval = Tokens'.Falseval

      sharing InfoAndTokenList'.Tokens = Tokens' ) : sig 

	                                                 include PARSER 

                                                         sharing LL1Grammar = LL1Grammar'

							 sharing OkError = OkError'

							 sharing ParserError = ParserError'

							 sharing InfoAndTokenList = InfoAndTokenList'

							 sharing Tokens = Tokens'

						     end =

struct

    structure LL1Grammar = LL1Grammar'

    structure OkError = OkError'

    structure ParserError = ParserError'

    structure InfoAndTokenList = InfoAndTokenList'

    structure Tokens = Tokens'

local

    open LL1Grammar

    open OkError

    open Tokens

    open ParserError

    open InfoAndTokenList

in

    fun get_program (NIL(i)) = ERROR(i, PROGRAM_TOO_SHORT)
      | get_program (l as CONS(i, _, _)) = 
	(case get_block l
	   of OK(block, rest1) => 
	       (case get_void rest1 
		  of OK(gi) => (* possibly some test, e.g. for not allowed chars *)
		      OK(BLOCKprog(i, block))
		   | ERROR(e) => ERROR(e))
	    | ERROR(e) => ERROR(e))
    
    and get_command_list (NIL(i)) = ERROR(i, PROGRAM_TOO_SHORT)
      | get_command_list (l as CONS(i, _, _)) = 
	(case get_command l
	   of OK(command, rest1) =>
	       (case get_rest_of_command_list rest1 
		  of OK(rest_of_command_list, rest2) => 
		      OK(COMMAND_LIST(i, command, rest_of_command_list), rest2)
		   | ERROR(e) => ERROR(e))
	    | ERROR(e) => ERROR(e))

    and get_rest_of_command_list (CONS(i, T_SEMICOLON, rest)) =
	(case get_command_list rest
	   of OK(command_list, rest1) => OK(SEMIcom(i, command_list), rest1)
	    | ERROR(e) => ERROR(e))
      | get_rest_of_command_list l = OK(EPScom, l)

    and get_command (NIL(i)) = ERROR(i, PROGRAM_TOO_SHORT)
      | get_command (CONS(i, T_SKIP, rest)) = OK(SKIP(i), rest)
      | get_command (CONS(i, T_ABORT, rest)) = OK(ABORT(i), rest)
      | get_command (l as CONS(i, T_ID(_), _)) = 
	(case get_identifier l
	   of OK(identifier, rest1) =>
	       (case get_vir_or_not rest1
		  of OK(vir_or_not, rest2) =>
		      (case get_expression rest2
			 of OK(expression, rest3) =>
			       OK(ASSIGN(i, identifier, vir_or_not, expression), rest3)
			  | ERROR(e) => ERROR(e))
		   | ERROR(e) => ERROR(e))
	    | ERROR(e) => ERROR(e))
      | get_command (CONS(i, T_IF, rest)) = 
	(case get_guard_list rest
	   of OK(guard_list, rest1) =>
	       (case rest1 
		  of CONS(_, T_FI, rest2) =>
		      OK(IFFI(i, guard_list), rest2)
		   | CONS(ie, t, _) => ERROR(ie, TOKEN_EXPECTED(T_FI))
		   | NIL(i) => ERROR(i, PROGRAM_TOO_SHORT))
	    | ERROR(e) => ERROR(e))
      | get_command (CONS(i, T_DO, rest)) = 
	(case get_guard_list rest
	   of OK(guard_list, rest1) =>
	       (case rest1 
		  of CONS(_, T_OD, rest2) =>
		      OK(DOOD(i, guard_list), rest2)
		   | CONS(ie, t, _) => ERROR(ie, TOKEN_EXPECTED(T_OD))
		   | NIL(i) => ERROR(i, PROGRAM_TOO_SHORT))
	    | ERROR(e) => ERROR(e))
      | get_command (l as CONS(i, T_BEGIN, _)) =                
         (case get_block l
	   of OK(block, rest1) =>
	       OK(BLOCK(i, block), rest1)
	    | ERROR(e) => ERROR(e))
      | get_command (CONS(i, t, _)) = ERROR(i, COMMAND_UNKNOWN(t))

    and get_vir_or_not (NIL(i)) = ERROR(i, PROGRAM_TOO_SHORT)
      | get_vir_or_not (CONS(i, T_VIR, rest)) = OK(VIR(i), rest)
      | get_vir_or_not (CONS(i, T_ASSIGN, rest)) = OK(ASS(i), rest)
      | get_vir_or_not (CONS(i, t, _)) = ERROR(i, ASSIGNMENT_UNKNOWN(t))

    and get_guard_list (NIL(i)) = ERROR(i, PROGRAM_TOO_SHORT)
      | get_guard_list (l as CONS(i, _, _)) = 
	(case get_guard l
	   of OK(guard, rest1) =>
	       (case get_rest_of_guard_list rest1 
		  of OK(rest_of_guard_list, rest2) => 
		      OK(GUARD_LIST(i, guard, rest_of_guard_list), rest2)
		   | ERROR(e) => ERROR(e))
	    | ERROR(e) => ERROR(e))

    and get_rest_of_guard_list (CONS(i, T_TACT, rest)) =
	(case get_guard_list rest
	   of OK(guard_list, rest1) => OK(TACTgua(i, guard_list), rest1)
	    | ERROR(e) => ERROR(e))
      | get_rest_of_guard_list l = OK(EPSgua, l)

    and get_guard (NIL(i)) = ERROR(i, PROGRAM_TOO_SHORT)
      | get_guard (l as CONS(i, _, _)) = 
	(case get_expression l
	   of OK(expression, rest1) =>
	       (case rest1
		  of CONS(_, T_ARROW, rest2) =>
		      (case get_command_list rest2
			 of OK(command_list, rest3) =>
			     OK(ARROW(i, expression, command_list), rest3)
			  | ERROR(e) => ERROR(e))
		   | CONS(ie, t, _) => ERROR(ie, TOKEN_EXPECTED(T_ARROW))
		   | NIL(i) => ERROR(i, PROGRAM_TOO_SHORT))		    
	    | ERROR(e) => ERROR(e))  

    and get_block (NIL(i)) = ERROR(i, PROGRAM_TOO_SHORT)
      | get_block (CONS(i, T_BEGIN, rest)) =
	(case get_declaration_list rest
	   of OK(declaration_list, rest1) =>
	       (case get_command_list rest1
		  of OK(command_list, rest2) =>
		      (case rest2 
			 of CONS(_, T_END, rest3) =>
			     OK(BEGIN(i, declaration_list, command_list), rest3)
			  | CONS(ie, t, _) => ERROR(ie, TOKEN_EXPECTED(T_END))
			  | NIL(i) => ERROR(i, PROGRAM_TOO_SHORT))
		   | ERROR(e) => ERROR(e))
	    | ERROR(e) => ERROR(e))
      | get_block (CONS(i, t, _)) = ERROR(i, BLOCK_UNKNOWN(t))

    and get_declaration_list (NIL(i)) = ERROR(i, PROGRAM_TOO_SHORT)
      | get_declaration_list (l as CONS(i, _, _)) = 
	(case get_declaration l
	   of OK(declaration, rest1) =>
	       (case get_rest_of_declaration_list rest1
		  of OK(rest_of_declaration_list, rest2) => 
		      OK(DECLARATION_LIST(i, declaration, rest_of_declaration_list), rest2)
		   | ERROR(e) => ERROR(e))
	    | ERROR(e) => ERROR(e))

    and get_rest_of_declaration_list (CONS(i, T_SEMICOLON, rest)) =
	(case get_declaration_list rest
	   of OK(declaration_list, rest1) => OK(SEMIdec(i, declaration_list), rest1)
	    | ERROR(e) => ERROR(e))
      | get_rest_of_declaration_list l = OK(EPSdec, l)

    and get_declaration (NIL(i)) = ERROR(i, PROGRAM_TOO_SHORT)
      | get_declaration (CONS(i, T_PRIVAR, rest)) =
	(case get_identifier rest
	   of OK(identifier, rest1) =>
	       (case rest1
		  of CONS(_, T_COLON, rest2) =>
		      (case get_type_exp rest2
			 of OK(type_exp, rest3) =>
			     OK(PRIVAR(i, identifier, type_exp), rest3)
			  | ERROR(e) => ERROR(e))
		   | CONS(ie, t, _) => ERROR(ie, TOKEN_EXPECTED(T_COLON))
		   | NIL(i) => ERROR(i, PROGRAM_TOO_SHORT))
	    | ERROR(e) => ERROR(e))
      | get_declaration (CONS(i, T_PRICON, rest)) =
	(case get_identifier rest
	   of OK(identifier, rest1) =>
	       (case rest1
		  of CONS(_, T_COLON, rest2) =>
		      (case get_type_exp rest2
			 of OK(type_exp, rest3) =>
			     OK(PRICON(i, identifier, type_exp), rest3)
			  | ERROR(e) => ERROR(e))
		   | CONS(ie, t, _) => ERROR(ie, TOKEN_EXPECTED(T_COLON))
		   | NIL(i) => ERROR(i, PROGRAM_TOO_SHORT))
	    | ERROR(e) => ERROR(e))
      | get_declaration (CONS(i, T_VIRVAR, rest)) =
	(case get_identifier rest
	   of OK(identifier, rest1) =>
	       (case get_type_or_not rest1
		  of OK(type_or_not, rest2) =>
		      OK(VIRVAR(i, identifier, type_or_not), rest2)
		   | ERROR(e) => ERROR(e))
	    | ERROR(e) => ERROR(e))
      | get_declaration (CONS(i, T_VIRCON, rest)) =
	(case get_identifier rest
	   of OK(identifier, rest1) =>
	       (case get_type_or_not rest1
		  of OK(type_or_not, rest2) =>
		      OK(VIRCON(i, identifier, type_or_not), rest2)
		   | ERROR(e) => ERROR(e))
	    | ERROR(e) => ERROR(e))
      | get_declaration (CONS(i, T_GLOVAR, rest)) =
	(case get_identifier rest
	   of OK(identifier, rest1) =>
	       (case get_type_or_not rest1
		  of OK(type_or_not, rest2) =>
		      OK(GLOVAR(i, identifier, type_or_not), rest2)
		   | ERROR(e) => ERROR(e))
	    | ERROR(e) => ERROR(e))
      | get_declaration (CONS(i, T_GLOCON, rest)) =
	(case get_identifier rest
	   of OK(identifier, rest1) =>
	       (case get_type_or_not rest1
		  of OK(type_or_not, rest2) =>
		      OK(GLOCON(i, identifier, type_or_not), rest2)
		   | ERROR(e) => ERROR(e))
	    | ERROR(e) => ERROR(e))
      | get_declaration (CONS(i, t, _)) = ERROR(i, DECLARATION_UNKNOWN(t))

    and get_type_or_not (CONS(i, T_COLON, rest)) = 
	(case get_type_exp rest
	     of OK(type_exp, rest1) => OK(COLON(i, type_exp), rest1)
	      | ERROR(e) => ERROR(e))
      | get_type_or_not l = OK(EPSton, l)

    and get_type0_atom (NIL(i)) = ERROR(i, PROGRAM_TOO_SHORT)
      | get_type0_atom (CONS(i, T_TYPE_INT, rest)) = OK(INT(i), rest)
      | get_type0_atom (CONS(i, T_TYPE_BOOL, rest)) = OK(BOOL(i), rest)
      | get_type0_atom (CONS(i, T_LPAREN, rest)) =
	(case get_type_exp rest
	   of OK(type_exp, rest1) =>
	       (case rest1
		  of (CONS(i, T_RPAREN, rest2)) =>
		      OK(PARtyp(i, type_exp), rest2)
		   | CONS(ie, t, _) => ERROR(ie, TOKEN_EXPECTED(T_RPAREN))
		   | NIL(i) => ERROR(i, PROGRAM_TOO_SHORT))
	    | ERROR(e) => ERROR(e))
      | get_type0_atom (CONS(i, t, _)) = ERROR(i, TYPE0_ATOM_UNKNOWN(t))

    and get_type1_atom (NIL(i)) = ERROR(i, PROGRAM_TOO_SHORT)
      | get_type1_atom (l as CONS(i, _, _)) =
	(case get_type0_atom l
	   of OK(type0_atom, rest0) => 
	       (case get_type1_atom' rest0
		  of OK(type1_atom', rest1) =>
		      OK(TYPE1_ATOM(i, type0_atom, type1_atom'), rest1)
		   | ERROR(e) => ERROR(e))
	    | ERROR(e) => ERROR(e))

    and get_type1_atom' (CONS(i, T_ARROW, rest)) =
	(case get_type_exp rest
	   of OK(type_exp, rest1) =>
		      OK(ARROWtyp(i, type_exp), rest1)
	    | ERROR(e) => ERROR(e))
      | get_type1_atom' l = OK(EPS1typ, l)

    and get_type_exp (NIL(i)) = ERROR(i, PROGRAM_TOO_SHORT)
      | get_type_exp (l as CONS(i, _, _)) =
	(case get_type1_atom l
	   of OK(type1_atom, rest1) => 
	       OK(TYPE_EXP(i, type1_atom), rest1)
	    | ERROR(e) => ERROR(e))

    and get_a0_atom (NIL(i)) = ERROR(i, PROGRAM_TOO_SHORT)
      | get_a0_atom (l as CONS(i, T_ID(_), _)) =
	(case get_identifier l
	   of OK(identifier, rest1) =>
	       OK(ID(i, identifier), rest1)
	    | ERROR(e) => ERROR(e))
      | get_a0_atom (l as CONS(i, T_BEGIN, _)) =
	(case get_block l
	   of OK(block, rest1) =>
	       OK(BLOCKexp(i, block), rest1)
	    | ERROR(e) => ERROR(e))
      | get_a0_atom (l as CONS(i, T_NUM(_), _)) =
	(case get_numeral l
	   of OK(numeral, rest1) =>
	       OK(NUM(i, numeral), rest1)
	    | ERROR(e) => ERROR(e))
      | get_a0_atom (l as CONS(i, T_FAL(_), _)) =
	(case get_falseval l
	   of OK(falseval, rest1) =>
	       OK(FAL(i, falseval), rest1)
	    | ERROR(e) => ERROR(e))
      | get_a0_atom (CONS(i, T_NEG, rest)) =
	(case get_a0_atom rest
	   of OK(a0_atom, rest1) =>
	       OK(NEG(i, a0_atom), rest1)
	    | ERROR(e) => ERROR(e))
      | get_a0_atom (CONS(i, T_NOT, rest)) =
	(case get_a0_atom rest
	   of OK(a0_atom, rest1) =>
	       OK(NOT(i, a0_atom), rest1)
	    | ERROR(e) => ERROR(e))
      | get_a0_atom (CONS(i, T_LPAREN, rest)) =
	(case get_expression rest
	   of OK(expression, rest1) =>
	       (case rest1
		  of (CONS(i, T_RPAREN, rest2)) =>
		      OK(PAR(i, expression), rest2)
		   | CONS(ie, t, _) => ERROR(ie, TOKEN_EXPECTED(T_RPAREN))
		   | NIL(i) => ERROR(i, PROGRAM_TOO_SHORT))
	    | ERROR(e) => ERROR(e))
      | get_a0_atom (CONS(i, t, _)) = ERROR(i, A0_ATOM_UNKNOWN(t))

    and get_a1_atom (NIL(i)) = ERROR(i, PROGRAM_TOO_SHORT)
      | get_a1_atom (l as CONS(i, _, _)) =
	(case get_a0_atom l
	   of OK(a0_atom, rest0) => 
	       (case get_a1_atom' rest0
		  of OK(a1_atom', rest1) =>
		      OK(A1_ATOM(i, a0_atom, a1_atom'), rest1)
		   | ERROR(e) => ERROR(e))
	    | ERROR(e) => ERROR(e))

    and get_a1_atom' (CONS(i, T_DOT, rest)) =
	(case get_a0_atom rest
	   of OK(a0_atom, rest0) => 
	       (case get_a1_atom' rest0
		  of OK(a1_atom', rest1) =>
		      OK(APP(i, a0_atom, a1_atom'), rest1)
		   | ERROR(e) => ERROR(e))
	    | ERROR(e) => ERROR(e))
      | get_a1_atom' l = OK(EPS1, l)

    and get_a2_atom (NIL(i)) = ERROR(i, PROGRAM_TOO_SHORT)
      | get_a2_atom (l as CONS(i, _, _)) =
	(case get_a1_atom l
	   of OK(a1_atom, rest1) => 
	       (case get_a2_atom' rest1
		  of OK(a2_atom', rest2) =>
		      OK(A2_ATOM(i, a1_atom, a2_atom'), rest2)
		   | ERROR(e) => ERROR(e))
	    | ERROR(e) => ERROR(e))

    and get_a2_atom' (CONS(i, T_DIV, rest)) =
	(case get_a1_atom rest
	   of OK(a1_atom, rest1) => 
	       (case get_a2_atom' rest1
		  of OK(a2_atom', rest2) =>
		      OK(DIV(i, a1_atom, a2_atom'), rest2)
		   | ERROR(e) => ERROR(e))
	    | ERROR(e) => ERROR(e))
      | get_a2_atom' (CONS(i, T_MOD, rest)) =
	(case get_a1_atom rest
	   of OK(a1_atom, rest1) => 
	       (case get_a2_atom' rest1
		  of OK(a2_atom', rest2) =>
		      OK(MOD(i, a1_atom, a2_atom'), rest2)
		   | ERROR(e) => ERROR(e))
	    | ERROR(e) => ERROR(e))
      | get_a2_atom' (CONS(i, T_TIMES, rest)) =
	(case get_a1_atom rest
	   of OK(a1_atom, rest1) => 
	       (case get_a2_atom' rest1
		  of OK(a2_atom', rest2) =>
		      OK(TIMES(i, a1_atom, a2_atom'), rest2)
		   | ERROR(e) => ERROR(e))
	    | ERROR(e) => ERROR(e))
      | get_a2_atom' l = OK(EPS2, l)

    and get_a3_atom (NIL(i)) = ERROR(i, PROGRAM_TOO_SHORT)
      | get_a3_atom (l as CONS(i, _, _)) =
	(case get_a2_atom l
	   of OK(a2_atom, rest2) => 
	       (case get_a3_atom' rest2
		  of OK(a3_atom', rest3) =>
		      OK(A3_ATOM(i, a2_atom, a3_atom'), rest3)
		   | ERROR(e) => ERROR(e))
	    | ERROR(e) => ERROR(e))

    and get_a3_atom' (CONS(i, T_PLUS, rest)) =
	(case get_a2_atom rest
	   of OK(a2_atom, rest2) => 
	       (case get_a3_atom' rest2
		  of OK(a3_atom', rest3) =>
		      OK(PLUS(i, a2_atom, a3_atom'), rest3)
		   | ERROR(e) => ERROR(e))
	    | ERROR(e) => ERROR(e))
      | get_a3_atom' (CONS(i, T_MINUS, rest)) =
	(case get_a2_atom rest
	   of OK(a2_atom, rest2) => 
	       (case get_a3_atom' rest2
		  of OK(a3_atom', rest3) =>
		      OK(MINUS(i, a2_atom, a3_atom'), rest3)
		   | ERROR(e) => ERROR(e))
	    | ERROR(e) => ERROR(e))
      | get_a3_atom' l = OK(EPS3, l)

    and get_a4_atom (NIL(i)) = ERROR(i, PROGRAM_TOO_SHORT)
      | get_a4_atom (l as CONS(i, _, _)) =
	(case get_a3_atom l
	   of OK(a3_atom, rest3) => 
	       (case get_a4_atom' rest3
		  of OK(a4_atom', rest4) =>
		      OK(A4_ATOM(i, a3_atom, a4_atom'), rest4)
		   | ERROR(e) => ERROR(e))
	    | ERROR(e) => ERROR(e))

    and get_a4_atom' (CONS(i, T_EQUAL, rest)) =
	(case get_a3_atom rest
	   of OK(a3_atom, rest3) => 
	       (case get_a4_atom' rest3
		  of OK(a4_atom', rest4) =>
		      OK(EQUAL(i, a3_atom, a4_atom'), rest4)
		   | ERROR(e) => ERROR(e))
	    | ERROR(e) => ERROR(e))
      | get_a4_atom' (CONS(i, T_DIFF, rest)) =
	(case get_a3_atom rest
	   of OK(a3_atom, rest3) => 
	       (case get_a4_atom' rest3
		  of OK(a4_atom', rest4) =>
		      OK(DIFF(i, a3_atom, a4_atom'), rest4)
		   | ERROR(e) => ERROR(e))
	    | ERROR(e) => ERROR(e))
      | get_a4_atom' (CONS(i, T_LESS, rest)) =
	(case get_a3_atom rest
	   of OK(a3_atom, rest3) => 
	       (case get_a4_atom' rest3
		  of OK(a4_atom', rest4) =>
		      OK(LESS(i, a3_atom, a4_atom'), rest4)
		   | ERROR(e) => ERROR(e))
	    | ERROR(e) => ERROR(e))
      | get_a4_atom' (CONS(i, T_GREAT, rest)) =
	(case get_a3_atom rest
	   of OK(a3_atom, rest3) => 
	       (case get_a4_atom' rest3
		  of OK(a4_atom', rest4) =>
		      OK(GREAT(i, a3_atom, a4_atom'), rest4)
		   | ERROR(e) => ERROR(e))
	    | ERROR(e) => ERROR(e))
      | get_a4_atom' (CONS(i, T_EQLESS, rest)) =
	(case get_a3_atom rest
	   of OK(a3_atom, rest3) => 
	       (case get_a4_atom' rest3
		  of OK(a4_atom', rest4) =>
		      OK(EQLESS(i, a3_atom, a4_atom'), rest4)
		   | ERROR(e) => ERROR(e))
	    | ERROR(e) => ERROR(e))
      | get_a4_atom' (CONS(i, T_EQGREAT, rest)) =
	(case get_a3_atom rest
	   of OK(a3_atom, rest3) => 
	       (case get_a4_atom' rest3
		  of OK(a4_atom', rest4) =>
		      OK(EQGREAT(i, a3_atom, a4_atom'), rest4)
		   | ERROR(e) => ERROR(e))
	    | ERROR(e) => ERROR(e))
      | get_a4_atom' l = OK(EPS4, l)

    and get_a5_atom (NIL(i)) = ERROR(i, PROGRAM_TOO_SHORT)
      | get_a5_atom (l as CONS(i, _, _)) =
	(case get_a4_atom l
	   of OK(a4_atom, rest4) => 
	       (case get_a5_atom' rest4
		  of OK(a5_atom', rest5) =>
		      OK(A5_ATOM(i, a4_atom, a5_atom'), rest5)
		   | ERROR(e) => ERROR(e))
	    | ERROR(e) => ERROR(e))

    and get_a5_atom' (CONS(i, T_AND, rest)) =
	(case get_a4_atom rest
	   of OK(a4_atom, rest4) => 
	       (case get_a5_atom' rest4
		  of OK(a5_atom', rest5) =>
		      OK(AND(i, a4_atom, a5_atom'), rest5)
		   | ERROR(e) => ERROR(e))
	    | ERROR(e) => ERROR(e))
      | get_a5_atom' (CONS(i, T_OR, rest)) =
	(case get_a4_atom rest
	   of OK(a4_atom, rest4) => 
	       (case get_a5_atom' rest4
		  of OK(a5_atom', rest5) =>
		      OK(OR(i, a4_atom, a5_atom'), rest5)
		   | ERROR(e) => ERROR(e))
	    | ERROR(e) => ERROR(e))
      | get_a5_atom' l = OK(EPS5, l)

    and get_expression (NIL(i)) = ERROR(i, PROGRAM_TOO_SHORT)
      | get_expression (l as CONS(i, _, _)) =
	(case get_a5_atom l
	   of OK(a5_atom, rest5) => 
	       OK(EXPRESSION(i, a5_atom), rest5)
	    | ERROR(e) => ERROR(e))

    and get_identifier (NIL(i)) = ERROR(i, PROGRAM_TOO_SHORT)
      | get_identifier (CONS(i, T_ID(id), rest)) = OK(IDENTIFIER(i, id), rest)
      | get_identifier (CONS(i, t, _)) = ERROR(i, IDENTIFIER_UNKNOWN(t))

    and get_numeral (NIL(i)) = ERROR(i, PROGRAM_TOO_SHORT)
      | get_numeral (CONS(i, T_NUM(num), rest)) = OK(NUMERAL(i, num), rest)
      | get_numeral (CONS(i, t, _)) = ERROR(i, NUMERAL_UNKNOWN(t))

    and get_falseval (NIL(i)) = ERROR(i, PROGRAM_TOO_SHORT)
      | get_falseval (CONS(i, T_FAL(fal), rest)) = OK(FALSEVAL(i, fal), rest)
      | get_falseval (CONS(i, t, _)) = ERROR(i, FALSEVAL_UNKNOWN(t))

    and get_void (NIL(i)) = OK(i)
      | get_void (CONS(i, _, _)) = ERROR(i, PROGRAM_TOO_LONG) 

end

end