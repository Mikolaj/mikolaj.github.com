(**************)
(* LL1Grammar *)
(**************)


(*

<program>
  -> <block>

<command_list>
  -> <command> <rest_of_command_list>

<rest_of_command_list>
  -> epsilon
   | ; <command_list>

<command>
  -> skip
   | abort
   | <identifier> <vir_or_not> <expression>
   | if <guard_list> fi
   | do <guard_list> od
   | <block>

<vir_or_not>
  -> :vir= 
   | :=

<guard_list>
  -> <guard> <rest_of_guard_list>

<rest_of_guard_list>
  -> epsilon
   | [] <guard_list>

<guard>
  -> <expression> -> <command_list>

<block>
  -> begin <declaration_list> <command_list> end

<declaration_list>
  -> <declaration> <rest_of_declaration_list>

<rest_of_declaration_list>
  -> epsilon
   | ; <declaration_list>

<declaration>
  -> privar <identifier> : <type_exp>
   | pricon <identifier> : <type_exp>
   | virvar <identifier> <type_or_not>
   | vircon <identifier> <type_or_not>
   | glovar <identifier> <type_or_not>
   | glocon <identifier> <type_or_not>

<type_or_not>
  -> epsilon
   | : <type_exp>

<type0_atom>
  -> int
   | bool
   | ( <type_exp> )    

<type1_atom>
  -> <type0_atom> <type1_atom'>

<type1_atom'>
  -> epsilon
   | -> <type0_atom> <type1_atom'>

<type_exp>
  -> <type1_atom>

<a0_atom>
  -> <identifier>
   | block
   | <numeral>
   | <falseval>
   | ~ <a0_atom>
   | not <a0_atom>
   | ( <expression> )

<a1_atom>
  -> <a0_atom> <a1_atom'>

<a1_atom'>
  -> epsilon
   | . <a0_atom> <a1_atom'>

<a2_atom>
  -> <a1_atom> <a2_atom'>

<a2_atom'>
  -> epsilon
   | div <a1_atom> <a2_atom'>
   | mod <a1_atom> <a2_atom'>
   | * <a1_atom> <a2_atom'>

<a3_atom>
  -> <a2_atom> <a3_atom'>

<a3_atom'>
  -> epsilon
   | + <a2_atom> <a3_atom'>
   | - <a2_atom> <a3_atom'>

<a4_atom>
  -> <a3_atom> <a4_atom'>

<a4_atom'>
  -> epsilon
   | = <a3_atom> <a4_atom'>
   | <> <a3_atom> <a4_atom'>
   | < <a3_atom> <a4_atom'>
   | > <a3_atom> <a4_atom'>
   | <= <a3_atom> <a4_atom'>
   | >= <a3_atom> <a4_atom'>

<a5_atom>
  -> <a4_atom> <a5_atom'>

<a5_atom'>
  -> epsilon
   | and <a4_atom> <a5_atom'>
   | or <a4_atom> <a5_atom'>

<expression> 
  -> <a5_atom>

<identifier>
  -> blabla | bleble  ...

<numeral>
  -> 1 | 2 ...

<falseval>
  -> true
   | false

*)

signature LL1_GRAMMAR =
sig

    structure CharInfo : sig eqtype info end

    structure Identifier : sig type id end

    structure Numeral : sig type num end

    structure Falseval : sig type fal end

    datatype program =
	BLOCKprog of CharInfo.info * block

    and command_list =
	COMMAND_LIST of CharInfo.info * command * rest_of_command_list

    and rest_of_command_list =
        EPScom 
      | SEMIcom of CharInfo.info * command_list

    and command =
	SKIP of CharInfo.info
      | ABORT of CharInfo.info
      | ASSIGN of CharInfo.info * identifier * vir_or_not * expression
      | IFFI of CharInfo.info * guard_list
      | DOOD of CharInfo.info * guard_list
      | BLOCK of CharInfo.info * block

    and vir_or_not =
	VIR of CharInfo.info
      | ASS of CharInfo.info

    and guard_list =
	GUARD_LIST of CharInfo.info * guard * rest_of_guard_list

    and rest_of_guard_list =
	EPSgua 
      | TACTgua of CharInfo.info * guard_list

    and guard =
	ARROW of CharInfo.info * expression * command_list

    and block =
	BEGIN of CharInfo.info * declaration_list * command_list

    and declaration_list =
	DECLARATION_LIST of CharInfo.info * declaration * rest_of_declaration_list

    and rest_of_declaration_list =
	EPSdec
      | SEMIdec of CharInfo.info * declaration_list

    and declaration =
	PRIVAR of CharInfo.info * identifier * type_exp
      | PRICON of CharInfo.info * identifier * type_exp
      | VIRVAR of CharInfo.info * identifier * type_or_not
      | VIRCON of CharInfo.info * identifier * type_or_not
      | GLOVAR of CharInfo.info * identifier * type_or_not
      | GLOCON of CharInfo.info * identifier * type_or_not

    and type_or_not =
	EPSton
      | COLON of CharInfo.info * type_exp
   
    and type0_atom =
        INT of CharInfo.info
      | BOOL of CharInfo.info
      | PARtyp of CharInfo.info * type_exp

    and type1_atom =
	TYPE1_ATOM of CharInfo.info * type0_atom * type1_atom'

    and type1_atom' =
	EPS1typ
      | ARROWtyp of CharInfo.info * type_exp
  
    and type_exp =
        TYPE_EXP of CharInfo.info * type1_atom

    and a0_atom =
	ID of CharInfo.info * identifier
      | BLOCKexp of CharInfo.info * block
      | NUM of CharInfo.info * numeral
      | FAL of CharInfo.info * falseval
      | NEG of CharInfo.info * a0_atom
      | NOT of CharInfo.info * a0_atom
      | PAR of CharInfo.info * expression 

    and a1_atom =
	A1_ATOM of CharInfo.info * a0_atom * a1_atom'

    and a1_atom' =
	EPS1
      | APP of CharInfo.info * a0_atom * a1_atom'
 
    and a2_atom =
	A2_ATOM of CharInfo.info * a1_atom * a2_atom'

    and a2_atom' =
	EPS2
      | DIV of CharInfo.info * a1_atom * a2_atom'
      | MOD of CharInfo.info * a1_atom * a2_atom'
      | TIMES of CharInfo.info * a1_atom * a2_atom'

    and a3_atom =
	A3_ATOM of CharInfo.info * a2_atom * a3_atom'

    and a3_atom' =
	EPS3 
      | PLUS of CharInfo.info * a2_atom * a3_atom'
      | MINUS of CharInfo.info * a2_atom * a3_atom'

    and a4_atom =
	A4_ATOM of CharInfo.info * a3_atom * a4_atom'

    and a4_atom' =
 	EPS4 
      | EQUAL of CharInfo.info * a3_atom * a4_atom'
      | DIFF of CharInfo.info * a3_atom * a4_atom'
      | LESS of CharInfo.info * a3_atom * a4_atom'
      | GREAT of CharInfo.info * a3_atom * a4_atom'
      | EQLESS of CharInfo.info * a3_atom * a4_atom'
      | EQGREAT of CharInfo.info * a3_atom * a4_atom'

    and a5_atom =
	A5_ATOM of CharInfo.info * a4_atom * a5_atom'

    and a5_atom' =
 	EPS5 
      | AND of CharInfo.info * a4_atom * a5_atom'
      | OR of CharInfo.info * a4_atom * a5_atom'

    and expression =
	EXPRESSION of CharInfo.info * a5_atom

    and identifier = IDENTIFIER of CharInfo.info * Identifier.id

    and numeral = NUMERAL of CharInfo.info * Numeral.num

    and falseval = FALSEVAL of CharInfo.info * Falseval.fal

end
