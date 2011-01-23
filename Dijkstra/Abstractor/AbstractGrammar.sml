(*******************)
(* AbstractGrammar *)
(*******************)


(*

The concrete syntax may look like this:

<program>
  -> <block>

<command>
  -> skip
   | abort
   | <command> ; <command>
   | <identifier> :vir= <expression>
   | <identifier> := <expression>
   | if <guard> fi
   | do <guard> od
   | <block>

<guard>
  -> <expression> -> <command>
   | <guard> [] <guard>

<block>
  -> begin <declaration> <command> end

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
 
<type_exp>
  -> int
   | bool
   | <type_exp> -> <type_exp>

<expression>
  -> <identifier>
   | <block>
   | <numeral>
   | <falseval>
   | <expression> . <expression>
   | ~ 
   | not 
   | div
   | mod
   | *
   | +
   | -
   | =
   | <>
   | <
   | >
   | <=
   | >=
   | and
   | or

<identifier>
  -> blabla | bleble  ...

<numeral>
  -> 1 | 2 ...

<falseval>
  -> true
   | false

*)

functor AbstractGrammar

    ( structure CharInfo' : sig eqtype info end

      structure Identifier' : sig type id end

      structure Numeral' : sig type num end

      structure Falseval' : sig type fal end ) : sig

	                                             include ABSTRACT_GRAMMAR

                                                     sharing CharInfo = CharInfo'

						     sharing Identifier = Identifier'

						     sharing Numeral = Numeral'

						     sharing Falseval = Falseval'

						 end =

struct

(*

In the absence of recursive modules
I have to put all this into a single structure :-(

*)

    structure CharInfo = CharInfo'

    structure Identifier = Identifier'

    structure Numeral = Numeral'

    structure Falseval = Falseval'

    datatype program =
	BLOCKprog of CharInfo.info * block

    and command =
	SKIP of CharInfo.info
      | ABORT of CharInfo.info
      | FOLLOW of CharInfo.info * command * command
      | VIR of CharInfo.info * identifier * expression
      | ASSIGN of CharInfo.info * identifier * expression
      | IFFI of CharInfo.info * guard
      | DOOD of CharInfo.info * guard
      | BLOCK of CharInfo.info * block

    and guard =
	ARROW of CharInfo.info * expression * command
      | TACT of CharInfo.info * guard * guard

    and block =
	BEGIN of CharInfo.info * declaration * command

    and declaration =
	PRIVAR of CharInfo.info * identifier * type_exp
      | PRICON of CharInfo.info * identifier * type_exp
      | VIRVAR of CharInfo.info * identifier * type_or_not
      | VIRCON of CharInfo.info * identifier * type_or_not
      | GLOVAR of CharInfo.info * identifier * type_or_not
      | GLOCON of CharInfo.info * identifier * type_or_not
      | COMPOUND of CharInfo.info * declaration * declaration

    and type_or_not =
	EPSton
      | COLON of CharInfo.info * type_exp

    and type_exp =
        INT of CharInfo.info
      | BOOL of CharInfo.info
      | FUN of CharInfo.info * type_exp * type_exp

    and expression =
	ID of CharInfo.info * identifier
      | BLOCKexp of CharInfo.info * block
      | NUM of CharInfo.info * numeral
      | FAL of CharInfo.info * falseval
      | APP of CharInfo.info * expression * expression
      | NEG of CharInfo.info
      | NOT of CharInfo.info
      | DIV of CharInfo.info
      | MOD of CharInfo.info
      | TIMES of CharInfo.info
      | PLUS of CharInfo.info
      | MINUS of CharInfo.info
      | EQUAL of CharInfo.info
      | DIFF of CharInfo.info
      | LESS of CharInfo.info
      | GREAT of CharInfo.info
      | EQLESS of CharInfo.info
      | EQGREAT of CharInfo.info
      | AND of CharInfo.info
      | OR of CharInfo.info

    and identifier = IDENTIFIER of CharInfo.info * Identifier.id

    and numeral = NUMERAL of CharInfo.info * Numeral.num

    and falseval = FALSEVAL of CharInfo.info * Falseval.fal

end
