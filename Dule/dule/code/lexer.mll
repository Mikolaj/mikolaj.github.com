(* Copyright (C) 2003 Mikolaj Konarski
 *
 * based on lexer of Objective Caml http://caml.inria.fr/
 *
 * This file is part of the Dule compiler.
 * The Dule compiler is released under the GNU General Public License (GPL).
 * Please see the file Dule-LICENSE for license information.
 *
 * $Id: lexer.mll,v 1.18 2004/05/21 23:15:41 mikon Exp $
 *)

{
open Parser
open Error_rep

let comment_stack = Stack.create ()

(* the keywords and their names mostly from OCaml lexer, some unused, yet,
   written by me by hand, not to violate the OCaml copyright and licence: *)
let keywords =
  ["and", AND;
   "assert", ASSERT;
   "coind", COIND;
   "con", CON;
   "de", DE;
   "else", ELSE;
   "end", END;
   "exception", EXCEPTION;
   "fail", FAIL;
   "fold", FOLD;
   "fun", FUN;
   "if", IF;
   "in", IN;
   "include", INCLUDE;
   "ind", IND;
   "let", LET;
   "library", LIBRARY;
   "link", LINK;
   "load", LOAD;
   "map", MAP;
   "match", MATCH;
   "module", MODULE;
   "of", OF;
   "open", OPEN;
   "rec", REC;
   "sig", SIG;
   "spec", SPEC;
   "struct", STRUCT;
   "then", THEN;
   "type", TYPE;
   "uncon", UNCON;
   "unde", UNDE;
   "unfold", UNFOLD;
   "value", VALUE;
   "when", WHEN;
   "with", WITH]

let keyword_table =
  let table = Hashtbl.create 99 in
  let _ =
    List.iter (fun (keyword, token) ->
      Hashtbl.add table keyword token) keywords
  in 
  table

}

(* these definitions are taken verbatim from the OCaml lexer
   (the only other such two cases are below and in parser.mly) 
   with the oral permission obtained on ETAPS 2003 from Xavier Leroy 
   to take small snippets, if necessary (sigh, those non-free licences): *)
let blank = [' ' '\010' '\013' '\009' '\012']
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar = 
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let decimal_literal = ['0'-'9']+

(* simplified and changed rules of the OCaml lexer, 
   rewritten by hand not to violate the OCaml copyright and licence: *)
rule in_code = parse
|blank +
    { in_code lexbuf }
|"_" 
    { UNDERSCORE }
|lowercase identchar * 
    { let s = Lexing.lexeme lexbuf in
      try
	Hashtbl.find keyword_table s
      with Not_found ->
	LIDENT s }
|uppercase identchar *
    { UIDENT(Lexing.lexeme lexbuf) } (* no capitalized kewords *)
|decimal_literal
    { INT (int_of_string(Lexing.lexeme lexbuf)) }
|"(*"
    { assert (Stack.is_empty comment_stack);
      let pos = Lexing.lexeme_start lexbuf in
      Stack.push pos comment_stack;
      in_comment lexbuf;
      in_code lexbuf }
(* the following list of matches again taken verbatim from the OCaml lexer: *)
  | "~"  { TILDE }
  | "?"  { QUESTION }
  | "#"  { SHARP }
  | "&"  { AMPERSAND }
  | "&&" { AMPERAMPER }
  | "`"  { BACKQUOTE }
  | "'"  { QUOTE }
  | "("  { LPAREN }
  | ")"  { RPAREN }
  | "*"  { STAR }
  | ","  { COMMA }
  | "->" { MINUSGREATER }
  | "."  { DOT }
  | ".." { DOTDOT }
  | ":"  { COLON }
  | "::" { COLONCOLON }
  | ":>" { COLONGREATER }
  | ";"  { SEMI }
  | ";;" { SEMISEMI }
  | "<"  { LESS }
  | "="  { EQUAL }
  | "["  { LBRACKET }
  | "[|" { LBRACKETBAR }
  | "[<" { LBRACKETLESS }
  | "]"  { RBRACKET }
  | "{"  { LBRACE }
  | "{<" { LBRACELESS }
  | "|"  { BAR }
  | "||" { BARBAR }
  | "|]" { BARRBRACKET }
  | ">"  { GREATER }
  | ">]" { GREATERRBRACKET }
  | "}"  { RBRACE }
  | ">}" { GREATERRBRACE }
  | "+"  { PLUS }
  | "-"  { MINUS }
  | "-." { MINUSDOT }
  | eof { EOF }
|_
    { let illegal_char = (Lexing.lexeme lexbuf).[0] in
      raise (TextualError.TextualError(TextualError.makeIllegalCharacter 
					 illegal_char
					 (Lexing.lexeme_start lexbuf)
					 (Lexing.lexeme_end lexbuf))) }


(* the rules for comments, mostly from the OCaml lexer,
   rewritten by hand not to violate the OCaml copyright and licence: *)
and in_comment = parse
|"(*"
    { let pos = Lexing.lexeme_start lexbuf in
      Stack.push pos comment_stack;
      in_comment lexbuf }
|"*)"
    { assert (not (Stack.is_empty comment_stack));
      ignore (Stack.pop comment_stack);
      if Stack.is_empty comment_stack then ()
      else in_comment lexbuf }
|eof
    { let start_pos = Stack.top comment_stack in
      raise (TextualError.TextualError
	       (TextualError.makeRunoffComment
		  start_pos (start_pos + 2))) }
|_
    { in_comment lexbuf }
