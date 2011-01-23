(* Copyright (C) 2003 Mikolaj Konarski
 *
 * This file is part of the Dule compiler.
 * The Dule compiler is released under the GNU General Public License (GPL).
 * Please see the file Dule-LICENSE for license information.
 *
 * $Id: main.ml,v 1.53 2006-02-23 21:04:25 mikon Exp $
 *) 

open Main_common

let process parse semantics filename =
  let _ = Error_rep.Location.input_name := filename in
  let ch = open_in filename in
  try
    let lexbuf = Lexing.from_channel ch in
    let parsed = 
      try parse Lexer.in_code lexbuf with
      | Parsing.Parse_error ->
	  let loc = 
	    Error_rep.Location.int2t 
	      (Lexing.lexeme_start lexbuf) (Lexing.lexeme_end lexbuf) 
	  in
	  let er = Error_rep.TextualError.makeOtherSyntax loc in
	  raise (Error_rep.TextualError.TextualError er)
    in
    let _ = Parsing.clear_parser() in
    let _ = close_in ch in
    let t = semantics parsed in
    t
  with
    x -> close_in ch; raise x

let anonymous filename = 
  flush stderr; 
  print_string ("Compiling " ^ filename); 
  flush stdout;
  let _ = update_prelude (process Parser.start) in
  process Parser.start elm filename

let corymous filename =
  flush stderr; 
  print_string ("Compiling core language file " ^ filename); 
  flush stdout;
  let prelude_m = update_prelude (process Parser.start) in
  flush stderr; print_string "."; flush stdout;
  process Parser.core (elc prelude_m) filename

let _ = main (speclist anonymous corymous) anonymous

