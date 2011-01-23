(***********)
(* sources *)
(***********)



 
(********************)
(* sources of Lexer *)
(********************)


(***********************************************************)
(*                                                         *)
	use "ver.EML/Lexer/IDENTIFIER.sml";
	use "ver.EML/Lexer/Identifier.sml";

	use "ver.EML/Lexer/NUMERAL.sml";
	use "ver.EML/Lexer/Numeral.sml";

	use "ver.EML/Lexer/FALSEVAL.sml";
	use "ver.EML/Lexer/Falseval.sml";

	use "ver.EML/Lexer/TOKENS.sml";
	use "ver.EML/Lexer/Tokens.sml";

	use "ver.EML/Lexer/MULTI_CHAR.sml";
	use "ver.EML/Lexer/MultiChar.sml";

	use "ver.EML/Lexer/CHAR_LIST.sml";
	use "ver.EML/Lexer/CharList.sml";

	use "ver.EML/Lexer/CHAR_INFO.sml";
	use "ver.EML/Lexer/CharInfo.sml";

	use "ver.EML/Lexer/INFO_AND_TOKEN_LIST.sml";
	use "ver.EML/Lexer/InfoAndTokenList.sml";

	use "ver.EML/Lexer/NUMBER.sml";
	use "ver.EML/Lexer/Number.sml";

	use "ver.EML/Lexer/COUNTER.sml";
	use "ver.EML/Lexer/Counter.sml";

	use "ver.EML/Lexer/WORD.sml";
	use "ver.EML/Lexer/Word.sml";

	use "ver.EML/Lexer/RECOGNIZER.sml";
	use "ver.EML/Lexer/Recognizer.sml";

	use "ver.EML/Lexer/CUTTER.sml";
	use "ver.EML/Lexer/Cutter.sml";

	use "ver.EML/Lexer/LEXER.sml";
	use "ver.EML/Lexer/Lexer.sml";
(*                                                         *)
(***********************************************************)


(*********************)
(* sources of Parser *)
(*********************)


	use "Parser/LL1_GRAMMAR.sml";
	use "Parser/LL1Grammar.sml";

	use "Parser/PARSER_ERROR.sml";
	use "Parser/ParserError.sml"; 

	use "Parser/OK_ERROR.sml";
	use "Parser/OkError.sml";

	use "Parser/PARSER.sml";
	use "Parser/Parser.sml";

 
(*************************)
(* sources of Abstractor *)
(*************************)


	use "Abstractor/ABSTRACT_GRAMMAR.sml";
	use "Abstractor/AbstractGrammar.sml";

	use "Abstractor/ABSTRACTOR.sml";
	use "Abstractor/Abstractor.sml";


(******************************)
(* sources of SemanticObjects *)
(******************************)


        use "SemanticObjects/INTEGER.sml";
	use "SemanticObjects/Integer.sml"; 
	
	use "SemanticObjects/BOOLEAN.sml";
	use "SemanticObjects/Boolean.sml";	

	use "SemanticObjects/POST.sml";
	use "SemanticObjects/Post.sml";

	use "SemanticObjects/STORABLE.sml";
	use "SemanticObjects/Storable.sml";

(***********************************************************)
(*                                                         *)
	use "ver.EML/SemanticObjects/LOCATION.sml";
(*                                                         *)
(***********************************************************)
	use "SemanticObjects/Location.sml";

	use "SemanticObjects/STORE.sml";
	use "SemanticObjects/Store.sml";

	use "SemanticObjects/TTYPE.sml";
	use "SemanticObjects/Ttype.sml";

	use "SemanticObjects/DENOTABLE.sml";
	use "SemanticObjects/Denotable.sml";

	use "SemanticObjects/NAME.sml";
	use "SemanticObjects/Name.sml";	

	use "SemanticObjects/ENVIRONMENT.sml";
	use "SemanticObjects/Environment.sml";
   
	use "SemanticObjects/NAME_SET.sml";
	use "SemanticObjects/NameSet.sml";
 
	use "SemanticObjects/ELABORATOR_ERROR.sml";
	use "SemanticObjects/ElaboratorError.sml";

(*************************)
(* sources of Elaborator *)
(*************************)


	use "Elaborator/ELAB_ID.sml";
	use "Elaborator/ElabId.sml";

	use "Elaborator/ELAB_RIGHT_ID.sml";
	use "Elaborator/ElabRightId.sml";

	use "Elaborator/ELAB_NUM.sml";
	use "Elaborator/ElabNum.sml";

	use "Elaborator/ELAB_FAL.sml";
	use "Elaborator/ElabFal.sml";

	use "Elaborator/ELAB_RIGHT_BL.sml";
	use "Elaborator/ElabRightBl.sml";

	use "Elaborator/ELAB_EXP.sml";
	use "Elaborator/ElabExp.sml";

	use "Elaborator/ELAB_TY.sml";
	use "Elaborator/ElabTy.sml";

	use "Elaborator/ELAB_TON.sml";
	use "Elaborator/ElabTon.sml";

	use "Elaborator/ELAB_DEC.sml";
	use "Elaborator/ElabDec.sml";

	use "Elaborator/ELAB_GUA.sml";
	use "Elaborator/ElabGua.sml";

	use "Elaborator/ELAB_BL.sml";
	use "Elaborator/ElabBl.sml";

	use "Elaborator/ELAB_COM.sml";
	use "Elaborator/ElabCom.sml";

	use "Elaborator/ELAB_PROG.sml";
	use "Elaborator/ElabProg.sml";


(*********************)
(* sources of Linker *)
(*********************)


	use "Linker/LINK_LEXER.sml";
	use "Linker/LinkLexer.sml";

	use "Linker/LINK_PARSER.sml";
	use "Linker/LinkParser.sml";

	use "Linker/LINK_ABSTRACTOR.sml";
	use "Linker/LinkAbstractor.sml";

	use "Linker/LINK_ELABORATOR.sml";
	use "Linker/LinkElaborator.sml";

	use "Linker/LINKER.sml";
	use "Linker/Linker.sml";


(*******************)
(* sources of Test *)
(*******************)


	use "Test/STRING2MULTI_CHAR.sml";
(***********************************************************)
(*                                                         *)
	use "ver.EML/Test/String2MultiChar.sml";

	use "ver.EML/Test/Test.sml";
(*                                                         *)
(***********************************************************)


