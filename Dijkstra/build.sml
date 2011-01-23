(***********)
(* sources *)
(***********)



 
(********************)
(* sources of Lexer *)
(********************)


	use "Lexer/IDENTIFIER.sml";
	use "Lexer/Identifier.sml";

	use "Lexer/NUMERAL.sml";
	use "Lexer/Numeral.sml";

	use "Lexer/FALSEVAL.sml";
	use "Lexer/Falseval.sml";

	use "Lexer/TOKENS.sml";
	use "Lexer/Tokens.sml";

	use "Lexer/MULTI_CHAR.sml";
	use "Lexer/MultiChar.sml";

	use "Lexer/CHAR_LIST.sml";
	use "Lexer/CharList.sml";

	use "Lexer/CHAR_INFO.sml";
	use "Lexer/CharInfo.sml";

	use "Lexer/INFO_AND_TOKEN_LIST.sml";
	use "Lexer/InfoAndTokenList.sml";

	use "Lexer/NUMBER.sml";
	use "Lexer/Number.sml";

	use "Lexer/COUNTER.sml";
	use "Lexer/Counter.sml";

	use "Lexer/WORD.sml";
	use "Lexer/Word.sml";

	use "Lexer/RECOGNIZER.sml";
	use "Lexer/Recognizer.sml";

	use "Lexer/CUTTER.sml";
	use "Lexer/Cutter.sml";

	use "Lexer/LEXER.sml";
	use "Lexer/Lexer.sml";


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

	use "SemanticObjects/LOCATION.sml";
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
	use "Test/String2MultiChar.sml";

	use "Test/Test.sml";
